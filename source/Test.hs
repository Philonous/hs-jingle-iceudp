{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Default
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import qualified Data.Text as Text
import           Data.XML.Pickle
import           Data.XML.Types
import           Network
import qualified Network.Xmpp as Xmpp
import           Network.Xmpp.IM
import qualified Network.Xmpp.Xep.Jingle as Jingle
import qualified Network.Xmpp.Xep.Jingle.Types as Jingle
import qualified Network.Xmpp.Xep.Jingle.IceUdp as Jingle
import qualified Network.Xmpp.Xep.Jingle.Picklers as Jingle
import           System.Log.Handler hiding (setLevel)
import           System.Log.Handler.Simple
import           System.Log.Logger
import           Text.Printf

realm    = "species64739.dyndns.org"
username1 = "echo1"
username2 = "echo2"
password = "pwd"
resource = Just "bot"
config = def{Xmpp.sessionStreamConfiguration
              = def{Xmpp.connectionDetails = Xmpp.UseHost "localhost" (PortNumber 5222)}}

-- | Automatically accept all subscription requests from other entities
autoAccept :: Xmpp.Session -> IO ()
autoAccept session = forever $ do
  st <- Xmpp.waitForPresence (\x -> Xmpp.presenceType x == Just Xmpp.Subscribe) session
  friend <- case Xmpp.presenceFrom st of
      Just from -> do
          Xmpp.sendPresence (Xmpp.presenceSubscribed from) session
          return $ show from
      Nothing -> return "anonymous" -- this shouldn't happen
  putStrLn $  "Accepted " ++ show friend


-- type HandlerFunc = Xmpp.Jid
--                    -> Jingle
--                    -> TVar State
--                    -> TChan Jingle
--                    -> JingleHandler
--                    -> IO (Maybe MessageHandler)

-- handleNewRequest
--   :: Xmpp.Jid
--      -> Jingle.ContentHandler
--      -> (Xmpp.IQRequestTicket -> Jingle.Jingle -> IO ())
--      -> Xmpp.Jid
--      -> Jingle.Jingle
--      -> t
--      -> t1
--      -> Jingle.JingleHandler
--      -> IO (Maybe Jingle.MessageHandler)
handleNewRequest we ch hsi remote ji st jis jh = do
    case Jingle.content ji of
        [jco] -> do
            let ch' = myContentHandler . fromJust $ Jingle.contentDescription jco
            let sendInitD = Jingle.sendSessionAccept Nothing ch' remote jh
            handler <- Jingle.startSession jh ch' hsi sendInitD (\_ -> return ())
                                           (Jingle.sid ji) remote
                                           (Jingle.creator jco) (Jingle.name jco)
                                           (fromJust $ Jingle.senders jco) we
            return $ Just handler
        _ -> do
            let term = Jingle.jingleTerminate Jingle.UnsupportedApplications
                                              (Jingle.sid ji)
            let [NodeElement termEl] = pickle Jingle.xpJingle term
            Xmpp.sendIQ Nothing (Just remote) Xmpp.Set Nothing termEl
                        (Jingle.jingleXmppSession jh)
            return Nothing


myContentHandler desc = Jingle.ContentHandler
    { Jingle.handleContentAdd      = br
    , Jingle.handleContentAccept   = answer
    , Jingle.handleContentModify   = br
    , Jingle.handleContentReject   = br
    , Jingle.handleContentRemove   = br
    , Jingle.handleDescriptionInfo = br
    , Jingle.handleSessionInfo     = br
    , Jingle.getContentDesc        = return desc
    }
  where
    br ticket _ _ = Jingle.errorBadRequest ticket
    answer ticket _ _ = Xmpp.answerIQ ticket (Right Nothing) >> return ()

newSession jh to xmppSession = do
    Just we <- Xmpp.getJid xmppSession
    let hsi ticket _ = Jingle.errorBadRequest ticket
    sid <- Jingle.randChars 8
    let registerSession hi = do
        debugM "Pontarius.Xmpp.Jingle" $ "Adding Session: " ++ Text.unpack sid
        sState <- newTVarIO Jingle.PENDING
        let jSession = Jingle.Session { Jingle.sState  = sState
                                      , Jingle.sSid    = sid
                                      , Jingle.sRemote = to
                                      , Jingle.sRequests = hi
                                      }
        Jingle.addSession jSession jh
        return ()
    handler <- Jingle.startSession jh (myContentHandler dummyE) hsi
                                   ( Jingle.sendSessionInitiate we to
                                         "test-session" (Just Jingle.SBoth)
                                         dummyE Nothing sid xmppSession
                                   )
                                   registerSession
                                   sid to Jingle.CInitiator "test-session"
                                   Jingle.SBoth we

    return ()

main' active uname = do
    updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
    updateGlobalLogger "Pontarius.Xmpp.Jingle" $ setLevel DEBUG
    sess' <- Xmpp.session realm
                     (Just ([Xmpp.scramSha1 uname Nothing password], resource))
                     config
    sess <- case sess' of
        Left err -> error $ "Error connection to XMPP server: " ++ show err
        Right sess -> return sess
    Xmpp.sendPresence Xmpp.presenceOnline sess
    putStrLn "Connected."
    -- We want to see all incoming stanzas in the auto-accept thread as well.
    sess' <- Xmpp.dupSession sess
    putStrLn "$$$"
    _thread <- forkIO $ autoAccept sess'
    Just we <- Xmpp.getJid sess
    -- let we = if active then "echo1@species64739.dyndns.org"
    --                    else "echo1@species64739.dyndns.org"
    let hsi ticket _ = Jingle.errorBadRequest ticket
        h   = handleNewRequest we myContentHandler hsi
    Just jh <- Jingle.startJingle h (\_ -> return True) sess
    debugM "Pontarius.Xmpp" $ "active: " ++ show active
    putStrLn "----------------"
    print =<< getRoster sess
    putStrLn "----------------"
    when active $ do
        Xmpp.sendMessage (simpleIM "echo2@species64739.dyndns.org" "bla") sess
        -- Xmpp.sendIQ Nothing (Just "echo2@species64739.dyndns.org/bot")
        --             Xmpp.Set Nothing dummyE  sess
        threadDelay 1000000
        newSession jh (read "echo2@species64739.dyndns.org/bot") sess
        return ()
    forever $ threadDelay 10000000
    return sess



main = do
    updateGlobalLogger "Pontarius.Xmpp" $ setLevel INFO
    updateGlobalLogger "Pontarius.Xmpp.Jingle" $ setLevel DEBUG
    -- handler <- streamHandler stderr DEBUG >>= \h ->
    --     return $ setFormatter h (simpleLogFormatter "$loggername: $msg")
    -- updateGlobalLogger "Pontarius.Xmpp" (addHandler handler)
    forkIO . void $ main' False "echo2"
    main' True "echo1"

main1 = main' True "echo1"
main2 = main' False "echo2"

dummyE = Element "{jingle:dummy}description" [] []
