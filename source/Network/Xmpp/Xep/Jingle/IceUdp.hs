{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Xmpp.Xep.Jingle.IceUdp where

import           Control.Applicative ((<$>))
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Error
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.XML.Pickle
import           Data.XML.Types
import           Foreign.Ptr (nullPtr)
import           Network
import           Network.Address
import qualified Network.Ice as Ice
import           Network.Socket
import qualified Network.Xmpp as Xmpp
import           Network.Xmpp.Xep.Jingle
import           Network.Xmpp.Xep.Jingle.IceUdp.Types
import qualified Network.Xmpp.Xep.Jingle.Picklers as Jingle
import qualified Network.Xmpp.Xep.Jingle.Types as Jingle
import           System.Glib
import           System.Glib.MainLoop
import           System.Glib.Properties
import           System.Glib.Signals
import           System.Random

data Session = Session { sid   :: Text
                       , peer  :: Xmpp.Jid
                       , agent :: Ice.NiceAgent
                       , stream :: Int
                       }

data JingleIceError = JIUnpickleError UnpickleError

randChars n = Text.take n . Text.decodeUtf8 . B64.encode . BS8.pack <$>
                           replicateM n randomIO

sendSessionInitiate we to contentName sends desc sec session  cands lfrag passwd
    = do
        sidBytes <- liftIO $ randChars 10
        let iceUdp = IceUdp { pwd = passwd
                            , ufrag = lfrag
                            , candidates = Left cands
                            }
        let [NodeElement transportElement] = pickleTree xpTransport iceUdp
        let cont = Jingle.JingleContent { Jingle.creator = Jingle.CInitiator
                                        , Jingle.disposition = Nothing -- stream
                                        , Jingle.name = contentName
                                        , Jingle.senders = sends
                                        , Jingle.contentDescription = Just desc
                                        , Jingle.contentTransport =
                                                Just transportElement
                                        , Jingle.contentSecurity = sec
                                        }

        let ji = Jingle.Jingle { Jingle.action = Jingle.SessionInitiate
                               , Jingle.initiator = Just we
                               , Jingle.responder = Nothing
                               , Jingle.sid = sidBytes
                               , Jingle.reason = Nothing
                               , Jingle.content = [cont]
                               , Jingle.jinglePayload = []
                               }
        let [NodeElement el] = pickleTree Jingle.xpJingle ji
        answer <- liftIO $ Xmpp.sendIQ' (Just to) Xmpp.Set Nothing el session
        check answer
        return ()
  where
    check = undefined

type Handler = Xmpp.IQRequestTicket -> Jingle.Jingle -> Session -> IO ()

data ContentHandler = ContentHandler
   { handleContentAdd      :: Handler
   , handleContentAccept   :: Handler
   , handleContentModify   :: Handler
   , handleContentReject   :: Handler
   , handleContentRemove   :: Handler
   , handleDescriptionInfo :: Handler
   , handleSessionInfo     :: Handler
   }



-- sessionInitiate :: (String -> String -> [Candidate] -> IO a)
--                 -> IO (String, String, [Candidate])
--                 -> IO ()
startSession jh ch handleSecurityInfo sendInitialData getRemoteData = do
    glibTypeInit
    ctx <- mainContextNew
    ml <- mainLoopNew (Just ctx) False
    forkIO $ mainLoopRun ml
    agent <- Ice.niceAgentNew Ice.Rfc5245 ctx
    stream <- Ice.addStream agent 1
    Ice.attachReceive agent stream 0 ctx (\_ -> return ())
    Ice.gatherCandidates agent stream
    on agent Ice.candidateGatheringDone $ \_ -> do
        cs' <- Ice.getLocalCandidates agent stream 0
        let cs = zipWith (marshalCandidate stream) [1..] cs'
        (_, lufrag, lpwd) <- Ice.getLocalCredentials agent stream
        sendInitialData lufrag lpwd cs
        (ufrag, pwd, rcs) <- getRemoteData
        Ice.setRemoteCredentials agent stream ufrag pwd
        Ice.setRemoteCandidates agent stream 0 $ map (unmarshalCandidate stream )
                                                     rcs
        return ()
    let sess = Session { sid = undefined -- todo
                       , peer = undefined -- todo
                       , agent = agent
                       , stream = stream
                       }
    let handleIncoming ticket ji = case Jingle.action ji of
                Jingle.ContentAdd       -> handleContentAdd      ch ticket ji sess
                Jingle.ContentAccept    -> handleContentAccept   ch ticket ji sess
                Jingle.ContentModify    -> handleContentModify   ch ticket ji sess
                Jingle.ContentReject    -> handleContentReject   ch ticket ji sess
                Jingle.ContentRemove    -> handleContentRemove   ch ticket ji sess
                Jingle.DescriptionInfo  -> handleDescriptionInfo ch ticket ji sess
                Jingle.SessionInfo      -> handleSessionInfo     ch ticket ji sess
                Jingle.SecurityInfo     -> handleSecurityInfo     ticket ji
                Jingle.SessionAccept    -> handleSessionAccept    ticket ji sess
                Jingle.SessionInitiate  -> errorBadRequest        ticket
                Jingle.SessionTerminate -> handleSessionTerminate ticket sess
                Jingle.TransportAccept  -> handleTransportAccept  ticket
                Jingle.TransportInfo    -> handleTransportInfo    ticket ji sess
                Jingle.TransportReject  -> handleTransportReject  ticket sess
                Jingle.TransportReplace -> handleTransportReplace ticket sess
    return handleIncoming
  where
    handleSessionAccept ticket ji sess = do
        case Jingle.content ji of
            [Jingle.JingleContent { Jingle.contentDescription = Nothing
                                  , Jingle.contentSecurity = Nothing
                                  , Jingle.contentTransport = Just te }] ->
                do
                    case unpickle xpTransport [NodeElement te] of
                        Left _ -> void $ errorBadRequest ticket
                        Right IceUdp{candidates = Left [c]} -> do
                            Ice.setRemoteCandidates (agent sess) (stream sess) 0
                                                    [unmarshalCandidate 0 c]
                            Xmpp.answerIQ ticket (Right Nothing)
                            return ()
            _ -> void $ errorBadRequest ticket
    handleSessionTerminate ticket sess = do
        Xmpp.answerIQ ticket (Right Nothing)
        endSession (sid sess) jh
    handleTransportAccept ticket  = errorBadRequest ticket -- Assuming that we
                                                           -- never send
                                                           -- transport-replace.
    handleTransportInfo    ticket ji sess = do
        case Jingle.content ji of
            [Jingle.JingleContent {Jingle.contentTransport = Just te }] ->
                do
                    case unpickle xpTransport [NodeElement te] of
                        Left _ -> void $ errorBadRequest ticket
                        Right IceUdp{candidates = Left cs} -> do
                            handleContentAccept ch ticket ji sess --Is this right?
                            Ice.setRemoteCandidates (agent sess) (stream sess) 0
                                                    (map (unmarshalCandidate 0) cs)
                            Xmpp.answerIQ ticket (Right Nothing)
                            return ()
            _ -> void $ errorBadRequest ticket
    handleTransportReject ticket session = do
        Xmpp.answerIQ ticket (Right Nothing)
        terminateSession (sid session) jh
                         Jingle.JingleReason { Jingle.reasonType = Jingle.FailedTransport
                                             , Jingle.reasonText = Nothing
                                             , Jingle.reasonElement = Nothing
                                             }
    handleTransportReplace ticket sess = do
        Xmpp.answerIQ ticket (Right Nothing)
        let remote = Xmpp.iqRequestFrom $ Xmpp.iqRequestBody ticket
        case remote of
            Nothing -> return ()
            Just r -> transportReject (sid sess) r (Jingle.jingleXmppSession jh)
    port (SockAddrInet p _) = p
    port (SockAddrInet6 p _ _ _) = p
    port _ = error "port on IPC socket"

marshalCandidate :: Show a => t -> a -> Ice.NiceCandidate -> Candidate
marshalCandidate stream number nc =
    Candidate { cComponent = fromIntegral $ Ice.componentId nc
              , cFoundation = Text.pack $ Ice.foundation nc
              , cGeneration = 0
              , cAddress = Ice.address nc
              , cNetwork = 0
              , cPriority = Ice.priority nc
              , cProtocol = "udp"
              , cRelAddr = case Ice.candidateType nc of
                  Ice.ServerReflexive -> Ice.baseAddress nc
                  Ice.PeerReflexive   -> Ice.baseAddress nc
                  Ice.Relayed         -> Nothing -- TODO
                  Ice.Host            -> Nothing
              , cType = case Ice.candidateType nc of
                  Ice.Host            -> Host
                  Ice.ServerReflexive -> Srflx
                  Ice.PeerReflexive   -> Prflx
                  Ice.Relayed         -> Relay
              , cId = Text.pack $ show number
              }

unmarshalCandidate :: Int -> Candidate -> Ice.NiceCandidate
unmarshalCandidate stream nc =
    Ice.NiceCandidate { Ice.candidateType = case cType nc of
                             Host -> Ice.Host
                             Srflx -> Ice.ServerReflexive
                             Prflx -> Ice.PeerReflexive
                             Relay -> Ice.Relayed
                      , Ice.candidateTransport = Ice.CandidateTransportUdp
                      , Ice.address = cAddress nc
                      , Ice.baseAddress = Nothing
                      , Ice.priority = cPriority nc
                      , Ice.streamId = stream
                      , Ice.componentId = fromIntegral $ cComponent nc
                      , Ice.foundation = Text.unpack $ cFoundation nc
                      , Ice.username = Nothing
                      , Ice.password = Nothing
                      , Ice.turn = nullPtr
                      , Ice.sockPtr = nullPtr
                      }

iceUdpNs :: Text
iceUdpNs = "urn:xmpp:jingle:transports:ice-udp:1"

iceUdpName :: Text -> Name
iceUdpName n = Name n (Just iceUdpNs) Nothing

xpTransport :: PU [Node] IceUdp
xpTransport = xpWrap (\((u, p), c) -> IceUdp u p c)
                     (\(IceUdp u p c) -> ((u, p), c)) $
                xpElem (iceUdpName "transport")
                    (xp2Tuple
                         (xpAttrImplied "ufrag" xpText)
                         (xpAttrImplied "pwd" xpText))
                    (xpEither
                         xpCandidates
                         xpRemoteCandidate)

xpCandidates :: PU [Node] [Candidate]
xpCandidates = xpWrap (map from) (map to) $
    xpElems (iceUdpName "candidate")
                 (xp2Tuple
                    (xp6Tuple
                        (xpAttribute "component"  xpPrim)
                        (xpAttribute "foundation" xpText)
                        (xpAttribute "generation" xpPrim)
                        (xpAttribute "id"         xpPrim)
                        (xpAttribute "network"    xpPrim)
                        xpAddress)
                    (xp4Tuple
                        (xpAttribute "priority"   xpPrim)
                        (xpAttribute "protocol"   xpText)
                        xpRelAddress
                        (xpAttribute "type"       xpPrim)))
                  xpUnit
  where
    from (((a, b, c, d, e, f), (g, h, i, j)), ()) =
        Candidate a b c d e f g h i j
    to (Candidate a b c d e f g h i j) =
        (((a, b, c, d, e, f), (g, h, i, j)), ())

xpAddress = xpWrap (\(port, ip) -> setPort (fromIntegral port) ip)
                   (\addr -> (fromIntegral $ getPort addr, setPort 0 addr)) $
            xp2Tuple
            ( xpAttribute "port" xpPrim)
            ( xpAttribute "ip" xpIP)

xpRelAddress = xpWrap (\(port, ip) -> liftM2 setPort
                                               (fromIntegral `fmap` port) ip)
                   (maybe (Nothing, Nothing)
                      (\addr -> (Just . fromIntegral $ getPort addr
                                , Just $ setPort 0 addr))) $
            xp2Tuple
            ( xpAttribute' "rel-addr" xpPrim)
            ( xpAttribute' "rel-port" xpIP)

xpIP = xpWrapMaybe (readIP . Text.unpack) (Text.pack . showIP) xpId

xpRemoteCandidate :: PU [Node] RemoteCandidate
xpRemoteCandidate = xpWrap (\(comp, i, p) -> RemoteCandidate comp i p)
                           (\(RemoteCandidate comp i p) -> (comp, i, p)) $
                           xpElemAttrs (iceUdpName "candidate")
                           (xp3Tuple
                               (xpAttribute "component" xpPrim)
                               (xpAttribute "ip" xpText)
                               (xpAttribute "port" xpPrim))

getPort (SockAddrInet p _) = p
getPort (SockAddrInet6 p _ _ _) = p
getPort _ = error "getPort: not an IP address"

setPort p (SockAddrInet _ ha) = SockAddrInet p ha
setPort p (SockAddrInet6 _ fi ha sid) = SockAddrInet6 p fi ha sid
setPort p _ = error "getPort: not an IP address"

transportReject sid remote sess = let
    ji = Jingle.Jingle { Jingle.action = Jingle.TransportReject
                       , Jingle.initiator = Nothing
                       , Jingle.responder = Nothing
                       , Jingle.sid = sid
                       , Jingle.reason = Nothing
                       , Jingle.content = []
                       , Jingle.jinglePayload = []
                       }
    [NodeElement jiElement] = pickle Jingle.xpJingle ji
    in void $ Xmpp.sendIQ Nothing (Just remote) Xmpp.Set Nothing jiElement sess
