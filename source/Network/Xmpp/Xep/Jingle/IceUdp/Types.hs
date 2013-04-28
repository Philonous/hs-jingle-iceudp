module Network.Xmpp.Xep.Jingle.IceUdp.Types where

import Data.Word
import Data.Text (Text)
import Network.Socket

-- <xs:enumeration value='host'/>
-- <xs:enumeration value='prflx'/>
-- <xs:enumeration value='relay'/>
-- <xs:enumeration value='srflx'/>
data CType = Host | Prflx | Relay | Srflx deriving (Eq)

instance Show CType where
    show Host  = "host"
    show Prflx = "prflx"
    show Relay = "relay"
    show Srflx = "srflx"

instance Read CType where
    readsPrec _ "host"  = [(Host  , "")]
    readsPrec _ "prflx" = [(Prflx , "")]
    readsPrec _ "relay" = [(Relay , "")]
    readsPrec _ "srflx" = [(Srflx , "")]
    readsPrec _ _       = []

data Candidate = Candidate
                 { cComponent  :: Word8
                 , cFoundation :: Text
                 , cGeneration :: Word8
                 , cId         :: Text
                 , cNetwork    :: Word8
                 , cAddress    :: SockAddr
                 , cPriority   :: Integer
                 , cProtocol   :: Text
                 , cRelAddr    :: Maybe SockAddr
                 , cType       :: CType
                 } deriving (Eq, Show)

data RemoteCandidate = RemoteCandidate
                       { rComponent :: Word8
                       , rIp        :: Text
                       , rPort      :: Word16
                       }

data IceUdp = IceUdp
              { ufrag :: Maybe Text
              , pwd :: Maybe Text
              , candidates :: Either [Candidate] RemoteCandidate
              }
