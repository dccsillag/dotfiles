{-# LANGUAGE BlockArguments #-}

import Data.Aeson
import System.Environment (getArgs)
import XMonad
import Data.ByteString.Lazy.UTF8 (toString)

main = sendCommand "XMONAD_COMMAND" =<< getArgs

sendCommand addr command = sendCommand' addr $ toString $ encode command

sendCommand' addr command = do
  d <- openDisplay ""
  rw <- rootWindow d $ defaultScreen d
  a <- internAtom d addr False
  m <- internAtom d command False
  allocaXEvent \e -> do
    setEventType e clientMessage
    setClientMessageEvent e rw a 32 m 0
    sendEvent d rw False structureNotifyMask e
    sync d False
