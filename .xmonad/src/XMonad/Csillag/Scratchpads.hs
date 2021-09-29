module XMonad.Csillag.Scratchpads where

import Data.List

import XMonad

import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

import XMonad.Csillag.Externals


-- Scratchpads:
myScratchpads =
  [ NS { name  = "sysmon"
       , cmd   = termRun' "sysmon" systemMonitor
       , query = className =? "sysmon"
       , hook  = floatingScratchpad
       }
  , NS { name  = "whatsapp"
       , cmd   = "brave --app=https://web.whatsapp.com/"
       , query = className =? "Brave-browser" <&&> appName =? "web.whatsapp.com"
       , hook  = floatingScratchpad
       }
  , NS { name  = "telegram"
       , cmd   = "telegram-desktop"
       , query = className =? "TelegramDesktop"
       , hook  = floatingScratchpad
       }
  , NS { name  = "terminal"
       , cmd   = termSpawn' "scratchterm"
       , query = className =? "scratchterm"
       , hook  = floatingScratchpad
       }
  , NS { name  = "calculator"
       , cmd   = termRun' "calculator" "insect"
       , query = className =? "calculator"
       , hook  = floatingScratchpad
       }
  , NS { name  = "audio"
       , cmd   = termRun' "audiomanage" "pulsemixer"
       , query = className =? "audiomanage"
       , hook  = floatingScratchpad
       }
  , NS { name  = "deezer"
       , cmd   = browserOpen "http://deezer.com"
       , query = isSuffixOf "Deezer - qutebrowser" <$> title
       , hook  = floatingScratchpad
       }
  , NS
    { name  = "slack"
    , cmd   = "slack -s"
    , query = className =? "Slack"
    , hook  = floatingScratchpad
    }
  , NS { name  = "discord"
       , cmd   = "discord"
       , query = className =? "discord"
       , hook  = floatingScratchpad
       }
  , NS { name  = "mail"
       , cmd   = termRun' "email" "aerc"
       , query = className =? "email"
       , hook  = floatingScratchpad
       }
  ]
  where floatingScratchpad = customFloating $ W.RationalRect 0.05 0.05 0.9 0.9
