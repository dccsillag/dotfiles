module XMonad.Csillag.Scratchpads where

import Data.List

import XMonad

import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

import XMonad.Csillag.Externals


-- Scratchpads:
myScratchpads =
  [ NS { name  = "sysmon"
       , cmd   = term_run' "sysmon" system_monitor
       , query = className =? "sysmon"
       , hook  = floatingScratchpad
       }
  , NS { name  = "whatsapp"
       , cmd   = "whatsapp-for-linux"
       , query = className =? "Whatsapp-for-linux"
       , hook  = floatingScratchpad
       }
  , NS { name  = "bluetooth"
       , cmd   = "blueman-manager"
       , query = title =? "Bluetooth Devices"
       , hook  = floatingScratchpad
       }
  , NS { name  = "terminal"
       , cmd   = term_spawn' "scratchterm"
       , query = className =? "scratchterm"
       , hook  = floatingScratchpad
       }
  , NS { name  = "calculator"
       , cmd   = term_run' "calculator" "insect"
       , query = className =? "calculator"
       , hook  = floatingScratchpad
       }
  , NS { name  = "audio"
       , cmd   = term_run' "audiomanage" "pulsemixer"
       , query = className =? "audiomanage"
       , hook  = floatingScratchpad
       }
  , NS { name  = "deezer"
       , cmd   = browser_open "http://deezer.com"
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
       , cmd   = term_run' "email" "meli"
       , query = className =? "email"
       , hook  = floatingScratchpad
       }
  ]
  where floatingScratchpad = customFloating $ W.RationalRect 0.05 0.05 0.9 0.9
