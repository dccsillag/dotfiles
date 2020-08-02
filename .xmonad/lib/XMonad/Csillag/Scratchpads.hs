module XMonad.Csillag.Scratchpads where

import Data.List

import XMonad

import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

import XMonad.Csillag.Externals


-- Scratchpads:
myScratchpads =
  [ NS { name  = "sysmon"
       , cmd   = term_run' "sysmon" "gotop"
       , query = className =? "sysmon"
       , hook  = floatingScratchpad
       }
  , NS { name  = "whatsapp"
       , cmd   = browser_open "https://web.whatsapp.com/"
       , query = isSuffixOf "WhatsApp - qutebrowser" <$> title
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
  , NS { name  = "audio"
       -- , cmd   = "pavucontrol"
       -- , query = title =? "Volume Control"
       , cmd   = term_run' "audiomanage" "pulsemixer"
       , query = className =? "audiomanage"
       -- , cmd   = "alacritty --class audiomanage,audiomanage -e alsamixer"
       -- , query = className =? "audiomanage"
       , hook  = floatingScratchpad
       }
  , NS { name  = "advancedaudio"
       , cmd   = term_run' "advancedaudio" "alsamixer"
       , query = className =? "advancedaudio"
       , hook  = floatingScratchpad
       }
  , NS { name  = "music"
       , cmd   = term_run' "musicplayer" "cmus"
       , query = className =? "musicplayer"
       , hook  = floatingScratchpad
       }
  , NS { name  = "deezer"
       , cmd   = browser_open "http://deezer.com"
       , query = isSuffixOf "Deezer - qutebrowser" <$> title
       , hook  = floatingScratchpad
       }
  , NS
    { name  = "todo"
    -- , cmd   = "alacritty --class todo,todo -e"
    , cmd   = "st -c todo -e"
                ++ " sh -c \"nvim $(ls ~/todo/*.toq | tr '\\n' ' ')\""
    , query = className =? "todo"
    , hook  = floatingScratchpad
    }
  , NS
    { name  = "slack"
    , cmd   = "slack"
    , query =
      (\x ->
          "Slack | " `isPrefixOf` x && not (" | Slack call with " `isInfixOf` x)
        )
        <$> title
    , hook  = floatingScratchpad
    }
  , NS { name  = "discord"
       , cmd   = "discord"
       , query = className =? "discord"
       , hook  = floatingScratchpad
       }
  , NS { name  = "mail"
       -- , cmd   = "alacritty --class email,email -e aerc"
       , cmd   = "mailspring"
       , query = className =? "Mailspring"
       , hook  = floatingScratchpad
       }
  , NS { name  = "calendar"
       , cmd   = term_run' "calendar" "nvim -c Calendar"
       , query = className =? "calendar"
       , hook  = floatingScratchpad
       }
  ]
  where floatingScratchpad = customFloating $ W.RationalRect 0.05 0.05 0.9 0.9
