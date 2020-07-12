module XMonad.Csillag.Scratchpads where

import           Data.List

import           XMonad

import qualified XMonad.StackSet               as W
import           XMonad.Util.NamedScratchpad


-- Scratchpads:
myScratchpads =
  [ NS { name  = "sysmon"
       , cmd   = "alacritty --class sysmon,sysmon -e gotop"
       , query = className =? "sysmon"
       , hook  = floatingScratchpad
       }
  , NS { name  = "whatsapp"
       , cmd   = "qutebrowser --target window https://web.whatsapp.com/"
       , query = isSuffixOf "WhatsApp - qutebrowser" <$> title
       , hook  = floatingScratchpad
       }
  , NS { name  = "bluetooth"
       , cmd   = "blueman-manager"
       , query = title =? "Bluetooth Devices"
       , hook  = floatingScratchpad
       }
  , NS { name  = "terminal"
       , cmd   = "alacritty --class scratchterm,scratchterm"
       , query = className =? "scratchterm"
       , hook  = floatingScratchpad
       }
  , NS { name  = "audio"
       -- , cmd   = "pavucontrol"
       -- , query = title =? "Volume Control"
       , cmd   = "alacritty --class audiomanage,audiomanage -e pulsemixer"
       , query = className =? "audiomanage"
       -- , cmd   = "alacritty --class audiomanage,audiomanage -e alsamixer"
       -- , query = className =? "audiomanage"
       , hook  = floatingScratchpad
       }
  , NS { name  = "music"
       , cmd   = "alacritty --class musicplayer,musicplayer -e cmus"
       , query = className =? "musicplayer"
       , hook  = floatingScratchpad
       }
  , NS { name  = "deezer"
       , cmd   = "qutebrowser --target window http://deezer.com/"
       , query = isSuffixOf "Deezer - qutebrowser" <$> title
       , hook  = floatingScratchpad
       }
  , NS
    { name  = "todo"
    , cmd   = "alacritty --class todo,todo -e"
                ++ " sh -c \"nvim $(ls ~/todo/*.todo | tr '\\n' ' ')\""
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
       , cmd   = "alacritty --class calendar,calendar -e nvim -c Calendar"
       , query = className =? "calendar"
       , hook  = floatingScratchpad
       }
  ]
  where floatingScratchpad = customFloating $ W.RationalRect 0.05 0.05 0.9 0.9
