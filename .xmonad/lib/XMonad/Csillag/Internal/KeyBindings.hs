module XMonad.Csillag.Internal.KeyBindings where

import           XMonad
import XMonad.Actions.Submap
import qualified Data.Map as M


-- Structure for Keybindings

data HumanKey = ModKey
              | ShiftKey
              | ControlKey
              | AltKey
              | AlphaKey Char
              | FunctionKey Int
              | BrightnessDownKey
              | BrightnessUpKey
              | AudioRaiseKey
              | AudioLowerKey
              | AudioMuteKey
              | RightArrowKey
              | LeftArrowKey
              | ReturnKey
              | TabKey
              | SpaceKey
              | EscapeKey
              deriving (Eq, Read)

instance Show HumanKey where
  show ModKey            = "Mod"
  show ShiftKey          = "Shift"
  show ControlKey        = "Ctrl"
  show AltKey            = "Alt"
  show (AlphaKey    k)   = [k]
  show (FunctionKey n)   = 'F' : show n
  show BrightnessUpKey   = "\62942+" -- \62942  =  
  show BrightnessDownKey = "\62942-" -- \62942  =  
  show AudioRaiseKey     = "墳"
  show AudioLowerKey     = "奔"
  show AudioMuteKey      = "婢"
  show RightArrowKey     = "→"
  show LeftArrowKey      = "←"
  show ReturnKey         = "Return" -- \63504 = 
  show TabKey            = "Tab"
  show SpaceKey          = "Space"
  show EscapeKey         = "Esc"

data KeyBinding = KeyBinding { keybinding_description :: String
                             , keybinding_mask        :: KeyMask
                             , keybinding_key         :: KeySym
                             , keybinding_humankey    :: [HumanKey]
                             , keybinding_action      :: X ()
                             }
                | KeySubmap { keysubmap_description :: String
                            , keysubmap_mask        :: KeyMask
                            , keysubmap_key         :: KeySym
                            , keysubmap_humankey    :: [HumanKey]
                            , keysubmap_submaps     :: [KeyBinding]
                            }
                | KeyMode { keymode_name            :: String
                          , keymode_mask            :: KeyMask
                          , keymode_key             :: KeySym
                          , keymode_escape_mask     :: KeyMask
                          , keymode_escape_key      :: KeySym
                          , keymode_humankey        :: [HumanKey]
                          , keymode_escape_humankey :: [HumanKey]
                          , keymode_submaps         :: [KeyBinding]
                          }
                | KeyHeading String

keybindingToTuple :: KeyMask -> KeyBinding -> ((KeyMask, KeySym), X ())
keybindingToTuple m (KeyBinding _ mask key _ action)  = ((mask .|. m, key), action)
keybindingToTuple m (KeySubmap  _ mask key _ submaps) =
    ((mask .|. m, key), submap . M.fromList $ keybindingToTuple m <$> submaps)
keybindingToTuple m (KeyMode _ mask key escMask escKey _ _ submaps)    =
    ((mask .|. m, key), modalmap (escMask, escKey) . M.fromList $ keybindingToTuple m <$> submaps)
keybindingToTuple _ (KeyHeading _                  )  = ((0, 0), return ())

modalmap :: (KeyMask, KeySym) -> M.Map (KeyMask, KeySym) (X ()) -> X ()
modalmap esc s = submapDefault (modalmap esc s) $ M.insert esc (return ()) $ M.map (>> modalmap esc s) s

altMask = mod1Mask
