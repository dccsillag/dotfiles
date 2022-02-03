-- __ __          __  __ __  __                           _
-- \ \\ \  _____  \ \/ /|  \/  |  ___   _ __    __ _   __| |
--  \ \\ \|_____|  \  / | |\/| | / _ \ | '_ \  / _` | / _` |
--  / // /|_____|  /  \ | |  | || (_) || | | || (_| || (_| |
-- /_//_/         /_/\_\|_|  |_| \___/ |_| |_| \__,_| \__,_|
--
--  @what My XMonad configuration.
--  @author Daniel Csillag (aka. dccsillag)
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Imports.

import Control.Monad (unless)
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString)
import System.Directory (doesFileExist)
import XMonad hiding (config, (|||))
import XMonad.Actions.Navigation2D
import XMonad.Actions.ShowText
import XMonad.Csillag.Commands
import XMonad.Csillag.Consts
import XMonad.Csillag.Externals
import XMonad.Csillag.Keys
import XMonad.Csillag.Layouts
import XMonad.Csillag.Scratchpads
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat)
import XMonad.Hooks.Rescreen
import XMonad.Hooks.ServerMode (serverModeEventHookF)
import XMonad.Util.Cursor
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

-- Things to do upon startup:
startup = do
  -- Start the compositor
  spawn compositorSpawn
  -- spawnOnce "kdeconnect-cli -l"
  spawnOnce "redshift"
  spawnOnce "/home/daniel/.local/scripts/zoom-autodevour.sh"
  spawnOnce "/home/daniel/.local/scripts/browser-screensharing-autohide.sh"
  spawnOnce "eww open bar-window"

  spawnOnce "/home/daniel/.xmonad/on_start.sh"

  -- Set the default cursor to the left arrow/pointer (not the X).
  setDefaultCursor xC_left_ptr

rescreenConfig :: RescreenConfig
rescreenConfig =
  def
    { afterRescreenHook = do
        -- This runs after display configuration is changed (e.g., via xrandr)
        spawn "eww reload" -- reload the statusbar
        spawn "background-setter set", -- set a new desktop background
      randrChangeHook = do
        -- This runs after physical display changes, such as a display disconnect
        spawn "xlayoutdisplay" -- automatically setup screens via xrandr
    }

myXMonadConfig = do
  tempfileExists <- doesFileExist workspaceTempFile
  unless tempfileExists $ writeFile workspaceTempFile "."
  wkss <- lines <$> readFile workspaceTempFile
  return $
    ewmh $
      Hacks.javaHack $
        withNavigation2DConfig myNavigation2DConfig $
          addDescrKeys' ((mod4Mask, xK_F1), \x -> writeFile "/tmp/xmonad-help.txt" (unlines $ showKm x) >> spawn (termRun "less /tmp/xmonad-help.txt")) myKeys $
            rescreenHook rescreenConfig $
              def
                { terminal = termSpawn,
                  modMask = mod4Mask, -- Super key
                  focusFollowsMouse = False,
                  normalBorderColor = "#555555", -- "#cccccc"
                  focusedBorderColor = "#FFFFFF",
                  borderWidth = 1,
                  workspaces = wkss,
                  manageHook =
                    insertPosition Below Newer
                      <+> namedScratchpadManageHook myScratchpads -- Manage scratchpads
                      <+> manageDocks -- ???
                      <+> composeAll
                        [ className =? "feh" --> doFloat, -- Float `feh` windows
                          className =? "Sxiv" --> doFloat, -- Float `sxiv` windows
                          title =? "KDE Connect Daemon" --> doFullFloat, -- Full float KDEConnect pointer
                          className =? "Florence" --> doFloat -- Float `florence` windows
                        ]
                      <+> manageHook def, -- The default
                  layoutHook = avoidStruts myLayouts, -- Respect struts (mainly for `polybar`/`xmobar` and `onboard`
                  handleEventHook =
                    serverModeEventHookF "XMONAD_COMMAND" (flip whenJust commandHandler . decode . fromString)
                      <+> handleTimerEvent
                      <+> docksEventHook
                      <+> Hacks.windowedFullscreenFixEventHook,
                  startupHook = startup, -- (on startup)
                  mouseBindings = myMouse
                }

main :: IO ()
main = xmonad =<< myXMonadConfig

myNavigation2DConfig = def {defaultTiledNavigation = sideNavigationWithBias 1}
