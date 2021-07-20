-- __ __          __  __ __  __                           _
-- \ \\ \  _____  \ \/ /|  \/  |  ___   _ __    __ _   __| |
--  \ \\ \|_____|  \  / | |\/| | / _ \ | '_ \  / _` | / _` |
--  / // /|_____|  /  \ | |  | || (_) || | | || (_| || (_| |
-- /_//_/         /_/\_\|_|  |_| \___/ |_| |_| \__,_| \__,_|
--
--  @what My XMonad configuration.
--  @author Daniel Csillag (aka. dccsillag)

{-# LANGUAGE NoMonomorphismRestriction, NumericUnderscores, LambdaCase #-}

-- Imports.

-- Standard Haskell
import Control.Monad (unless)
import System.Directory (doesFileExist)

-- XMonad imports
import XMonad hiding ((|||), config)
import XMonad.Actions.ShowText
import XMonad.Hooks.ManageHelpers (doFullFloat)
import XMonad.Hooks.SetWMName
import XMonad.Util.Cursor
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.Navigation2D
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.ManageDocks
import XMonad.Util.SpawnOnce
import XMonad.Hooks.InsertPosition
import XMonad.Util.NamedActions

-- My Configs
import XMonad.Csillag.Layouts
import XMonad.Csillag.Keys
import XMonad.Csillag.Scratchpads
import XMonad.Csillag.Consts
import XMonad.Csillag.Externals


-- Things to do upon startup:
startup = do
  -- Start the compositor
  spawn compositorSpawn
  -- spawnOnce "kdeconnect-cli -l"
  spawnOnce "redshift"
  spawnOnce "/home/daniel/.local/scripts/zoom-autodevour.sh"
  spawnOnce "/home/daniel/.local/scripts/browser-screensharing-autohide.sh"
  spawnOnce "toggle_statusbar"

  -- Set window manager name to LG3D, for compatibility with some finicky software
  setWMName "LG3D"
  -- Set the default cursor to the left arrow/pointer (not the X).
  setDefaultCursor xC_left_ptr

myXMonadConfig = do
    tempfileExists <- doesFileExist workspaceTempFile
    unless tempfileExists $ writeFile workspaceTempFile "."
    wkss <- lines <$> readFile workspaceTempFile
    return
        $ ewmh
        $ withNavigation2DConfig myNavigation2DConfig
        $ addDescrKeys' ((mod4Mask .|. shiftMask, xK_slash), \x -> writeFile "/tmp/xmonad-help.txt" (unlines $ showKm x) >> spawn (termRun "less /tmp/xmonad-help.txt")) myKeys
        $ def {
          terminal           = termSpawn
        , modMask            = mod4Mask -- Super key
        , focusFollowsMouse  = False
        , normalBorderColor  = "#555555" -- "#cccccc"
        , focusedBorderColor = "#FFFFFF"
        , borderWidth        = 1
        , workspaces         = wkss
        , manageHook         = insertPosition Below Newer
                               <+> namedScratchpadManageHook myScratchpads -- Manage scratchpads
                               <+> manageDocks -- ???
                               <+> composeAll [ className =? "feh" --> doFloat -- Float `feh` windows
                                              , className =? "Sxiv" --> doFloat -- Float `sxiv` windows
                                              , title =? "KDE Connect Daemon" --> doFullFloat -- Full float KDEConnect pointer
                                              ]
                               <+> manageHook def -- The default
        , layoutHook         = avoidStruts myLayouts -- Respect struts (mainly for `polybar`/`xmobar` and `onboard`
        , handleEventHook    = docksEventHook -- ???
                               <+> handleTimerEvent
        , startupHook        = startup -- (on startup)
        , mouseBindings      = myMouseBindings
        }


main :: IO ()
main = xmonad =<< myXMonadConfig

myNavigation2DConfig = def { defaultTiledNavigation = sideNavigationWithBias 1 }
