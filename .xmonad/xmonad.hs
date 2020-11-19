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
import Control.Monad (forM_, when, unless)
import Control.Monad.Extra (whenM)
import qualified Data.Map as Map
import System.Directory (doesPathExist, doesFileExist)
import System.Environment
import Data.Bifunctor (first)
import System.Posix.Signals

-- XMonad imports
import XMonad hiding ((|||), config)
import XMonad.Actions.ShowText
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers (doFullFloat)
import XMonad.Util.Run
import XMonad.Hooks.SetWMName
import XMonad.Util.Cursor
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.Navigation2D
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.ManageDocks
import XMonad.Util.SpawnOnce
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.WindowSwallowing

-- My Configs
import XMonad.Csillag.Internal.KeyBindings
import XMonad.Csillag.Layouts
import XMonad.Csillag.Keys
import XMonad.Csillag.Scratchpads
import XMonad.Csillag.CommonActions
import XMonad.Csillag.Consts
import XMonad.Csillag.Externals
import XMonad.Csillag.Commands


-- Things to do upon startup:
startup = do
  -- Start the compositor
  spawn compositor_spawn
  -- spawnOnce "kdeconnect-cli -l"
  spawnOnce "redshift"
  spawnOnce "/home/daniel/.local/scripts/zoom-autodevour.sh"

  -- Set window manager name to LG3D, for compatibility with some finicky software
  setWMName "LG3D"
  -- Set the default cursor to the left arrow/pointer (not the X).
  setDefaultCursor xC_left_ptr

myXMonadConfig = do
    tempfileExists <- doesFileExist workspaceTempFile
    unless tempfileExists $ writeFile workspaceTempFile "."
    wkss <- lines <$> readFile workspaceTempFile
    return $ ewmh $ withNavigation2DConfig myNavigation2DConfig $ def {
          terminal           = term_spawn
        , modMask            = mod4Mask -- Super key
        , focusFollowsMouse  = False
        , normalBorderColor  = "#555555" -- "#cccccc"
        , focusedBorderColor = "#eeeeee"
        , borderWidth        = 1
        -- , workspaces         = map show $ init ([0 .. 10 {- 35 -}] :: [Int])
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
        , handleEventHook    = myFullscreenEventHook -- Automatically redraw windows when they become fullscreen
                               <+> docksEventHook -- ???
                               <+> handleTimerEvent
                               <+> myServerModeEventHook
                               <+> swallowEventHook (className =? "qutebrowser") (className =? "mpv") -- Swallow mpv from qutebrowser
        -- , logHook            = myWorkspaceNamesPP xmobarPP
        --                            { ppOutput = appendFile "/tmp/.xmonad-workspace-log" . (++ "\n") -- Pipe to write data for polybar
        --                            , ppTitle            = const "" -- Don't show the title
        --                            , ppLayout = wrapMonospace . polybarLabel "LAY" -- Show layout
        --                            , ppCurrent          = polybarLabel "WRK" -- Show current workspace
        --                            , ppHidden           = const "" -- Don't show hidden workspaces
        --                            , ppVisible          = const "" -- Don't show visible workspaces
        --                            , ppVisibleNoWindows = Nothing -- Don't show visible workspaces with no windows
        --                            , ppUrgent           = const "" -- Don't show "urgent" workspaces
        --                            , ppSep              = "   " -- Separator for the statusbar
        --                            } >>= dynamicLogWithPP
        , startupHook        = startup -- (on startup)
        , mouseBindings      = myMouseBindings
        , keys               = \x -> let myKeys' = filter (\case KeyHeading _ -> False; _ -> True) (myKeys x)
                                         systemFunctionKeys' = filter (\case KeyHeading _ -> False; _ -> True) (systemFunctionKeys x)
                                         in Map.insert (modMask x, xK_Escape) (modalmap (0, xK_i) $ Map.fromList $ keybindingToTuple 0 <$> myKeys') $
                                             Map.fromList $ (keybindingToTuple 0 <$> systemFunctionKeys') ++ (keybindingToTuple (modMask x) <$> myKeys')
        }


main :: IO ()
main = do
  -- Trap signals:
  installHandler sigUSR1 (Catch $ spawn "if xmonad --recompile; then xmonad --restart && notify-send -u low XMonad \"Restarted.\"; else notify-send -u critical XMonad \"Compilation failed.\"; fi") Nothing

  -- Set environment variables:
  -- let environment_vars_path = "/home/daniel/.xmonad/environment_variables.txt"
  let environment_vars_path = "/home/daniel/.config/environment.d/env.conf"
  whenM (doesPathExist environment_vars_path) $
    mapM_ (flip whenJust (uncurry setEnv) . splitOnEqual) . lines
      =<< readFile environment_vars_path

  -- xmproc <- spawnPipe "xmobar"
  -- _ <- spawnPipe "/home/daniel/.local/scripts/polybar_init.sh csillag"
  -- _ <- spawnPipe "/home/daniel/.local/scripts/blurwal_init.sh"
  spawn "/home/daniel/.local/scripts/xmobar_init.sh"

  -- -- Create pipes for communicating with Polybar
  -- forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file ->
  --     safeSpawn "mkfifo" ["/tmp/" ++ file]

  xmonad =<< myXMonadConfig


splitOnEqual :: String -> Maybe (String, String)
splitOnEqual ""        = Nothing
splitOnEqual ('=':str) = Just ("", str)
splitOnEqual (c:str)   = first (c:) <$> splitOnEqual str


myNavigation2DConfig = def { defaultTiledNavigation = sideNavigationWithBias 1 }
