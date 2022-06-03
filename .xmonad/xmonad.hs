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
{-# LANGUAGE BlockArguments #-}

-- Imports.

import Control.Monad (unless, when)
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString)
import System.Directory (doesFileExist)
import XMonad hiding (config, (|||))
import qualified XMonad.StackSet as W
import XMonad.Actions.Navigation2D
import XMonad.Actions.ShowText
import XMonad.Csillag.Commands
import XMonad.Csillag.Consts
import XMonad.Csillag.Externals
import XMonad.Csillag.Keys
import XMonad.Csillag.Layouts.TreeLayout
import XMonad.Csillag.Layouts.WindowCard
import XMonad.Csillag.Scratchpads
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat)
import XMonad.Hooks.Rescreen
import XMonad.Hooks.Place
import XMonad.Hooks.ServerMode (serverModeEventHookF)
import XMonad.Util.Cursor
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce
import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Layout.IfMax
import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Maximize
import XMonad.Layout.BoringWindows hiding (Replace)
import XMonad.Layout.NoBorders

-- Things to do upon startup:
startup = do
  -- Start the compositor
  spawn compositorSpawn
  -- spawnOnce "kdeconnect-cli -l"
  spawnOnce "redshift"
  spawnOnce "/home/daniel/.local/scripts/zoom-autodevour.sh"
  spawnOnce "/home/daniel/.local/scripts/browser-screensharing-autohide.sh"
  -- spawnOnce "eww open bar-window"
  spawnOnce "polybar csillag"

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
        spawn "/home/daniel/.local/scripts/rescreen.sh" -- automatically setup screens via xrandr
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
                  normalBorderColor = "#282C33", -- "#cccccc"
                  focusedBorderColor = "#DDDDDD",
                  borderWidth = 0,
                  workspaces = wkss,
                  manageHook =
                      namedScratchpadManageHook myScratchpads -- Manage scratchpads
                      <+> placeHook simpleSmart -- fix placement of floating windows
                      <+> manageDocks -- ???
                      <+> composeAll
                        [ className =? "feh" --> doFloat, -- Float `feh` windows
                          className =? "Sxiv" --> doFloat, -- Float `sxiv` windows
                          title =? "KDE Connect Daemon" --> doFullFloat, -- Full float KDEConnect pointer
                          className =? "Florence" --> doFloat, -- Float `florence` windows
                          className =? "Xmessage" --> doFloat -- Float `xmessage` windows
                        ]
                      <+> manageHook def, -- The default
                  layoutHook = avoidStruts $ boringWindows $ normalLayout ||| fullLayout,
                  handleEventHook =
                    serverModeEventHookF "XMONAD_COMMAND" (flip whenJust commandHandler . decode . fromString)
                      <+> handleTimerEvent
                      <+> docksEventHook
                      <+> Hacks.windowedFullscreenFixEventHook,
                  startupHook = startup, -- (on startup)
                  mouseBindings = myMouse
                }

normalLayout = windowCard windowCardConfig $ draggingVisualizer $ maximize $ spacing' 0 treeLayout
    where
        spacing' amount = spacingRaw False (Border gapsize' gapsize' (amount + gapsize') (amount + gapsize')) True (Border gapsize gapsize gapsize gapsize) True

        gapsize = 4
        gapsize' = 10

        windowCardConfig = WindowCardConfig
            { barSize = 24
            , buttonSize = 10
            , buttonSpacing = 10
            , barButtons =
                [ BarButton "#e6194B" CloseWindow
                , BarButton "#ffe119" CollapseWindow
                , BarButton "#3cb44b" MaximizeWindow
                ]
            , dragStartAction = PickWindow
            , dragEndAction = PlaceWindow
            }
fullLayout = noBorders Full

data ButtonActions = CloseWindow
                   | MaximizeWindow
                   | CollapseWindow
                   | PickWindow
                   | PlaceWindow
                   deriving (Show, Read)
instance ButtonAction ButtonActions where
    runAction CloseWindow w = killWindow w
    runAction MaximizeWindow w = focus w >> sendMessage (maximizeRestore w)
    runAction CollapseWindow w = focus w >> sendMessage (toggleCollapsed w)
    runAction PickWindow w = sendMessage $ pickOrPlace w
    runAction PlaceWindow w = do
        -- first, let's get the window under the cursor (if there is any)
        withDisplay \d -> do
            root <- asks theRoot
            (_, _, w', _, _, _, _, _) <- io $ queryPointer d root
            ws <- gets windowset
            let all_windows = W.index ws
            if (w `elem` all_windows) && (w' `elem` all_windows)
                then focus w' >> sendMessage (pickOrPlace w')
                else sendMessage (pickOrPlace w)

main :: IO ()
main = xmonad =<< myXMonadConfig

myNavigation2DConfig = def {defaultTiledNavigation = sideNavigationWithBias 1}
