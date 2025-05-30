{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module XMonad.Csillag.Keys
  ( myKeys,
    myMouse,
  )
where

import Control.Concurrent
import Control.Monad
import Data.Char (isDigit, toLower)
import Data.List (isPrefixOf, stripPrefix)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import System.Directory (listDirectory)
import System.Exit
import XMonad hiding (config, keys)
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.Navigation2D
import XMonad.Actions.TiledWindowDragging
import XMonad.Actions.Warp
import XMonad.Config.Prime (ScreenId)
import XMonad.Csillag.Consts
import XMonad.Csillag.Externals
import XMonad.Csillag.Scratchpads
import XMonad.Csillag.Layouts.TreeLayout
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutCombinators (JumpToLayout (..))
import XMonad.Layout.Maximize
import XMonad.Prompt (XPConfig, mkComplFunFromList')
import XMonad.Prompt.Input
import XMonad.Prompt.Pass
import XMonad.Prompt.Ssh
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.WindowProperties
import XMonad.Util.Dmenu

myKeys =
  flip
    mkNamedKeymap
    -- Core:
    [ ("M-r", addName "Restart XMonad" $ spawn "xmonad --restart"),
      ("M-S-9", addName "Kill the compositor" $ spawn compositorKill),
      ("M-S-0", addName "Start the compositor" $ spawn compositorSpawn),
      -- ("M-<Space>", addName "Change keyboard" changeKeyboard),
      -- Directional keys
      ("M-h", addName "Focus window to the left" $ windowGo L False),
      ("M-j", addName "Focus window below" $ windowGo D False),
      ("M-k", addName "Focus window above" $ windowGo U False),
      ("M-l", addName "Focus window to the right" $ windowGo R False),
      ("M-S-h", addName "Move window to the left" $ windowSwap L False),
      ("M-S-j", addName "Move window down" $ windowSwap D False),
      ("M-S-k", addName "Move window up" $ windowSwap U False),
      ("M-S-l", addName "Move window to the right" $ windowSwap R False),
      -- Stack Keys
      ("M-S-<Tab>", addName "Focus up on the stack" $ windows W.focusUp),
      ("M-<Tab>", addName "Focus down on the stack" $ windows W.focusDown),
      ("M-C-S-<Tab>", addName "Swap window up on the stack" $ windows W.swapUp),
      ("M-C-<Tab>", addName "Swap window down on the stack" $ windows W.swapDown),
      -- Spawn Stuff
      ("M-n M-n", addName "Open shell prompt" $ spawn "rofi -show drun -matching fuzzy -show-icons -markup"),
      ("M-n M-t", addName "Spawn a terminal" $ spawn termSpawn),
      ("M-n M-S-t", addName "Spawn a terminal with SSH" $ spawn "rofi -show ssh -matching fuzzy"),
      ("M-n M-f", addName "Spawn a file manager" $ spawn filemanagerSpawn),
      ("M-n M-v", addName "Spawn an editor" $ spawn texteditorSpawn),
      ("M-n M-b", addName "Spawn a browser" $ spawn browserSpawn),
      ("M-n M-S-b", addName "Spawn a private browser" $ spawn browserSpawnPrivate),
      ("M-n M-c", addName "Spawn a camera view" $ spawn camviewSpawn),
      ("M-n M-w", addName "Spawn a notebook" $ spawn "rnote"),
      ("M-n M-r", addName "Spawn a calculator" $ spawn calculatorSpawn),
      -- Floating Windows
      ("M-f M-t", addName "Tile floating window" $ withFocused $ windows . W.sink),
      ("M-f M-s", addName "Float window as a scratchpad" $ withFocused $ windows . flip W.float (W.RationalRect 0.05 0.05 0.9 0.9)),
      -- Close Windows
      ("M-x M-x", addName "Close window" kill1),
      ("M-x M-d", addName "Close program" kill),
      ("M-x M-c", addName "Close copy" killCopy),
      -- Screens (Xinerama)
      ("M-S-s M-S-a", addName "Focus on 1st screen" $ goToScreen 0),
      ("M-S-s M-S-q", addName "Focus on 2nd screen" $ goToScreen 1),
      ("M-S-s M-a", addName "Send window to 1st screen" $ sendToScreen 0),
      ("M-S-s M-q", addName "Send window to 2nd screen" $ sendToScreen 1),
      ("M-S-s M-S-s", addName "Swap screens" $ screenSwap R True),
      ("M-S-s M-S-c", addName "Change screen setup" changeScreenConfig),
      ("M-S-s M-S-o M-S-k", addName "Set screen orientation to 'up'" $ spawn $ setScreenOrientation "normal" 0),
      ("M-S-s M-S-o M-S-j", addName "Set screen orientation to 'down'" $ spawn $ setScreenOrientation "inverted" 0),
      ("M-S-s M-S-o M-S-h", addName "Set screen orientation to 'left'" $ spawn $ setScreenOrientation "left" 0),
      ("M-S-s M-S-o M-S-l", addName "Set screen orientation to 'right'" $ spawn $ setScreenOrientation "right" 0),
      -- Workspaces
      ("M-w M-r", addName "Rename workspace" $ rofi "Rename to" Nothing "new workspace name" [] ?+ renameWorkspaceByName),
      ("M-w M-d", addName "Delete workspace" $ removeEmptyWorkspaceAfter (windows \ws -> flip W.view ws $ W.tag $ head $ filter ((/= "NSP") . W.tag) $ W.hidden ws)),
      ("M-w M-g", addName "Go to workspace" $ promptWorkspaces "Go to" Nothing ?+ (\x -> addHiddenWorkspace x >> windows (W.view x))),
      ("M-w M-s", addName "Send to workspace" $ promptWorkspaces "Send to" Nothing ?+ (\x -> addHiddenWorkspace x >> windows (W.shift x))),
      ("M-w M-C-g", addName "Send&Go to workspace" $ promptWorkspaces "Send&Go to" Nothing ?+ (\x -> addHiddenWorkspace x >> windows (W.shift x) >> windows (W.view x))),
      ("M-w M-S-c", addName "Send copy to workspace" $ promptWorkspaces "Send copy to" Nothing ?+ (\x -> addHiddenWorkspace x >> windows (copy x))),
      ("M-w M-c", addName "Send&Go copy to workspace" $ promptWorkspaces "Send&Go copy to" Nothing ?+ (\x -> addHiddenWorkspace x >> windows (copy x) >> windows (W.view x))),
      ("M-6", addName "Switch with last workspace" $ windows \ws -> flip W.view ws $ W.tag $ head $ filter ((/= "NSP") . W.tag) $ W.hidden ws),
      -- Layouts & Layout Messages
      ("M-;", addName "Cycle to next layout" $ sendMessage NextLayout),
      -- ("M-[", addName "Shrink master area" $ sendMessage Shrink),
      -- ("M-]", addName "Expand master area" $ sendMessage Expand),
      ("M-<Return>", addName "Toggle magnifier" $ withFocused $ sendMessage . maximizeRestore),
      ("M-t", addName "Pick or place a window" $ withFocused $ sendMessage . pickOrPlace),
      ("M-c", addName "Collapse or decollapse a window" $ withFocused $ sendMessage . toggleCollapsed),
      -- Scratchpads
      ("M-s M-b", addName "Toggle scratchpad 'sysmon'" $ namedScratchpadAction myScratchpads "sysmon"),
      ("M-s M-q", addName "Toggle scratchpad 'terminal'" $ namedScratchpadAction myScratchpads "terminal"),
      ("M-s M-c", addName "Toggle scratchpad 'calendar'" $ namedScratchpadAction myScratchpads "calendar"),
      ("M-s M-a", addName "Toggle scratchpad 'audio'" $ namedScratchpadAction myScratchpads "audio"),
      ("M-s M-S-m", addName "Toggle scratchpad 'deezer'" $ namedScratchpadAction myScratchpads "deezer"),
      ("M-s M-s", addName "Toggle scratchpad 'slack'" $ namedScratchpadAction myScratchpads "slack"),
      ("M-s M-d", addName "Toggle scratchpad 'discord'" $ namedScratchpadAction myScratchpads "discord"),
      ("M-s M-w", addName "Toggle scratchpad 'whatsapp'" $ namedScratchpadAction myScratchpads "whatsapp"),
      ("M-s M-g", addName "Toggle scratchpad 'googlechat'" $ namedScratchpadAction myScratchpads "googlechat"),
      ("M-s M-t", addName "Toggle scratchpad 'todos'" $ namedScratchpadAction myScratchpads "todos"),
      ("M-s M-e", addName "Toggle scratchpad 'mail'" $ namedScratchpadAction myScratchpads "mail"),
      ("M-s M-m", addName "Toggle scratchpad 'element'" $ namedScratchpadAction myScratchpads "element"),
      ("M-s M-l", addName "Toggle scratchpad 'localsend'" $ namedScratchpadAction myScratchpads "localsend"),
      -- Passwords
      ("M-p M-p", addName "Get a password" $ spawn "rofi-pass"),
      -- Screenshots
      ("M-y M-s", addName "Yank the whole screen" $ spawn scrotScreen),
      ("M-y M-f", addName "Yank the current window" $ spawn scrotThiswindow),
      ("M-y M-r", addName "Yank a region of the screen" $ spawn scrotRegion),
      -- Background
      ("M-b M-r", addName "Set a random background" $ spawn "background-setter set"),
      ("M-b M-a", addName "Automatically set a random background every 1h" $ spawn "background-setter auto"),
      ("M-b M-b", addName "Automatically set a random background every 1h" $ spawn "background-setter choose"),
      -- System
      ("M-q M-S-q", addName "Quit XMonad" $ io exitSuccess),
      ("M-q M-S-s", addName "Suspend" $ spawn "systemctl suspend"),
      ("M-q M-S-h", addName "Hibernate" $ spawn "systemctl hibernate"),
      ("M-q M-b", addName "Blank the screen" $ spawn "sleep 0.5; xset dpms force off"),
      ("M-q M-l", addName "Lock" $ spawn "dm-tool lock"),
      ("M-q M-d M-[", addName "Enable 'do not disturb'" $ spawn "dunstctl set-paused true"),
      ("M-q M-d M-]", addName "Disable 'do not disturb'" $ spawn "dunstctl set-paused false"),
      ("M-q M-=", addName "Toggle Statusbar" $ spawn "toggle_statusbar"),
      ("M-q M-v M-u", addName "Enable VPN" $ mvpn "up"),
      ("M-q M-v M-d", addName "Disable VPN" $ mvpn "down"),
      -- Notifications
      ("M-S-n M-S-c", addName "Close notification" $ spawn "dunstctl close"),
      ("M-S-n M-c", addName "Close all notifications" $ spawn "dunstctl close-all"),
      ("M-S-n M-S-r", addName "Redisplay the most recently closed notification" $ spawn "dunstctl history-pop"),
      -- -- Mouse actions
      -- , ("M-C-S-m", addName "Open mouse actions gridselect" mouseActionsGridSelect)
      -- Function Keys
      ( "M-<Right>", addName "Raise brightness" $ spawn "brightnessctl set +5%" >> spawnBrightnessOSD),
      ( "M-<Left>", addName "Lower brightness" $ spawn "brightnessctl set 5%-" >> spawnBrightnessOSD),
      ( "M-<Up>", addName "Raise volume" $ spawn ("pamixer -i 2; paplay " ++ volumeChangeSound) >> spawnVolumeOSD),
      ( "M-<Down>", addName "Lower volume" $ spawn ("pamixer -d 2; paplay " ++ volumeChangeSound) >> spawnVolumeOSD),
      ( "M-*", addName "Toggle mute" $ spawn ("pamixer -t; paplay " ++ volumeChangeSound) >> spawnVolumeOSD),
      ("M-C-v", addName "Play test sound" $ spawnVolumeOSD >> spawn ("paplay " ++ volumeChangeSound))
    ]

myMouse config =
  M.fromList
    [ ((modMask config, button2), considerFloat (windows . W.sink) (sendMessage . maximizeRestore)),
      ((modMask config, button1), considerFloat translateFloatingWindow dragWindow),
      ((modMask config, button3), considerFloat resizeFloatingWindow floatTiledWindow)
    ]
  where
    translateFloatingWindow w = focus w >> mouseMoveWindow w >> windows W.shiftMaster
    resizeFloatingWindow w = focus w >> mouseResizeWindow w >> windows W.shiftMaster
    floatTiledWindow w = windows $ W.float w (W.RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3))
    considerFloat whenFloat whenTiled w = do
      isFloat <- withWindowSet $ \ws -> return $ M.member w $ W.floating ws
      if isFloat then whenFloat w else whenTiled w
    considerClass cls whenOfClass whenNotOfClass w = do
      isOfClass <- hasProperty (ClassName cls) w
      if isOfClass then whenOfClass w else whenNotOfClass w

killCopy =
  let delete'' w = W.modify Nothing $ W.filter (/= w)
   in withWindowSet \ss ->
        whenJust (W.peek ss) \w ->
          when (W.member w $ delete'' w ss) $
            windows $ delete'' w

changeScreenConfig = do
  profiles <- io $ listDirectory ".config/autorandr"
  maybe_profile_name <- gridselect csillagGridSelectConfig ((\x -> (x, x)) <$> profiles)
  case maybe_profile_name of
    Just profile_name -> spawn $ "autorandr --load " ++ profile_name
    Nothing -> return ()

mvpn :: String -> X ()
mvpn action = do
  vpns <- io $ listDirectory ".local/share/vpns"
  maybe_vpn_name <- gridselect csillagGridSelectConfig ((\x -> (x, x)) <$> vpns)
  case maybe_vpn_name of
    Just vpn_name -> spawn $ "mvpn " ++ action ++ " " ++ vpn_name
    Nothing -> return ()

changeKeyboard :: X ()
changeKeyboard = do
  let basedir = ".local/share/keyboards/"
  kbds <- io $ listDirectory basedir
  maybe_kbd_name <- gridselect csillagGridSelectConfig ((\x -> (x, x)) <$> kbds)
  case maybe_kbd_name of
    Just kbd_name -> liftIO (readFile $ basedir ++ kbd_name) >>= spawn . ("kb " ++)
    Nothing -> return ()

promptWorkspaces prompt maybe_message = withWindowSet \ws -> do
    let wss = filter (/= "NSP") $ map W.tag $ filter ((/= "NSP") . W.tag) $ circshiftN (succ $ length $ W.visible ws) $ W.workspaces ws
    rofi prompt maybe_message "select or create a new workspace" wss
    where
      circshiftN :: Int -> [a] -> [a]
      circshiftN 0 lst = lst
      circshiftN k lst = circshiftN (pred k) $ circshift lst

      circshift :: [a] -> [a]
      circshift [] = []
      circshift (x : xs) = xs ++ [x]

notify :: String -> X ()
notify msg = spawn $ "dunstify -a xmonad XMonad '" ++ msg ++ "'"

spawnVolumeOSD :: X ()
spawnVolumeOSD = spawn "eww open volume --duration 1s"

spawnBrightnessOSD :: X ()
spawnBrightnessOSD = spawn "eww open brightness --duration 1s"

getNumber :: String -> Double
getNumber = read . takeWhile isDigit

asciibar :: Double -> String
asciibar x = p ++ "% [" ++ replicate k '#' ++ replicate (n - k) ' ' ++ "]"
  where
    n = 20
    k = floor $ fromIntegral n * x / 100
    p = case show (floor x :: Int) of
      s@[_] -> ' ' : ' ' : s
      s@[_, _] -> ' ' : s
      s -> s

cmdout :: MonadIO m => String -> [String] -> m String
cmdout c argv = runProcessWithInput c argv ""

goToScreen :: ScreenId -> X ()
goToScreen i = do
  screenWorkspace i >>= flip whenJust (windows . W.view)
  warpToScreen i 0.5 0.5

sendToScreen :: ScreenId -> X ()
sendToScreen i = do
  screenWorkspace i >>= flip whenJust (windows . W.shift)
  goToScreen i

rofiArgs :: MonadIO m => String -> String -> [String] -> [String] -> m (Maybe String)
rofiArgs prompt placeholder args options = do
    out <- menuArgs "rofi" (["-dmenu", "-matching", "fuzzy", "-i", "-p", prompt, "-theme-str", "entry { placeholder: \"" ++ placeholder ++ "\"; }"] ++ args) options
    return $ if null out then Nothing else Just out

rofi :: MonadIO m => String -> Maybe String -> String -> [String] -> m (Maybe String)
rofi prompt maybe_message placeholder options = rofiArgs prompt placeholder (maybe [] (\msg -> ["-mesg", msg]) maybe_message) options
