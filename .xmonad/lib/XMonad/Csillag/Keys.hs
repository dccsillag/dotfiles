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

myKeys =
  flip
    mkNamedKeymap
    -- Core:
    [ ("M-r", addName "Restart XMonad" $ spawn "restart-xmonad"),
      ("M-S-9", addName "Kill the compositor" $ spawn compositorKill),
      ("M-S-0", addName "Start the compositor" $ spawn compositorSpawn),
      ("M-S-8", addName "Restart the compositor" $ spawn compositorRestart),
      ("M-<Space>", addName "Change keyboard" changeKeyboard),
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
      ("M-m", addName "Focus master window" $ windows W.focusMaster),
      ("M-S-m", addName "Swap with master window" $ windows W.swapMaster),
      ("M-S-<Tab>", addName "Focus up on the stack" $ windows W.focusUp),
      ("M-<Tab>", addName "Focus down on the stack" $ windows W.focusDown),
      ("M-,", addName "Focus up on the stack" $ windows W.focusUp),
      ("M-.", addName "Focus down on the stack" $ windows W.focusDown),
      ("M-S-,", addName "Swap window up on the stack" $ windows W.swapUp),
      ("M-S-.", addName "Swap window down on the stack" $ windows W.swapDown),
      -- Spawn Stuff
      ("M-n M-n", addName "Open shell prompt" $ launcherPrompt csillagPromptConfig),
      ("M-n M-t", addName "Spawn a terminal" $ spawn termSpawn),
      ("M-n M-S-t", addName "Spawn a terminal with SSH" $ sshPrompt csillagPromptConfig),
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
      ("M-w M-r", addName "Rename workspace" $ inputPrompt csillagPromptConfig "Rename Workspace" ?+ renameWorkspaceByName),
      ("M-w M-d", addName "Delete workspace" $ removeEmptyWorkspaceAfter (windows \ws -> flip W.view ws $ W.tag $ head $ filter ((/= "NSP") . W.tag) $ W.hidden ws)),
      ("M-w M-g", addName "Go to workspace" $ myGridSelectWorkspace $ windows . W.view),
      ("M-w M-s", addName "Send to workspace" $ myGridSelectWorkspace $ windows . W.shift),
      ("M-w M-C-g", addName "Send&Go to workspace" $ myGridSelectWorkspace \x -> windows (W.shift x) >> windows (W.view x)),
      ("M-w M-S-c", addName "Send copy to workspace" $ myGridSelectWorkspace $ windows . copy),
      ("M-w M-c", addName "Send&Go copy to workspace" $ myGridSelectWorkspace \x -> windows (copy x) >> windows (W.view x)),
      ("M-w M-b", addName "Bring from workspace" workspaceBring),
      ("M-w M-n M-g", addName "Go to new workspace" $ inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (W.view wkname))),
      ("M-w M-n M-s", addName "Send to new workspace" $ inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (W.shift wkname))),
      ("M-w M-n M-C-g", addName "Send&Go to new workspace" $ inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (W.shift wkname) >> windows (W.view wkname))),
      ("M-w M-n M-S-c", addName "Send copy to new workspace" $ inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (copy wkname))),
      ("M-w M-n M-c", addName "Send&Go copy to new workspace" $ inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (copy wkname) >> windows (W.view wkname))),
      ("M-6", addName "Switch with last workspace" $ windows \ws -> flip W.view ws $ W.tag $ head $ filter ((/= "NSP") . W.tag) $ W.hidden ws),
      -- Layouts
      ("M-c M-<Space>", addName "Cycle to next layout" $ sendMessage NextLayout),
      ("M-c M-c", addName "Change layout" changeLayoutGridselect),
      ("M-c M-l M-g", addName "Set layout to 'Grid'" $ sendMessage $ JumpToLayout "Grid"),
      ("M-c M-l M-t", addName "Set layout to 'ThreeColMid'" $ sendMessage $ JumpToLayout "ThreeColMid"),
      ("M-c M-l M-d", addName "Set layout to 'Dishes'" $ sendMessage $ JumpToLayout "Dishes"),
      ("M-c M-l M-o", addName "Set layout to 'OneBig'" $ sendMessage $ JumpToLayout "OneBig"),
      ("M-c M-l M-S-d", addName "Set layout to 'Dwindle'" $ sendMessage $ JumpToLayout "Dwindle"),
      ("M-c M-l M-m", addName "Set layout to 'Mirror Dwindle'" $ sendMessage $ JumpToLayout "Mirror Dwindle"),
      ("M-c M-l M-S-t", addName "Set layout to 'Tall'" $ sendMessage $ JumpToLayout "Tall"),
      ("M-c M-l M-S-m", addName "Set layout to 'Mirror Tall'" $ sendMessage $ JumpToLayout "Mirror Tall"),
      ("M-c M-l M-s", addName "Set layout to 'Spiral'" $ sendMessage $ JumpToLayout "Spiral"),
      ("M-c M-l M-a", addName "Set layout to 'Accordion'" $ sendMessage $ JumpToLayout "Accordion"),
      ("M-c M-l M-c", addName "Set layout to 'Circle'" $ sendMessage $ JumpToLayout "Circle"),
      ("M-c M-l M-p", addName "Set layout to 'Plus'" $ sendMessage $ JumpToLayout "Plus"),
      ("M-c M-l M-f", addName "Set layout to 'Full'" $ sendMessage $ JumpToLayout "Full"),
      -- Layout Messages
      ("M-[", addName "Shrink master area" $ sendMessage Shrink),
      ("M-]", addName "Expand master area" $ sendMessage Expand),
      ("M-S-[", addName "Add one window to master pane" $ sendMessage $ IncMasterN 1),
      ("M-S-]", addName "Take one window from master pane" $ sendMessage $ IncMasterN (-1)),
      ("M-<Return>", addName "Toggle magnifier" $ withFocused $ sendMessage . maximizeRestore),
      -- Scratchpads
      ("M-s M-b", addName "Toggle scratchpad 'sysmon'" $ namedScratchpadAction myScratchpads "sysmon"),
      ("M-s M-q", addName "Toggle scratchpad 'terminal'" $ namedScratchpadAction myScratchpads "terminal"),
      ("M-s M-c", addName "Toggle scratchpad 'calculator'" $ namedScratchpadAction myScratchpads "calculator"),
      ("M-s M-a", addName "Toggle scratchpad 'audio'" $ namedScratchpadAction myScratchpads "audio"),
      ("M-s M-S-m", addName "Toggle scratchpad 'deezer'" $ namedScratchpadAction myScratchpads "deezer"),
      ("M-s M-s", addName "Toggle scratchpad 'slack'" $ namedScratchpadAction myScratchpads "slack"),
      ("M-s M-d", addName "Toggle scratchpad 'discord'" $ namedScratchpadAction myScratchpads "discord"),
      ("M-s M-w", addName "Toggle scratchpad 'whatsapp'" $ namedScratchpadAction myScratchpads "whatsapp"),
      ("M-s M-t", addName "Toggle scratchpad 'telegram'" $ namedScratchpadAction myScratchpads "telegram"),
      ("M-s M-e", addName "Toggle scratchpad 'mail'" $ namedScratchpadAction myScratchpads "mail"),
      ("M-s M-m", addName "Toggle scratchpad 'element'" $ namedScratchpadAction myScratchpads "element"),
      -- Passwords
      ("M-p M-p", addName "Get a password" $ passPrompt csillagPromptConfig),
      ("M-p M-g", addName "Generate a random password" $ passGeneratePrompt csillagPromptConfig),
      ("M-p M-n", addName "Insert a new password" $ passTypePrompt csillagPromptConfig),
      ("M-p M-S-d", addName "Remove a password" $ passRemovePrompt csillagPromptConfig),
      -- Screenshots
      ("M-y M-s", addName "Yank the whole screen" $ spawn scrotScreen),
      ("M-y M-w", addName "Yank a window" $ spawn scrotWindow),
      ("M-y M-f", addName "Yank the current window" $ spawn scrotThiswindow),
      ("M-y M-a", addName "Yank an area of the screen" $ spawn scrotRegion),
      -- Background
      ("M-b M-r", addName "Set a random background" $ spawn "background-setter set"),
      ("M-b M-a", addName "Automatically set a random background every 1h" $ spawn "background-setter auto"),
      ("M-b M-b", addName "Automatically set a random background every 1h" $ spawn "background-setter choose"),
      -- Devices
      ("M-d M-t M-w M-k", addName "Map wacom tablet to current window, with orientation 'up'" $ spawn "wacom-map up"),
      ("M-d M-t M-w M-j", addName "Map wacom tablet to current window, with orientation 'down'" $ spawn "wacom-map down"),
      ("M-d M-t M-w M-h", addName "Map wacom tablet to current window, with orientation 'cw'" $ spawn "wacom-map cw"),
      ("M-d M-t M-w M-l", addName "Map wacom tablet to current window, with orientation 'ccw'" $ spawn "wacom-map ccw"),
      ("M-d M-t M-s", addName "Map wacom tablet to whole screen" $ spawn "wacom-map screen"),
      -- System
      ("M-q M-S-q", addName "Quit XMonad" $ io exitSuccess),
      ("M-q M-S-s", addName "Suspend" $ spawn "systemctl suspend"),
      ("M-q M-S-h", addName "Hibernate" $ spawn "systemctl hibernate"),
      ("M-q M-b", addName "Blank the screen" $ spawn "sleep 0.5; xset dpms force off"),
      ("M-q M-l", addName "Lock" $ spawn "lock"),
      ("M-q M-d M-[", addName "Enable 'do not disturb'" $ spawn "dunstctl set-paused true"),
      ("M-q M-d M-]", addName "Disable 'do not disturb'" $ spawn "dunstctl set-paused false"),
      ("M-q M-a", addName "Fix audio" $ spawn "fix-audio" >> notify "Spawn: fix-audio"),
      ("M-q M-=", addName "Toggle Statusbar" $ spawn "toggle_statusbar"),
      ("M-q M-k", addName "Toggle Screenkey" $ spawn ".local/scripts/screenkey_toggle.sh"),
      ("M-q M-v M-u", addName "Enable VPN" $ mvpn "up"),
      ("M-q M-v M-d", addName "Disable VPN" $ mvpn "down"),
      -- Notifications
      ("M-S-n M-S-c", addName "Close notification" $ spawn "dunstctl close"),
      ("M-S-n M-c", addName "Close all notifications" $ spawn "dunstctl close-all"),
      ("M-S-n M-S-r", addName "Redisplay the most recently closed notification" $ spawn "dunstctl history-pop"),
      -- -- Mouse actions
      -- , ("M-C-S-m", addName "Open mouse actions gridselect" mouseActionsGridSelect)
      -- Function Keys
      ( "M-<Right>",
        addName "Raise brightness" $
          spawn "brightnessctl set +5%"
            >> spawnOSD "Brightness" (asciibar . (/ 256) . (* 100) . getNumber <$> cmdout "brightnessctl" ["get"])
      ),
      ( "M-<Left>",
        addName "Lower brightness" $
          spawn "brightnessctl set 5%-"
            >> spawnOSD "Brightness" (asciibar . (/ 256) . (* 100) . getNumber <$> cmdout "brightnessctl" ["get"])
      ),
      ( "M-<Up>",
        addName "Raise volume" $
          spawn ("pamixer -i 2; paplay " ++ volumeChangeSound)
            >> spawnOSD "Volume" (asciibar . getNumber <$> cmdout "pamixer" ["--get-volume"])
      ),
      ( "M-<Down>",
        addName "Lower volume" $
          spawn ("pamixer -d 2; paplay " ++ volumeChangeSound)
            >> spawnOSD "Volume" (asciibar . getNumber <$> cmdout "pamixer" ["--get-volume"])
      ),
      ( "M-*",
        addName "Toggle mute" $
          spawn ("pamixer -t; paplay " ++ volumeChangeSound)
            >> spawnOSD "Volume" ((\case "true\n" -> "Muted."; "false\n" -> "Unmuted."; _ -> "?") <$> cmdout "pamixer" ["--get-mute"])
      ),
      ("M-C-v", addName "Play test sound" $ spawnOSD "Sound" (pure "Played test sound.") >> spawn ("paplay " ++ volumeChangeSound)),
      ("M-\\", addName "Toggle play/pause" $ spawn "mcm toggle"),
      ("M-S-\\", addName "Go to next track" $ spawn "mcm next")
    ]

myMouse config =
  M.fromList
    [ ((modMask config, button2), considerClass "dzen" mouseActions $ considerFloat (windows . W.sink) (sendMessage . maximizeRestore)),
      ((modMask config, button1), considerClass "dzen" (const toggleSuper) $ considerFloat translateFloatingWindow dragWindow),
      ((modMask config, button3), considerFloat resizeFloatingWindow floatTiledWindow)
    ]
  where
    translateFloatingWindow w = focus w >> mouseMoveWindow w >> windows W.shiftMaster
    resizeFloatingWindow w = focus w >> mouseResizeWindow w >> windows W.shiftMaster
    floatTiledWindow w = windows $ W.float w (W.RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3))
    toggleSuper = spawn ".local/scripts/statusbar/toggle-super-key.sh"
    mouseActions _ = mouseActionsGridSelect >> toggleSuper
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

workspaceBring = myGridSelectWorkspace \x -> windows \ws ->
  case filter ((== x) . W.tag) $ W.hidden ws of
    targetWorkspace : _ -> foldl (\acc w -> copyWindow w (W.tag $ W.workspace $ W.current acc) acc) ws $ W.integrate' $ W.stack targetWorkspace
    _ -> ws

changeScreenConfig = do
  profiles <- io $ listDirectory ".config/autorandr"
  maybe_profile_name <- gridselect csillagGridSelectConfig ((\x -> (x, x)) <$> profiles)
  case maybe_profile_name of
    Just profile_name -> spawn $ "autorandr --load " ++ profile_name
    Nothing -> return ()

changeLayoutGridselect =
  gridselect
    csillagGridSelectConfig
    ( map
        (\x -> (x, x))
        [ "Grid",
          "ThreeColMid",
          "Dishes",
          "OneBig",
          "Dwindle",
          "Mirror Dwindle",
          "Tall",
          "Mirror Tall",
          "Spiral",
          "Accordion",
          "Circle",
          "Plus"
        ]
    )
    >>= flip whenJust (sendMessage . JumpToLayout)

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

myGridSelectWorkspace func = withWindowSet \ws -> do
  let wss =
        filter (/= "NSP") $
          map W.tag $
            filter ((/= "NSP") . W.tag) $
              circshiftN (succ $ length $ W.visible ws) $
                W.workspaces ws
  gridselect csillagWorkspaceGridSelectConfig (zip wss wss) >>= flip whenJust func
  where
    circshiftN :: Int -> [a] -> [a]
    circshiftN 0 lst = lst
    circshiftN k lst = circshiftN (pred k) $ circshift lst

    circshift :: [a] -> [a]
    circshift [] = []
    circshift (x : xs) = xs ++ [x]

notify :: String -> X ()
notify msg = spawn $ "dunstify -a xmonad XMonad '" ++ msg ++ "'"

spawnOSD :: String -> X String -> X ()
spawnOSD what extra = do
  io $ threadDelay $ seconds 0.05 -- FIXME: run this in another thread
  extra' <- extra
  spawn $ "dunstify -a xmonad-osd -r 2166983 -- '" ++ what ++ "' '" ++ extra' ++ "'"

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

launcherPrompt :: XPConfig -> X ()
launcherPrompt c = do
  ps <- io $ listDirectory applicationsDirectory
  ps' <- io $ forM ps \p -> fmap (p,) <$> getDesktopFileName p
  let ps'' = catMaybes ps'
  maybe (return ()) (launchProgram ps'')
    =<< inputPromptWithCompl c "Launch" (mkComplFunFromList' c $ snd <$> ps'')
  where
    applicationsDirectory = "/usr/share/applications"

    getDesktopFileName :: String -> IO (Maybe String)
    getDesktopFileName p = do
      ls <- lines <$> readFile (applicationsDirectory ++ "/" ++ p)
      return $ case filter ("Name=" `isPrefixOf`) ls of
        l : _ -> map toLower <$> stripPrefix "Name=" l
        [] -> Nothing

    launchProgram :: [(String, String)] -> String -> X ()
    launchProgram ps' p = spawn $ "gtk-launch " ++ fst (head $ filter ((== p) . snd) ps')

mouseActionsGridSelect :: X ()
mouseActionsGridSelect = do
  menu
    [ ("Close the focused window", kill1),
      ( "Programs..",
        menu
          [ ( "Scratchpads..",
              menu
                [ ("System Monitor", namedScratchpadAction myScratchpads "sysmon"),
                  ("Calculator", namedScratchpadAction myScratchpads "calculator"),
                  ("Audio", namedScratchpadAction myScratchpads "audio"),
                  ("Slack", namedScratchpadAction myScratchpads "slack"),
                  ("Discord", namedScratchpadAction myScratchpads "discord"),
                  ("WhatsApp", namedScratchpadAction myScratchpads "whatsapp"),
                  ("Telegram", namedScratchpadAction myScratchpads "telegram"),
                  ("Email", namedScratchpadAction myScratchpads "mail")
                ]
            ),
            ("Terminal", spawn termSpawn),
            ("File manager", spawn filemanagerSpawn),
            ("Browser", spawn browserSpawn),
            ("Private Browser", spawn browserSpawnPrivate),
            ("Camera", spawn camviewSpawn),
            ("Notebook", spawn "xournalpp"),
            ("Calculator", spawn calculatorSpawn)
          ]
      ),
      ( "Manage workspaces..",
        menu
          [ ("Go to workspace..", myGridSelectWorkspace $ windows . W.view),
            ("Send to workspace..", myGridSelectWorkspace $ windows . W.shift),
            ("Send&Go to workspace..", myGridSelectWorkspace \x -> windows (W.shift x) >> windows (W.view x)),
            ("Send copy to workspace..", myGridSelectWorkspace $ windows . copy),
            ("Send&Go copy to workspace..", myGridSelectWorkspace \x -> windows (copy x) >> windows (W.view x))
          ]
      ),
      ("Set layout..", changeLayoutGridselect),
      ( "Yank the screen..",
        menu
          [ ("Yank the whole screen", spawn scrotScreen),
            ("Yank a window", spawn scrotWindow),
            ("Yank the current window", spawn scrotThiswindow),
            ("Yank an area of the screen", spawn scrotRegion)
          ]
      )
    ]
  where
    menu :: [(String, X ())] -> X ()
    menu xs = gridselect csillagGridSelectConfig xs >>= flip whenJust (io (threadDelay $ seconds 0.15) >>)

goToScreen :: ScreenId -> X ()
goToScreen i = do
  screenWorkspace i >>= flip whenJust (windows . W.view)
  warpToScreen i 0.5 0.5

sendToScreen :: ScreenId -> X ()
sendToScreen i = do
  screenWorkspace i >>= flip whenJust (windows . W.shift)
  goToScreen i
