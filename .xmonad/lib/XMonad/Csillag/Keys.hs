{-# LANGUAGE LambdaCase #-}

module XMonad.Csillag.Keys
  ( myKeys
  , myMouseBindings
  )
where

import Data.List
import qualified Data.Map as Map
import Control.Monad

import XMonad.Csillag.CommonActions
import XMonad.Csillag.Scratchpads
import XMonad.Csillag.Layouts (windowGap, MAGNIFIER(..), WINDOWTITLES(..))
import XMonad.Csillag.Consts
import XMonad.Csillag.Externals

import XMonad hiding (config, keys)
import qualified XMonad.StackSet as W
import Graphics.X11.ExtraTypes.XF86

import XMonad.Util.EZConfig
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Spacing

import XMonad.Prompt ( )
import XMonad.Prompt.Shell
import XMonad.Prompt.Input
import XMonad.Prompt.Pass
import XMonad.Prompt.XMonad
import XMonad.Prompt.Ssh
import XMonad.Hooks.ManageDocks
import XMonad.Actions.Navigation2D
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Util.NamedScratchpad
import XMonad.Actions.TiledWindowDragging
import XMonad.Layout.MultiToggle


myKeys = flip mkKeymap $
    -- Core:
    [ ("M-r M-r",   restart_xmonad False)
    , ("M-r M-S-r", restart_xmonad True)
    , ("M-S-9",     spawn compositor_kill)
    , ("M-S-0",     spawn compositor_spawn)
    , ("M-S-8",     spawn compositor_restart)
    , ("M-;",       xmonadPrompt csillagPromptConfig)
    -- Directional keys
    , ("M-h",       windowGo L False)
    , ("M-j",       windowGo D False)
    , ("M-k",       windowGo U False)
    , ("M-l",       windowGo R False)
    , ("M-S-h",     windowSwap L False)
    , ("M-S-j",     windowSwap D False)
    , ("M-S-k",     windowSwap U False)
    , ("M-S-l",     windowSwap R False)
    -- Stack Keys
    , ("M-m",       windows W.focusMaster)
    , ("M-S-m",     windows W.swapMaster)
    , ("M-S-<Tab>", windows W.focusUp)
    , ("M-<Tab>",   windows W.focusDown)
    , ("M-,",       windows W.focusUp)
    , ("M-.",       windows W.focusDown)
    , ("M-S-,",     windows W.swapUp)
    , ("M-S-.",     windows W.swapDown)
    -- Spawn Stuff
    , ("M-n M-n",   shellPrompt csillagPromptConfig)
    , ("M-n M-t",   spawn term_spawn)
    , ("M-n M-S-t", sshPrompt csillagPromptConfig)
    , ("M-n M-f",   spawn filemanager_spawn)
    , ("M-n M-v",   spawn texteditor_spawn)
    , ("M-n M-b",   spawn browser_spawn)
    , ("M-n M-S-b", spawn browser_spawn_private)
    , ("M-n M-c",   spawn camview_spawn)
    , ("M-n M-w",   spawn "xournalpp")
    -- Floating Windows
    , ("M-f M-t",   withFocused $ windows . W.sink)
    , ("M-f M-s",   withFocused $ windows . flip W.float (W.RationalRect 0.05 0.05 0.9 0.9))
    -- Close Windows
    , ("M-x M-x",   kill1)
    , ("M-x M-d",   kill)
    , ("M-x M-c",   kill_copy)
    -- Screens (Xinerama)
    , ("M-S-s M-S-a", screenWorkspace 0 >>= flip whenJust (windows . W.view))
    , ("M-S-s M-S-q", screenWorkspace 1 >>= flip whenJust (windows . W.view))
    , ("M-S-s M-S-s", screenSwap R True)
    , ("M-S-s M-S-s", screenSwap R True)
    , ("M-S-s M-S-c", change_screen_config)
    , ("M-S-s M-S-o M-S-k", spawn $ set_screen_orientation "normal" 0)
    , ("M-S-s M-S-o M-S-j", spawn $ set_screen_orientation "inverted" 0)
    , ("M-S-s M-S-o M-S-h", spawn $ set_screen_orientation "left" 0)
    , ("M-S-s M-S-o M-S-l", spawn $ set_screen_orientation "right" 0)
    -- Workspaces
    , ("M-w M-r", inputPrompt csillagPromptConfig "Rename Workspace" ?+ renameWorkspaceByName)
    , ("M-w M-d", removeEmptyWorkspaceAfter (windows $ \ws -> flip W.view ws $ W.tag $ head $ filter ((/="NSP") . W.tag) $ W.hidden ws))
    , ("M-w M-g", myGridSelectWorkspace myGridSelectConfig $ windows . W.view)
    , ("M-w M-s", myGridSelectWorkspace myGridSelectConfig $ windows . W.shift)
    , ("M-w M-C-g", myGridSelectWorkspace myGridSelectConfig $ \x -> windows (W.shift x) >> windows (W.view x))
    , ("M-w M-S-c", myGridSelectWorkspace myGridSelectConfig $ \x -> windows (copy x))
    , ("M-w M-c", myGridSelectWorkspace myGridSelectConfig $ \x -> windows (copy x) >> windows (W.view x))
    , ("M-w M-b", workspace_bring)
    , ("M-w M-n M-g", inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (W.view wkname)))
    , ("M-w M-n M-s", inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (W.shift wkname)))
    , ("M-w M-n M-C-g", inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (W.shift wkname) >> windows (W.view wkname)))
    , ("M-w M-n M-S-c", inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (copy wkname)))
    , ("M-w M-n M-c", inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (copy wkname) >> windows (W.view wkname)))
    , ("M-6", windows $ \ws -> flip W.view ws $ W.tag $ head $ filter ((/="NSP") . W.tag) $ W.hidden ws)
    -- Layouts
    , ("M-c M-<Space>", sendMessage NextLayout)
    , ("M-c M-c", change_layout_gridselect)
    , ("M-c M-l M-m", sendMessage $ JumpToLayout "Mosaic")
    , ("M-c M-l M-g", sendMessage $ JumpToLayout "Grid")
    , ("M-c M-l M-t", sendMessage $ JumpToLayout "ThreeColMid")
    , ("M-c M-l M-d", sendMessage $ JumpToLayout "Dishes")
    , ("M-c M-l M-o", sendMessage $ JumpToLayout "OneBig")
    , ("M-c M-l M-f", sendMessage $ JumpToLayout "Full")
    , ("M-c M-l M-S-d", sendMessage $ JumpToLayout "Dwindle")
    , ("M-c M-l M-S-m", sendMessage $ JumpToLayout "Mirror Dwindle")
    -- Layout Messages
    , ("M-[",          sendMessage Shrink)
    , ("M-]",          sendMessage Expand)
    , ("M-{",          sendMessage $ IncMasterN 1)
    , ("M-}",          sendMessage $ IncMasterN (-1))
    , ("M-<Return>",   sendMessage $ Toggle MAGNIFIER)
    , ("M-S-<Return>", sendMessage $ Toggle WINDOWTITLES)
    -- Scratchpads
    , ("M-s M-t",   namedScratchpadAction myScratchpads "sysmon")
    , ("M-s M-b",   namedScratchpadAction myScratchpads "bluetooth")
    , ("M-s M-q",   namedScratchpadAction myScratchpads "terminal")
    , ("M-s M-a",   namedScratchpadAction myScratchpads "audio")
    , ("M-s M-z",   namedScratchpadAction myScratchpads "advancedaudio")
    , ("M-s M-S-m", namedScratchpadAction myScratchpads "deezer")
    , ("M-s M-x",   namedScratchpadAction myScratchpads "todo")
    , ("M-s M-s",   namedScratchpadAction myScratchpads "slack")
    , ("M-s M-d",   namedScratchpadAction myScratchpads "discord")
    , ("M-s M-w",   namedScratchpadAction myScratchpads "whatsapp")
    , ("M-s M-e",   namedScratchpadAction myScratchpads "mail")
    , ("M-s M-c",   namedScratchpadAction myScratchpads "calendar")
    -- Passwords
    , ("M-p M-p",   passPrompt csillagPromptConfig)
    , ("M-p M-g",   passGeneratePrompt csillagPromptConfig)
    , ("M-p M-n",   passTypePrompt csillagPromptConfig)
    , ("M-p M-S-d", passRemovePrompt csillagPromptConfig)
    -- Screenshots
    , ("M-y M-s",   spawn scrot_screen)
    , ("M-y M-w",   spawn scrot_window)
    , ("M-y M-f",   spawn scrot_thiswindow)
    -- Background
    , ("M-b M-r", spawn "background-setter set")
    , ("M-b M-a", spawn "background-setter auto")
    -- Devices
    , ("M-d M-t M-w M-k", spawn "wacom-map up")
    , ("M-d M-t M-w M-j", spawn "wacom-map down")
    , ("M-d M-t M-w M-h", spawn "wacom-map cw")
    , ("M-d M-t M-w M-l", spawn "wacom-map ccw")
    , ("M-d M-t M-s",     spawn "wacom-map screen")
    -- System
    , ("M-q M-S-q",   quitWithWarning)
    , ("M-q M-S-s",   spawn "systemctl suspend")
    , ("M-q M-b",     spawn "sleep 0.5; xset dpms force off")
    , ("M-q M-l",     spawn "slock")
    , ("M-q M-d M-[", spawn "notify-send \"DUNST_COMMAND_PAUSE\" && touch /home/daniel/.dunst_paused")
    , ("M-q M-d M-]", spawn "notify-send \"DUNST_COMMAND_RESUME\" && rm -f /home/daniel/.dunst_paused")
    , ("M-q M-a",     spawn "fix-audio" >> spawnOSD "A")
    , ("M-q M-=",     spawn "toggle_xmobar")
    , ("M-q M-k",     spawn ".local/scripts/screenkey_toggle.sh")
    -- Function Keys
    , ("<XF86MonBrightnessUp>",   spawn "lux -a 1%" >> spawnOSD brightnessUpIcon)
    , ("<XF86MonBrightnessDown>", spawn "lux -s 1%" >> spawnOSD brightnessDownIcon)
    , ("<XF86AudioLowerVolume>",  spawnOSD volumeDownIcon >> spawn "amixer sset Master 2%-" >> spawn ("sleep 0.1; paplay " ++ volumeChangeSound))
    , ("<XF86AudioRaiseVolume>",  spawnOSD volumeUpIcon >> spawn "amixer sset Master 2%+" >> spawn ("sleep 0.1; paplay " ++ volumeChangeSound))
    , ("<XF86AudioMute>",         spawnOSD volumeMuteIcon >> spawn "amixer sset Master toggle" >> spawn ("sleep 0.1; paplay " ++ volumeChangeSound))
    , ("M-C-v",                   spawnOSD volumePlayIcon >> spawn ("paplay " ++ volumeChangeSound))
    , ("M-\\",                    spawn "mmc toggle")
    , ("M-S-\\",                  spawn "mmc next")
    ]

myMouseBindings config = Map.fromList
    [ ((modMask config, button1),                 dragWindow)
    , ((modMask config, button3),                 \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    ]

restart_xmonad do_optimize = do
    withWindowSet $ \ws -> io $ writeFile workspaceTempFile $ unlines $ W.tag <$> W.workspaces ws
    when (not do_optimize) $ io $ writeFile "/tmp/.xmonad-nooptimize" "no optimize"
    spawn "if xmonad --recompile; then xmonad --restart && notify-send -u low XMonad \"Restarted.\"; else notify-send -u critical XMonad \"Compilation failed.\"; fi"

kill_copy = let delete'' w = W.modify Nothing $ W.filter (/=w)
                in withWindowSet $ \ss ->
                    whenJust (W.peek ss) $ \w ->
                        when (W.member w $ delete'' w ss) $
                        windows $ delete'' w

workspace_bring = myGridSelectWorkspace myGridSelectConfig $ \x -> windows $ \ws ->
    case filter ((==x) . W.tag) $ W.hidden ws of
         targetWorkspace:_ -> foldl (\acc w -> copyWindow w (W.tag $ W.workspace $ W.current acc) acc) ws $ W.integrate' $ W.stack targetWorkspace
         _ -> ws

change_screen_config = gridselect myGridSelectConfig (map (\x -> (x,x))
    [ "Mirror"
    , "Laptop .. HDMI"
    , "HDMI .. Laptop"
    , "Laptop only"
    , "HDMI only"
    ]) >>= flip whenJust (\case
        "Laptop .. HDMI" -> spawn "mons -e right"
        "HDMI .. Laptop" -> spawn "mons -e left"
        "Mirror"         -> spawn "mons -m"
        "Laptop only"    -> spawn "mons -o"
        "HDMI only"      -> spawn "mons -s"
        s                -> spawn $ "notify-send XMonad 'unhandled case: \"" ++ s ++ "\"'")

change_layout_gridselect = gridselect myGridSelectConfig (map (\x -> (x, x))
    [ "Mosaic"
    , "Grid"
    , "ThreeColMid"
    , "Dishes"
    , "OneBig"
    , "Full"
    , "Dwindle"
    , "Mirror Dwindle"
    ]) >>= flip whenJust (sendMessage . JumpToLayout)
