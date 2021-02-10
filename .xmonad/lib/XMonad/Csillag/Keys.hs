{-# LANGUAGE LambdaCase #-}

module XMonad.Csillag.Keys
  ( myKeys
  , myMouseBindings
  )
where

import qualified Data.Map as Map
import Control.Monad

import XMonad.Csillag.CommonActions
import XMonad.Csillag.Scratchpads
import XMonad.Csillag.Layouts (MAGNIFIER(..), WINDOWTITLES(..))
import XMonad.Csillag.Consts
import XMonad.Csillag.Externals

import XMonad hiding (config, keys)
import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Layout.LayoutCombinators

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


myKeys = flip mkNamedKeymap $
    -- Core:
    [ ("M-r M-r",   addName "Restart XMonad (with --fast)" $ restart_xmonad False)
    , ("M-r M-S-r", addName "Restart XMonad (optimized)"   $ restart_xmonad True)
    , ("M-S-9",     addName "Kill the compositor"          $ spawn compositor_kill)
    , ("M-S-0",     addName "Start the compositor"         $ spawn compositor_spawn)
    , ("M-S-8",     addName "Restart the compositor"       $ spawn compositor_restart)
    , ("M-;",       addName "XMonad command prompt"        $ xmonadPrompt csillagPromptConfig)
    -- Directional keys
    , ("M-h",       addName "Focus window to the left"  $ windowGo L False)
    , ("M-j",       addName "Focus window below"        $ windowGo D False)
    , ("M-k",       addName "Focus window above"        $ windowGo U False)
    , ("M-l",       addName "Focus window to the right" $ windowGo R False)
    , ("M-S-h",     addName "Move window to the left"   $ windowSwap L False)
    , ("M-S-j",     addName "Move window down"          $ windowSwap D False)
    , ("M-S-k",     addName "Move window up"            $ windowSwap U False)
    , ("M-S-l",     addName "Move window to the right"  $ windowSwap R False)
    -- Stack Keys
    , ("M-m",       addName "Focus master window"           $ windows W.focusMaster)
    , ("M-S-m",     addName "Swap with master window"       $ windows W.swapMaster)
    , ("M-S-<Tab>", addName "Focus up on the stack"         $ windows W.focusUp)
    , ("M-<Tab>",   addName "Focus down on the stack"       $ windows W.focusDown)
    , ("M-,",       addName "Focus up on the stack"         $ windows W.focusUp)
    , ("M-.",       addName "Focus down on the stack"       $ windows W.focusDown)
    , ("M-S-,",     addName "Swap window up on the stack"   $ windows W.swapUp)
    , ("M-S-.",     addName "Swap window down on the stack" $ windows W.swapDown)
    -- Spawn Stuff
    , ("M-n M-n",   addName "Open shell prompt"         $ shellPrompt csillagPromptConfig)
    , ("M-n M-t",   addName "Spawn a terminal"          $ spawn term_spawn)
    , ("M-n M-S-t", addName "Spawn a terminal with SSH" $ sshPrompt csillagPromptConfig)
    , ("M-n M-f",   addName "Spawn a file manager"      $ spawn filemanager_spawn)
    , ("M-n M-v",   addName "Spawn an editor"           $ spawn texteditor_spawn)
    , ("M-n M-b",   addName "Spawn a browser"           $ spawn browser_spawn)
    , ("M-n M-S-b", addName "Spawn a private browser"   $ spawn browser_spawn_private)
    , ("M-n M-c",   addName "Spawn a camera view"       $ spawn camview_spawn)
    , ("M-n M-w",   addName "Spawn a notebook"          $ spawn "write_stylus")
    , ("M-n M-r",   addName "Spawn a calculator"        $ spawn calculator_spawn)
    -- Floating Windows
    , ("M-f M-t",   addName "Tile floating window"         $ withFocused $ windows . W.sink)
    , ("M-f M-s",   addName "Float window as a scratchpad" $ withFocused $ windows . flip W.float (W.RationalRect 0.05 0.05 0.9 0.9))
    -- Close Windows
    , ("M-x M-x",   addName "Close window"  $ kill1)
    , ("M-x M-d",   addName "Close program" $ kill)
    , ("M-x M-c",   addName "Close copy"    $ kill_copy)
    -- Screens (Xinerama)
    , ("M-S-s M-S-a",       addName "Focus on 1st screen"               $ screenWorkspace 0 >>= flip whenJust (windows . W.view))
    , ("M-S-s M-S-q",       addName "Focus on 2nd screen"               $ screenWorkspace 1 >>= flip whenJust (windows . W.view))
    , ("M-S-s M-S-s",       addName "Swap screens"                      $ screenSwap U True)
    , ("M-S-s M-S-c",       addName "Change screen setup"               $ change_screen_config)
    , ("M-S-s M-S-o M-S-k", addName "Set screen orientation to 'up'"    $ spawn $ set_screen_orientation "normal" 0)
    , ("M-S-s M-S-o M-S-j", addName "Set screen orientation to 'down'"  $ spawn $ set_screen_orientation "inverted" 0)
    , ("M-S-s M-S-o M-S-h", addName "Set screen orientation to 'left'"  $ spawn $ set_screen_orientation "left" 0)
    , ("M-S-s M-S-o M-S-l", addName "Set screen orientation to 'right'" $ spawn $ set_screen_orientation "right" 0)
    -- Workspaces
    , ("M-w M-r",       addName "Rename workspace"              $ inputPrompt csillagPromptConfig "Rename Workspace" ?+ renameWorkspaceByName)
    , ("M-w M-d",       addName "Delete workspace"              $ removeEmptyWorkspaceAfter (windows $ \ws -> flip W.view ws $ W.tag $ head $ filter ((/="NSP") . W.tag) $ W.hidden ws))
    , ("M-w M-g",       addName "Go to workspace"               $ myGridSelectWorkspace myGridSelectConfig $ windows . W.view)
    , ("M-w M-s",       addName "Send to workspace"             $ myGridSelectWorkspace myGridSelectConfig $ windows . W.shift)
    , ("M-w M-C-g",     addName "Send&Go to workspace"          $ myGridSelectWorkspace myGridSelectConfig $ \x -> windows (W.shift x) >> windows (W.view x))
    , ("M-w M-S-c",     addName "Send copy to workspace"        $ myGridSelectWorkspace myGridSelectConfig $ \x -> windows (copy x))
    , ("M-w M-c",       addName "Send&Go copy to workspace"     $ myGridSelectWorkspace myGridSelectConfig $ \x -> windows (copy x) >> windows (W.view x))
    , ("M-w M-b",       addName "Bring from workspace"          $ workspace_bring)
    , ("M-w M-n M-g",   addName "Go to new workspace"           $ inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (W.view wkname)))
    , ("M-w M-n M-s",   addName "Send to new workspace"         $ inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (W.shift wkname)))
    , ("M-w M-n M-C-g", addName "Send&Go to new workspace"      $ inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (W.shift wkname) >> windows (W.view wkname)))
    , ("M-w M-n M-S-c", addName "Send copy to new workspace"    $ inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (copy wkname)))
    , ("M-w M-n M-c",   addName "Send&Go copy to new workspace" $ inputPrompt csillagPromptConfig "New Workspace Name" ?+ (\wkname -> addHiddenWorkspace wkname >> windows (copy wkname) >> windows (W.view wkname)))
    , ("M-6",           addName "Switch with last workspace"    $ windows $ \ws -> flip W.view ws $ W.tag $ head $ filter ((/="NSP") . W.tag) $ W.hidden ws)
    -- Layouts
    , ("M-c M-<Space>", addName "Cycle to next layout"           $ sendMessage NextLayout)
    , ("M-c M-c",       addName "Change layout"                  $ change_layout_gridselect)
    , ("M-c M-l M-m",   addName "Set layout to 'Mosaic'"         $ sendMessage $ JumpToLayout "Mosaic")
    , ("M-c M-l M-g",   addName "Set layout to 'Grid'"           $ sendMessage $ JumpToLayout "Grid")
    , ("M-c M-l M-t",   addName "Set layout to 'ThreeColMid'"    $ sendMessage $ JumpToLayout "ThreeColMid")
    , ("M-c M-l M-d",   addName "Set layout to 'Dishes'"         $ sendMessage $ JumpToLayout "Dishes")
    , ("M-c M-l M-o",   addName "Set layout to 'OneBig'"         $ sendMessage $ JumpToLayout "OneBig")
    , ("M-c M-l M-f",   addName "Set layout to 'Full'"           $ sendMessage $ JumpToLayout "Full")
    , ("M-c M-l M-S-d", addName "Set layout to 'Dwindle'"        $ sendMessage $ JumpToLayout "Dwindle")
    , ("M-c M-l M-S-m", addName "Set layout to 'Mirror Dwindle'" $ sendMessage $ JumpToLayout "Mirror Dwindle")
    -- Layout Messages
    , ("M-[",          addName "Shrink master area"               $ sendMessage Shrink)
    , ("M-]",          addName "Expand master area"               $ sendMessage Expand)
    , ("M-S-[",        addName "Add one window to master pane"    $ sendMessage $ IncMasterN 1)
    , ("M-S-]",        addName "Take one window from master pane" $ sendMessage $ IncMasterN (-1))
    , ("M-<Return>",   addName "Toggle magnifier"                 $ sendMessage $ Toggle MAGNIFIER)
    , ("M-S-<Return>", addName "Toggle window titles"             $ sendMessage $ Toggle WINDOWTITLES)
    -- Scratchpads
    , ("M-s M-t",   addName "Toggle scratchpad 'sysmon'"        $ namedScratchpadAction myScratchpads "sysmon")
    , ("M-s M-b",   addName "Toggle scratchpad 'bluetooth'"     $ namedScratchpadAction myScratchpads "bluetooth")
    , ("M-s M-q",   addName "Toggle scratchpad 'terminal'"      $ namedScratchpadAction myScratchpads "terminal")
    , ("M-s M-a",   addName "Toggle scratchpad 'audio'"         $ namedScratchpadAction myScratchpads "audio")
    , ("M-s M-z",   addName "Toggle scratchpad 'advancedaudio'" $ namedScratchpadAction myScratchpads "advancedaudio")
    , ("M-s M-S-m", addName "Toggle scratchpad 'deezer'"        $ namedScratchpadAction myScratchpads "deezer")
    , ("M-s M-x",   addName "Toggle scratchpad 'todo'"          $ namedScratchpadAction myScratchpads "todo")
    , ("M-s M-s",   addName "Toggle scratchpad 'slack'"         $ namedScratchpadAction myScratchpads "slack")
    , ("M-s M-d",   addName "Toggle scratchpad 'discord'"       $ namedScratchpadAction myScratchpads "discord")
    , ("M-s M-w",   addName "Toggle scratchpad 'whatsapp'"      $ namedScratchpadAction myScratchpads "whatsapp")
    , ("M-s M-e",   addName "Toggle scratchpad 'mail'"          $ namedScratchpadAction myScratchpads "mail")
    , ("M-s M-c",   addName "Toggle scratchpad 'calendar'"      $ namedScratchpadAction myScratchpads "calendar")
    -- Passwords
    , ("M-p M-p",   addName "Get a password"             $ passPrompt csillagPromptConfig)
    , ("M-p M-g",   addName "Generate a random password" $ passGeneratePrompt csillagPromptConfig)
    , ("M-p M-n",   addName "Insert a new password"      $ passTypePrompt csillagPromptConfig)
    , ("M-p M-S-d", addName "Remove a password"          $ passRemovePrompt csillagPromptConfig)
    -- Screenshots
    , ("M-y M-s",   addName "Yank the whole screen"   $ spawn scrot_screen)
    , ("M-y M-w",   addName "Yank a window"           $ spawn scrot_window)
    , ("M-y M-f",   addName "Yank the current window" $ spawn scrot_thiswindow)
    -- Background
    , ("M-b M-r", addName "Set a random background"                        $ spawn "background-setter set")
    , ("M-b M-a", addName "Automatically set a random background every 1h" $ spawn "background-setter auto")
    , ("M-b M-b", addName "Automatically set a random background every 1h" $ spawn "background-setter choose")
    -- Devices
    , ("M-d M-t M-w M-k", addName "Map wacom tablet to current window, with orientation 'up'"   $ spawn "wacom-map up")
    , ("M-d M-t M-w M-j", addName "Map wacom tablet to current window, with orientation 'down'" $ spawn "wacom-map down")
    , ("M-d M-t M-w M-h", addName "Map wacom tablet to current window, with orientation 'cw'"   $ spawn "wacom-map cw")
    , ("M-d M-t M-w M-l", addName "Map wacom tablet to current window, with orientation 'ccw'"  $ spawn "wacom-map ccw")
    , ("M-d M-t M-s",     addName "Map wacom tablet to whole screen"                            $ spawn "wacom-map screen")
    -- System
    , ("M-q M-S-q",   addName "Quit XMonad"              $ quitWithWarning)
    , ("M-q M-S-s",   addName "Suspend"                  $ spawn "systemctl suspend")
    , ("M-q M-b",     addName "Blank the screen"         $ spawn "sleep 0.5; xset dpms force off")
    , ("M-q M-l",     addName "Lock"                     $ spawn "slock")
    , ("M-q M-d M-[", addName "Enable 'do not disturb'"  $ spawn "notify-send \"DUNST_COMMAND_PAUSE\" && touch /home/daniel/.dunst_paused")
    , ("M-q M-d M-]", addName "Disable 'do not disturb'" $ spawn "notify-send \"DUNST_COMMAND_RESUME\" && rm -f /home/daniel/.dunst_paused")
    , ("M-q M-a",     addName "Fix audio"                $ spawn "fix-audio" >> spawnOSD "A")
    , ("M-q M-=",     addName "Toggle XMobar"            $ spawn "toggle_xmobar")
    , ("M-q M-k",     addName "Toggle Screenkey"         $ spawn ".local/scripts/screenkey_toggle.sh")
    -- Function Keys
    , ("<XF86MonBrightnessUp>",   addName "Raise brightness"  $ spawn "lux -a 1%" >> spawnOSD brightnessUpIcon)
    , ("<XF86MonBrightnessDown>", addName "Lower brightness"  $ spawn "lux -s 1%" >> spawnOSD brightnessDownIcon)
    , ("<XF86AudioRaiseVolume>",  addName "Raise volume"      $ spawnOSD volumeUpIcon >> spawn "amixer sset Master 2%+" >> spawn ("sleep 0.1; paplay " ++ volumeChangeSound))
    , ("<XF86AudioLowerVolume>",  addName "Lower volume"      $ spawnOSD volumeDownIcon >> spawn "amixer sset Master 2%-" >> spawn ("sleep 0.1; paplay " ++ volumeChangeSound))
    , ("<XF86AudioMute>",         addName "Toggle mute"       $ spawnOSD volumeMuteIcon >> spawn "amixer sset Master toggle" >> spawn ("sleep 0.1; paplay " ++ volumeChangeSound))
    , ("M-C-v",                   addName "Play test sound"   $ spawnOSD volumePlayIcon >> spawn ("paplay " ++ volumeChangeSound))
    , ("M-\\",                    addName "Toggle play/pause" $ spawn "mmc toggle")
    , ("M-S-\\",                  addName "Go to next track"  $ spawn "mmc next")
    ]

myMouseBindings config = Map.fromList
    [ ((modMask config, button1), dragWindow)
    , ((modMask config, button3), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
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
        "Laptop .. HDMI" -> spawn "mons -e top"
        "HDMI .. Laptop" -> spawn "mons -e bottom"
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
