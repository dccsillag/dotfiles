{-# LANGUAGE LambdaCase #-}

module XMonad.Csillag.Keys
  ( myKeys
  , myMouseBindings
  , systemFunctionKeys
  )
where

import Data.List
import qualified Data.Map as Map
import Control.Monad

import XMonad.Csillag.Internal.KeyBindings
import XMonad.Csillag.CommonActions
import XMonad.Csillag.Scratchpads
import XMonad.Csillag.Layouts (windowGap, MAGNIFIER(..))
import XMonad.Csillag.Consts
import XMonad.Csillag.Externals

import XMonad hiding (config, keys)
import qualified XMonad.StackSet as W
import Graphics.X11.ExtraTypes.XF86
-- import XMonad.Util.WorkspaceCompare
-- import XMonad.Actions.Volume (getVolume)
-- import XMonad.Actions.TagWindows

import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Spacing

import XMonad.Prompt ( )
-- import XMonad.Prompt.Shell
import XMonad.Prompt.Zsh
import XMonad.Prompt.Input
import XMonad.Prompt.Pass
import XMonad.Prompt.XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Actions.Navigation2D
import XMonad.Actions.CopyWindow
-- import XMonad.Actions.Volume
import XMonad.Actions.DynamicWorkspaces
-- import XMonad.Actions.WorkspaceNames
-- import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Util.NamedScratchpad
import XMonad.Actions.TiledWindowDragging
import XMonad.Layout.MultiToggle


myKeys config =
    [ KeyHeading "Core"
    , KeyBinding { keybinding_description = "Restart XMonad"
                 , keybinding_mask        = shiftMask
                 , keybinding_key         = xK_q
                 , keybinding_humankey    = [ShiftKey, AlphaKey 'q']
                 , keybinding_action      = do
                     withWindowSet $ \ws -> io $ writeFile workspaceTempFile $ unlines $ W.tag <$> W.workspaces ws
                     spawn "if xmonad --recompile; then xmonad --restart && notify-send -u low XMonad \"Restarted.\"; else notify-send -u critical XMonad \"Compilation failed.\"; fi"
                 }
    , KeyBinding { keybinding_description = "Kill the Compositor"
                 , keybinding_mask        = shiftMask
                 , keybinding_key         = xK_9
                 , keybinding_humankey    = [AlphaKey '(']
                 , keybinding_action      = spawn compositor_kill
                 }
    , KeyBinding { keybinding_description = "Start the Compositor"
                 , keybinding_mask        = shiftMask
                 , keybinding_key         = xK_0
                 , keybinding_humankey    = [AlphaKey ')']
                 , keybinding_action      = spawn compositor_spawn
                 }
    , KeyBinding { keybinding_description = "Restart the Compositor"
                 , keybinding_mask        = shiftMask
                 , keybinding_key         = xK_8
                 , keybinding_humankey    = [AlphaKey '*']
                 , keybinding_action      = spawn compositor_restart
                 }
    , KeyBinding { keybinding_description = "XMonad Command Prompt"
                 , keybinding_mask        = 0
                 , keybinding_key         = xK_semicolon
                 , keybinding_humankey    = [AlphaKey ';']
                 , keybinding_action      = xmonadPrompt csillagPromptConfig
                 }
    , KeyHeading "Directional Keys"
    , KeyBinding { keybinding_description = "Focus window to the left"
                 , keybinding_mask        = 0
                 , keybinding_key         = xK_h
                 , keybinding_humankey    = [AlphaKey 'h']
                 , keybinding_action      = windowGo L False
                 }
    , KeyBinding { keybinding_description = "Focus window below"
                 , keybinding_mask        = 0
                 , keybinding_key         = xK_j
                 , keybinding_humankey    = [AlphaKey 'j']
                 , keybinding_action      = windowGo D False
                 }
    , KeyBinding { keybinding_description = "Focus window above"
                 , keybinding_mask        = 0
                 , keybinding_key         = xK_k
                 , keybinding_humankey    = [AlphaKey 'k']
                 , keybinding_action      = windowGo U False
                 }
    , KeyBinding { keybinding_description = "Focus window to the right"
                 , keybinding_mask        = 0
                 , keybinding_key         = xK_l
                 , keybinding_humankey    = [AlphaKey 'l']
                 , keybinding_action      = windowGo R False
                 }
    , KeyBinding { keybinding_description = "Move window to the left"
                 , keybinding_mask        = shiftMask
                 , keybinding_key         = xK_h
                 , keybinding_humankey    = [ShiftKey, AlphaKey 'h']
                 , keybinding_action      = windowSwap L False
                 }
    , KeyBinding { keybinding_description = "Move window down"
                 , keybinding_mask        = shiftMask
                 , keybinding_key         = xK_j
                 , keybinding_humankey    = [ShiftKey, AlphaKey 'j']
                 , keybinding_action      = windowSwap D False
                 }
    , KeyBinding { keybinding_description = "Move window up"
                 , keybinding_mask        = shiftMask
                 , keybinding_key         = xK_k
                 , keybinding_humankey    = [ShiftKey, AlphaKey 'k']
                 , keybinding_action      = windowSwap U False
                 }
    , KeyBinding { keybinding_description = "Move window to the right"
                 , keybinding_mask        = shiftMask
                 , keybinding_key         = xK_l
                 , keybinding_humankey    = [ShiftKey, AlphaKey 'l']
                 , keybinding_action      = windowSwap R False
                 }
    , KeyHeading "Stack Keys"
    , KeyBinding { keybinding_description = "Focus master window"
                 , keybinding_mask        = 0
                 , keybinding_key         = xK_m
                 , keybinding_humankey    = [AlphaKey 'm']
                 , keybinding_action      = windows W.focusMaster
                 }
    , KeyBinding { keybinding_description = "Swap with master window"
                 , keybinding_mask        = shiftMask
                 , keybinding_key         = xK_m
                 , keybinding_humankey    = [ShiftKey, AlphaKey 'm']
                 , keybinding_action      = windows W.swapMaster
                 }
    , KeyBinding { keybinding_description = "Focus up on the stack"
                 , keybinding_mask        = 0
                 , keybinding_key         = xK_comma
                 , keybinding_humankey    = [AlphaKey ',']
                 , keybinding_action      = windows W.focusUp
                 }
    , KeyBinding { keybinding_description = "Focus up on the stack"
                 , keybinding_mask        = shiftMask
                 , keybinding_key         = xK_Tab
                 , keybinding_humankey    = [ShiftKey, TabKey]
                 , keybinding_action      = windows W.focusUp
                 }
    , KeyBinding { keybinding_description = "Focus down on the stack"
                 , keybinding_mask        = 0
                 , keybinding_key         = xK_period
                 , keybinding_humankey    = [AlphaKey '.']
                 , keybinding_action      = windows W.focusDown
                 }
    , KeyBinding { keybinding_description = "Focus down on the stack"
                 , keybinding_mask        = 0
                 , keybinding_key         = xK_Tab
                 , keybinding_humankey    = [TabKey]
                 , keybinding_action      = windows W.focusDown
                 }
    , KeyBinding { keybinding_description = "Swap window up on the stack"
                 , keybinding_mask        = shiftMask
                 , keybinding_key         = xK_comma
                 , keybinding_humankey    = [AlphaKey '<']
                 , keybinding_action      = windows W.swapUp
                 }
    , KeyBinding { keybinding_description = "Swap window down on the stack"
                 , keybinding_mask        = shiftMask
                 , keybinding_key         = xK_period
                 , keybinding_humankey    = [AlphaKey '>']
                 , keybinding_action      = windows W.swapDown
                 }
    , KeyHeading "Spawn Stuff"
    , KeySubmap { keysubmap_description = "Spawn stuff"
                , keysubmap_mask = 0
                , keysubmap_key = xK_n
                , keysubmap_humankey = [AlphaKey 'n']
                , keysubmap_submaps =
                    [ KeyBinding { keybinding_description = "Executable Launcher Prompt"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_n
                                 , keybinding_humankey    = [AlphaKey 'n']
                                 -- , keybinding_action      = shellPrompt csillagPromptConfig
                                 , keybinding_action      = zshPrompt csillagPromptConfig "/home/daniel/.local/scripts/capture.zsh"
                                 }
                    , KeyBinding { keybinding_description = "Launch a terminal"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_t
                                 , keybinding_humankey    = [AlphaKey 't']
                                 , keybinding_action      = spawnNohup term_spawn
                                 }
                    , KeyBinding { keybinding_description = "Launch a file manager"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_f
                                 , keybinding_humankey    = [AlphaKey 'f']
                                 , keybinding_action      = spawnNohup $ filemanager_spawn
                                 }
                    , KeyBinding { keybinding_description = "Launch Vim"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_v
                                 , keybinding_humankey    = [AlphaKey 'v']
                                 , keybinding_action      = spawnNohup $ texteditor_spawn
                                 }
                    , KeyBinding { keybinding_description = "Launch Browser"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_b
                                 , keybinding_humankey    = [AlphaKey 'b']
                                 -- , keybinding_action      = spawnNohup "brave --new-window"
                                 -- , keybinding_action      = spawnNohup "qutebrowser"
                                 , keybinding_action      = spawnNohup browser_spawn
                                 }
                    , KeyBinding { keybinding_description = "Launch Browser in Incognito Mode"
                                 , keybinding_mask        = shiftMask
                                 , keybinding_key         = xK_b
                                 , keybinding_humankey    = [ShiftKey, AlphaKey 'b']
                                 -- TODO: switch to qutebrowser
                                 , keybinding_action      = spawnNohup browser_spawn_private
                                 }
                    , KeyBinding { keybinding_description = "Launch MPV with Camera"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_c
                                 , keybinding_humankey    = [AlphaKey 'c']
                                 -- , keybinding_action      = spawnNohup "mpv av://v4l2:/dev/video0"
                                 , keybinding_action      = spawnNohup camview_spawn
                                 }
                    ]
                }
    -- , KeyBinding { keybinding_description = "Open file search"
    --              , keybinding_mask        = 0
    --              , keybinding_key         = xK_slash
    --              , keybinding_humankey    = [AlphaKey '/']
    --              , keybinding_action      = fileManager "/home/daniel/" csillagPromptConfig
    --              }
    -- , KeyBinding { keybinding_description = "Search BibTeX"
    --              , keybinding_mask        = shiftMask
    --              , keybinding_key         = xK_backslash
    --              , keybinding_humankey    = [AlphaKey '|']
    --              , keybinding_action      = do
    --                  bibs <- io getBibs
    --                  let openPaper bibId = case filter ((==bibId) . BibTeX.identifier) bibs of
    --                                             (BibTeX.Cons _ _ fields):_ -> case filter ((=="url") . fst) fields of
    --                                                                           ("url", url):_ -> spawnNohup $ "xdg-open '" ++ url ++ "'"
    --                                                                           _ -> spawn "notify-send -u critical 'Failed to open paper' 'URL not found'"
    --                                             _ -> spawn "notify-send -u critical 'Failed to open paper' 'BibTeX ID not found'"
    --                  let bibIds = BibTeX.identifier <$> bibs
    --                  inputPromptWithCompl csillagPromptConfig "BibTeX" (mkComplFunFromList bibIds) ?+ openPaper
    --              }
    , KeyHeading "Floating Windows"
    , KeySubmap { keysubmap_description = "Manage floating windows"
                , keysubmap_mask = 0
                , keysubmap_key = xK_f
                , keysubmap_humankey = [AlphaKey 'f']
                , keysubmap_submaps =
                    [ KeyBinding { keybinding_description = "Tile floating window"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_t
                                 , keybinding_humankey    = [AlphaKey 't']
                                 , keybinding_action      = withFocused $ windows . W.sink
                                 }
                    , KeyBinding { keybinding_description = "Float window as a scratchpad"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_s
                                 , keybinding_humankey    = [AlphaKey 's']
                                 , keybinding_action      = withFocused $ windows . flip W.float (W.RationalRect 0.05 0.05 0.9 0.9)
                                 }
                    ]
                }
    , KeyHeading "Close Windows"
    , KeySubmap { keysubmap_description = "Close window"
                , keysubmap_mask = 0
                , keysubmap_key = xK_x
                , keysubmap_humankey = [AlphaKey 'x']
                , keysubmap_submaps =
                    [ KeyBinding { keybinding_description = "Close window"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_x
                                 , keybinding_humankey    = [ AlphaKey 'x' ]
                                 , keybinding_action      = kill1
                                 }
                    , KeyBinding { keybinding_description = "Close program"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_d
                                 , keybinding_humankey    = [AlphaKey 'd']
                                 , keybinding_action      = kill
                                 }
                    , KeyBinding { keybinding_description = "Close copy"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_c
                                 , keybinding_humankey    = [AlphaKey 'c']
                                 , keybinding_action      =
                                     let delete'' w = W.modify Nothing $ W.filter (/=w)
                                     in withWindowSet $ \ss ->
                                         whenJust (W.peek ss) $ \w ->
                                             when (W.member w $ delete'' w ss) $
                                                windows $ delete'' w
                                 }
                    ]
                }
    , KeyHeading "Screens (Xinerama)"
    , KeySubmap { keysubmap_description = "Manage Screens"
                , keysubmap_mask = shiftMask
                , keysubmap_key = xK_s
                , keysubmap_humankey = [ShiftKey, AlphaKey 's']
                , keysubmap_submaps =
                    [ KeyBinding { keybinding_description = "Focus 1ˢᵗ screen"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_a
                                 , keybinding_humankey    = [AlphaKey 'a']
                                 , keybinding_action      = screenWorkspace 0 >>= flip whenJust (windows . W.view)
                                 }
                    , KeyBinding { keybinding_description = "Focus 2ⁿᵈ screen"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_b
                                 , keybinding_humankey    = [AlphaKey 'b']
                                 , keybinding_action      = screenWorkspace 1 >>= flip whenJust (windows . W.view)
                                 }
                    , KeyBinding { keybinding_description = "Send to 1ˢᵗ screen"
                                 , keybinding_mask        = shiftMask
                                 , keybinding_key         = xK_a
                                 , keybinding_humankey    = [ShiftKey, AlphaKey 'a']
                                 , keybinding_action      = screenWorkspace 0 >>= flip whenJust (windows . W.shift)
                                 }
                    , KeyBinding { keybinding_description = "Send to 2ⁿᵈ screen"
                                 , keybinding_mask        = shiftMask
                                 , keybinding_key         = xK_b
                                 , keybinding_humankey    = [ShiftKey, AlphaKey 'b']
                                 , keybinding_action      = screenWorkspace 1 >>= flip whenJust (windows . W.shift)
                                 }
                    , KeyBinding { keybinding_description = "Swap screens"
                                 , keybinding_mask        = shiftMask
                                 , keybinding_key         = xK_s
                                 , keybinding_humankey    = [ShiftKey, AlphaKey 's']
                                 , keybinding_action      = screenSwap R True
                                 }
                    , KeyBinding { keybinding_description = "Change screen setup"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_c
                                 , keybinding_humankey    = [AlphaKey 'c']
                                 , keybinding_action      =
                                     inputPromptWithCompl csillagPromptConfig "Screen Config"
                                         (return . myPromptCompletion' [ "Laptop .. HDMI"
                                                                       , "HDMI .. Laptop"
                                                                       , "Mirror"
                                                                       , "Laptop only"
                                                                       , "HDMI only"
                                                                       ]) ?+ (\case
                                              "Laptop .. HDMI" -> spawn "mons -e right"
                                              "HDMI .. Laptop" -> spawn "mons -e left"
                                              "Mirror"         -> spawn "mons -m"
                                              "Laptop only"    -> spawn "mons -o"
                                              "HDMI only"      -> spawn "mons -s"
                                              _                -> return ())
                                 }
                    ]
                }
    , KeyHeading "Workspaces"
    , KeySubmap { keysubmap_description = "Manage Workspaces"
                , keysubmap_mask = 0
                , keysubmap_key = xK_w
                , keysubmap_humankey = [AlphaKey 'w']
                , keysubmap_submaps =
                    [ KeyBinding { keybinding_description = "Rename Workspace"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_r
                                 , keybinding_humankey    = [AlphaKey 'r']
                                 , keybinding_action      = renameWorkspace csillagPromptConfig
                                 }
                    , KeyBinding { keybinding_description = "Delete Workspace"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_d
                                 , keybinding_humankey    = [AlphaKey 'd']
                                 , keybinding_action      = removeEmptyWorkspace
                                 }
                    , KeyBinding { keybinding_description = "Go to workspace"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_g
                                 , keybinding_humankey    = [AlphaKey 'g']
                                 -- , keybinding_action      = selectWorkspace csillagPromptConfig
                                 , keybinding_action      = myGridSelectWorkspace myGridSelectConfig $ windows . W.view
                                 }
                    , KeyBinding { keybinding_description = "Send to workspace"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_s
                                 , keybinding_humankey    = [AlphaKey 's']
                                 -- , keybinding_action      = withWorkspace csillagPromptConfig $ windows . W.shift
                                 , keybinding_action      = myGridSelectWorkspace myGridSelectConfig $ windows . W.shift
                                 }
                    , KeyBinding { keybinding_description = "Send&Go to workspace"
                                 , keybinding_mask        = controlMask
                                 , keybinding_key         = xK_g
                                 , keybinding_humankey    = [ControlKey, AlphaKey 'g']
                                 -- , keybinding_action      = withWorkspace csillagPromptConfig $ \x -> windows (W.shift x) >> windows (W.view x)
                                 , keybinding_action      = myGridSelectWorkspace myGridSelectConfig $ \x -> windows (W.shift x) >> windows (W.view x)
                                 }
                    , KeyBinding { keybinding_description = "Send copy to workspace"
                                 , keybinding_mask        = shiftMask
                                 , keybinding_key         = xK_c
                                 , keybinding_humankey    = [ShiftKey, AlphaKey 'c']
                                 -- , keybinding_action      = withWorkspace csillagPromptConfig $ windows . copy
                                 , keybinding_action      = myGridSelectWorkspace myGridSelectConfig $ \x -> windows (copy x)
                                 }
                    , KeyBinding { keybinding_description = "Send&Go copy to workspace"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_c
                                 , keybinding_humankey    = [AlphaKey 'c']
                                 -- , keybinding_action      = withWorkspace csillagPromptConfig $ \x -> windows (copy x) >> windows (W.view x)
                                 , keybinding_action      = myGridSelectWorkspace myGridSelectConfig $ \x -> windows (copy x) >> windows (W.view x)
                                 }
                    , KeyBinding { keybinding_description = "Bring from workspace"
                                 , keybinding_mask = 0
                                 , keybinding_key = xK_b
                                 , keybinding_humankey = [AlphaKey 'b']
                                 , keybinding_action = myGridSelectWorkspace myGridSelectConfig $ \x -> windows $ \ws ->
                                     case filter ((==x) . W.tag) $ W.hidden ws of
                                          targetWorkspace:_ -> foldl (\acc w -> copyWindow w (W.tag $ W.workspace $ W.current acc) acc) ws $ W.integrate' $ W.stack targetWorkspace
                                          _ -> ws
                                 }
                    , KeySubmap { keysubmap_description = "New workspace.."
                                 , keysubmap_mask = 0
                                 , keysubmap_key = xK_n
                                 , keysubmap_humankey = [AlphaKey 'n']
                                 , keysubmap_submaps =
                                     [ KeyBinding { keybinding_description = "Go to new workspace"
                                                  , keybinding_mask        = 0
                                                  , keybinding_key         = xK_g
                                                  , keybinding_humankey    = [AlphaKey 'g']
                                                  -- , keybinding_action      = withWindowSet $ \ws -> let wks = getEmptyWorkspace ws in windows $ W.view $ W.tag wks
                                                  , keybinding_action      = inputPrompt csillagPromptConfig "New Workspace Name" ?+ \wkname -> do
                                                      addHiddenWorkspace wkname
                                                      windows $ W.view wkname
                                                  }
                                     , KeyBinding { keybinding_description = "Send to new workspace"
                                                  , keybinding_mask        = 0
                                                  , keybinding_key         = xK_s
                                                  , keybinding_humankey    = [AlphaKey 's']
                                                  -- , keybinding_action      = withWindowSet $ \ws -> let wks = getEmptyWorkspace ws in windows $ W.shift $ W.tag wks
                                                  , keybinding_action      = inputPrompt csillagPromptConfig "New Workspace Name" ?+ \wkname -> do
                                                      addHiddenWorkspace wkname
                                                      windows $ W.shift wkname
                                                      -- flip toNthWorkspace 0 $ windows . W.shift
                                                  }
                                     , KeyBinding { keybinding_description = "Send&Go to new workspace"
                                                  , keybinding_mask        = controlMask
                                                  , keybinding_key         = xK_g
                                                  , keybinding_humankey    = [ControlKey, AlphaKey 'g']
                                                  -- , keybinding_action      = withWindowSet $ \ws ->
                                                  --     let wks = getEmptyWorkspace ws
                                                  --         in windows (W.shift $ W.tag wks) >> windows (W.view $ W.tag wks)
                                                  , keybinding_action      = inputPrompt csillagPromptConfig "New Workspace Name" ?+ \wkname -> do
                                                      addHiddenWorkspace wkname
                                                      windows $ W.shift wkname
                                                      windows $ W.view wkname
                                                      -- flip toNthWorkspace 0 $ \x -> windows (W.shift x) >> windows (W.view x)
                                                  }
                                     , KeyBinding { keybinding_description = "Send copy to new workspace"
                                                  , keybinding_mask        = shiftMask
                                                  , keybinding_key         = xK_c
                                                  , keybinding_humankey    = [AlphaKey 'c']
                                                  , keybinding_action      = inputPrompt csillagPromptConfig "New Workspace Name" ?+ \wkname -> do
                                                      addHiddenWorkspace wkname
                                                      windows $ copy wkname
                                                      -- flip toNthWorkspace 0 $ \x -> windows (copy x) >> windows (W.view x)
                                                  -- , keybinding_action      = findWorkspace getSortByIndex Next EmptyWS 1 >>= (\x -> windows (copy x) >> windows (W.view x))
                                                  }
                                     , KeyBinding { keybinding_description = "Send&Go copy to new workspace"
                                                  , keybinding_mask        = 0
                                                  , keybinding_key         = xK_c
                                                  , keybinding_humankey    = [ShiftKey, AlphaKey 'c']
                                                  , keybinding_action      = inputPrompt csillagPromptConfig "New Workspace Name" ?+ \wkname -> do
                                                      addHiddenWorkspace wkname
                                                      windows $ copy wkname
                                                      windows $ W.view wkname
                                                      -- flip toNthWorkspace 0 $ \x -> windows (copy x) >> windows (W.view x)
                                                  -- , keybinding_action      = findWorkspace getSortByIndex Next EmptyWS 1 >>= (\x -> windows (copy x) >> windows (W.view x))
                                                  }
                                     ]
                                }
                    ]
                }
    , KeyHeading "Layouts"
    , KeySubmap { keysubmap_description = "Change Layout"
                , keysubmap_mask = 0
                , keysubmap_key = xK_c
                , keysubmap_humankey = [AlphaKey 'c']
                , keysubmap_submaps =
                    [ KeyBinding { keybinding_description = "Cycle to next layout"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_space
                                 , keybinding_humankey    = [ShiftKey, SpaceKey]
                                 , keybinding_action      = sendMessage NextLayout
                                 }
                    , KeyBinding { keybinding_description = "Change Layout"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_c
                                 , keybinding_humankey    = [AlphaKey 'c']
                                 , keybinding_action      = gridselect myGridSelectConfig (map (\x -> (x, x))
                                     [ "Dwindle"
                                     , "Mirror Dwindle"
                                     , "Grid"
                                     , "Full"
                                     , "OneBig"
                                     , "ThreeColMid"
                                     , "Mosaic"
                                     ]) >>= flip whenJust (sendMessage . JumpToLayout)
                                 }
                    , KeySubmap { keysubmap_description = "Set layout"
                                , keysubmap_mask = 0
                                , keysubmap_key = xK_l
                                , keysubmap_humankey = [AlphaKey 'l']
                                , keysubmap_submaps =
                                    [ KeyBinding { keybinding_description = "Set Layout to 'Dwindle'"
                                                 , keybinding_mask        = 0
                                                 , keybinding_key         = xK_d
                                                 , keybinding_humankey    = [AlphaKey 'd']
                                                 , keybinding_action      = sendMessage $ JumpToLayout "Dwindle"
                                                 }
                                    , KeyBinding { keybinding_description = "Set Layout to 'Mirror Dwindle'"
                                                 , keybinding_mask        = 0
                                                 , keybinding_key         = xK_m
                                                 , keybinding_humankey    = [AlphaKey 'm']
                                                 , keybinding_action = sendMessage $ JumpToLayout "Mirror Dwindle"
                                                 }
                                    , KeyBinding { keybinding_description = "Set Layout to 'Grid'"
                                                 , keybinding_mask        = 0
                                                 , keybinding_key         = xK_g
                                                 , keybinding_humankey    = [AlphaKey 'g']
                                                 , keybinding_action      = sendMessage $ JumpToLayout "Grid"
                                                 }
                                    , KeyBinding { keybinding_description = "Set Layout to 'Full'"
                                                 , keybinding_mask        = 0
                                                 , keybinding_key         = xK_f
                                                 , keybinding_humankey    = [AlphaKey 'f']
                                                 , keybinding_action      = sendMessage $ JumpToLayout "Full"
                                                 }
                                    , KeyBinding { keybinding_description = "Set Layout to 'OneBig'"
                                                 , keybinding_mask        = 0
                                                 , keybinding_key         = xK_o
                                                 , keybinding_humankey    = [AlphaKey 'o']
                                                 , keybinding_action      = sendMessage $ JumpToLayout "OneBig"
                                                 }
                                    , KeyBinding { keybinding_description = "Set Layout to 'ThreeColMid'"
                                                 , keybinding_mask        = 0
                                                 , keybinding_key         = xK_t
                                                 , keybinding_humankey    = [AlphaKey 't']
                                                 , keybinding_action = sendMessage $ JumpToLayout "ThreeColMid"
                                                 }
                                    , KeyBinding { keybinding_description = "Set Layout to 'Mosaic'"
                                                 , keybinding_mask        = shiftMask
                                                 , keybinding_key         = xK_m
                                                 , keybinding_humankey    = [ShiftKey, AlphaKey 'm']
                                                 , keybinding_action = sendMessage $ JumpToLayout "Mosaic"
                                                 }
                                    ]
                                }
                    ]
                }
    , KeyHeading "Layout Messages"
    -- , KeyBinding { keybinding_description = "Toggle struts"
    --              , keybinding_mask        = shiftMask
    --              , keybinding_key         = xK_backslash
    --              , keybinding_humankey    = [ShiftKey, AlphaKey '\\']
    --              , keybinding_action      = spawn "polybar-msg cmd toggle" >> sendMessage ToggleStruts
    --              }
    , KeyBinding { keybinding_description = "Shrink master area"
                 , keybinding_mask        = 0
                 , keybinding_key         = xK_bracketleft
                 , keybinding_humankey    = [AlphaKey '[']
                 , keybinding_action      = sendMessage Shrink
                 }
    , KeyBinding { keybinding_description = "Expand master area"
                 , keybinding_mask        = 0
                 , keybinding_key         = xK_bracketright
                 , keybinding_humankey    = [AlphaKey ']']
                 , keybinding_action      = sendMessage Expand
                 }
    , KeyBinding { keybinding_description = "Increase window spacing"
                 , keybinding_mask = controlMask
                 , keybinding_key = xK_plus
                 , keybinding_humankey = [ControlKey, AlphaKey '+']
                 , keybinding_action = sendMessage $ ModifyWindowBorder $
                     \(Border a b c d) -> Border (succ a) (succ b) (succ c) (succ d)
                 }
    , KeyBinding { keybinding_description = "Decrease window spacing"
                 , keybinding_mask = controlMask
                 , keybinding_key = xK_minus
                 , keybinding_humankey = [ControlKey, AlphaKey '-']
                 , keybinding_action = sendMessage $ ModifyWindowBorder $
                     \x@(Border a b c d) ->
                         if all (>0) [a, b, c, d]
                             then Border (pred a) (pred b) (pred c) (pred d)
                             else x
                 }
    , KeyBinding { keybinding_description = "Reset window spacing"
                 , keybinding_mask = controlMask
                 , keybinding_key = xK_0
                 , keybinding_humankey = [ControlKey, AlphaKey '0']
                 , keybinding_action = sendMessage $ ModifyWindowBorder $
                     const $ Border windowGap windowGap windowGap windowGap
                 }
    , KeyBinding { keybinding_description = "Toggle manifier"
                 , keybinding_mask = 0
                 , keybinding_key = xK_Return
                 , keybinding_humankey = [ReturnKey]
                 , keybinding_action = sendMessage $ Toggle MAGNIFIER
                 }
    , KeyHeading "Scratchpads"
    , KeySubmap { keysubmap_description = "Show/Hide Scratchpads"
                , keysubmap_mask = 0
                , keysubmap_key = xK_s
                , keysubmap_humankey = [AlphaKey 's']
                , keysubmap_submaps =
                    [ KeyBinding { keybinding_description = "Toggle Scratchpad 'sysmon'"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_t
                                 , keybinding_humankey    = [AlphaKey 't']
                                 , keybinding_action      = namedScratchpadAction myScratchpads "sysmon"
                                 }
                    , KeyBinding { keybinding_description = "Toggle Scratchpad 'bluetooth'"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_b
                                 , keybinding_humankey    = [AlphaKey 'b']
                                 , keybinding_action      = namedScratchpadAction myScratchpads "bluetooth"
                                 }
                    , KeyBinding { keybinding_description = "Toggle Scratchpad 'terminal'"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_q
                                 , keybinding_humankey    = [AlphaKey 'q']
                                 , keybinding_action      = namedScratchpadAction myScratchpads "terminal"
                                 }
                    , KeyBinding { keybinding_description = "Toggle Scratchpad 'audio'"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_a
                                 , keybinding_humankey    = [AlphaKey 'a']
                                 , keybinding_action = namedScratchpadAction myScratchpads "audio"
                                 }
                    , KeyBinding { keybinding_description = "Toggle Scratchpad 'advancedaudio'"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_z
                                 , keybinding_humankey    = [AlphaKey 'z']
                                 , keybinding_action = namedScratchpadAction myScratchpads "advancedaudio"
                                 }
                    , KeyBinding { keybinding_description = "Toggle Scratchpad 'music'"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_m
                                 , keybinding_humankey    = [AlphaKey 'm']
                                 , keybinding_action      = namedScratchpadAction myScratchpads "music"
                                 }
                    , KeyBinding { keybinding_description = "Toggle Scratchpad 'deezer'"
                                 , keybinding_mask        = shiftMask
                                 , keybinding_key         = xK_m
                                 , keybinding_humankey    = [ShiftKey, AlphaKey 'm']
                                 , keybinding_action      = namedScratchpadAction myScratchpads "deezer"
                                 }
                    , KeyBinding { keybinding_description = "Toggle Scratchpad 'todo'"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_x
                                 , keybinding_humankey    = [AlphaKey 'x']
                                 , keybinding_action = namedScratchpadAction myScratchpads "todo"
                                 }
                    , KeyBinding { keybinding_description = "Toggle Scratchpad 'slack'"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_s
                                 , keybinding_humankey    = [AlphaKey 's']
                                 , keybinding_action = namedScratchpadAction myScratchpads "slack"
                                 }
                    , KeyBinding { keybinding_description = "Toggle Scratchpad 'discord'"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_d
                                 , keybinding_humankey    = [AlphaKey 'd']
                                 , keybinding_action      = namedScratchpadAction myScratchpads "discord"
                                 }
                    , KeyBinding { keybinding_description = "Toggle Scratchpad 'whatsapp'"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_w
                                 , keybinding_humankey    = [AlphaKey 'w']
                                 , keybinding_action      = namedScratchpadAction myScratchpads "whatsapp"
                                 }
                    , KeyBinding { keybinding_description = "Toggle Scratchpad 'mail'"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_e
                                 , keybinding_humankey    = [AlphaKey 'e']
                                 , keybinding_action = namedScratchpadAction myScratchpads "mail"
                                 }
                    , KeyBinding { keybinding_description = "Toggle Scratchpad 'calendar'"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_f
                                 , keybinding_humankey    = [AlphaKey 'f']
                                 , keybinding_action = namedScratchpadAction myScratchpads "calendar"
                                 }
                    ]
                }
    , KeyHeading "Passwords"
    , KeySubmap { keysubmap_description = "Manage passwords"
                , keysubmap_mask = 0
                , keysubmap_key = xK_p
                , keysubmap_humankey = [AlphaKey 'p']
                , keysubmap_submaps =
                    [ KeyBinding { keybinding_description = "Get a password"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_p
                                 , keybinding_humankey    = [AlphaKey 'p']
                                 , keybinding_action      = passPrompt csillagPromptConfig
                                 }
                    , KeyBinding { keybinding_description = "Generate a random password"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_g
                                 , keybinding_humankey    = [AlphaKey 'g']
                                 , keybinding_action      = passGeneratePrompt csillagPromptConfig
                                 }
                    , KeyBinding { keybinding_description = "Insert a new password"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_n
                                 , keybinding_humankey    = [AlphaKey 'n']
                                 , keybinding_action      = passTypePrompt csillagPromptConfig
                                 }
                    , KeyBinding { keybinding_description = "Remove a password"
                                 , keybinding_mask        = shiftMask
                                 , keybinding_key         = xK_d
                                 , keybinding_humankey    = [ShiftKey, AlphaKey 'd']
                                 , keybinding_action      = passRemovePrompt csillagPromptConfig
                                 }
                    ]
                }
    , KeyHeading "Screenshots"
    , KeySubmap { keysubmap_description = "Yank the screen"
                , keysubmap_mask = 0
                , keysubmap_key = xK_y
                , keysubmap_humankey = [AlphaKey 'y']
                , keysubmap_submaps =
                    [ KeyBinding { keybinding_description = "Yank the whole screen"
                                 , keybinding_mask = 0
                                 , keybinding_key = xK_s
                                 , keybinding_humankey = [AlphaKey 's']
                                 , keybinding_action = spawn scrot_screen
                                 }
                    , KeyBinding { keybinding_description = "Yank a window"
                                 , keybinding_mask = 0
                                 , keybinding_key = xK_w
                                 , keybinding_humankey = [AlphaKey 'w']
                                 , keybinding_action = spawn scrot_window
                                 }
                    , KeyBinding { keybinding_description = "Yank the focused window"
                                 , keybinding_mask = 0
                                 , keybinding_key = xK_f
                                 , keybinding_humankey = [AlphaKey 'f']
                                 , keybinding_action = spawn scrot_thiswindow
                                 }
                    ]
                }
    , KeyHeading "System"
    , KeySubmap { keysubmap_description = "Manage system functions"
                , keysubmap_mask = 0
                , keysubmap_key = xK_q
                , keysubmap_humankey = [AlphaKey 'q']
                , keysubmap_submaps =
                    [ KeyBinding { keybinding_description = "Quit / Exit XMonad"
                                 , keybinding_mask        = shiftMask
                                 , keybinding_key         = xK_q
                                 , keybinding_humankey    = [AlphaKey 'q']
                                 , keybinding_action      = quitWithWarning
                                 }
                    , KeyBinding { keybinding_description = "Suspend"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_s
                                 , keybinding_humankey    = [AlphaKey 's']
                                 , keybinding_action      = spawn "systemctl suspend"
                                 }
                    , KeyBinding { keybinding_description = "Blank the screen"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_b
                                 , keybinding_humankey    = [AlphaKey 'b']
                                 , keybinding_action      = spawn "sleep 0.5; xset dpms force off"
                                 }
                    , KeyBinding { keybinding_description = "Lock"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_l
                                 , keybinding_humankey    = [AlphaKey 'l']
                                 -- , keybinding_action      = spawn "i3lock -f -e -r 15 -s 11"
                                 , keybinding_action      = spawnOSD lockIcon >> spawn "dm-tool lock"
                                 }
                    , KeySubmap { keysubmap_description = "'Do Not Disturb'"
                                , keysubmap_mask = 0
                                , keysubmap_key = xK_d
                                , keysubmap_humankey = [AlphaKey 'd']
                                , keysubmap_submaps =
                                    [ KeyBinding { keybinding_description = "Enable 'Do Not Disturb'"
                                                 , keybinding_mask        = 0
                                                 , keybinding_key         = xK_bracketleft
                                                 , keybinding_humankey    = [ModKey, ShiftKey, AlphaKey '[']
                                                 , keybinding_action      = spawn "notify-send \"DUNST_COMMAND_PAUSE\" && touch /home/daniel/.dunst_paused"
                                                 }
                                    , KeyBinding { keybinding_description = "Disable 'Do Not Disturb'"
                                                 , keybinding_mask        = 0
                                                 , keybinding_key         = xK_bracketright
                                                 , keybinding_humankey    = [ModKey, ShiftKey, AlphaKey ']']
                                                 , keybinding_action      = spawn "notify-send \"DUNST_COMMAND_RESUME\" && rm -f /home/daniel/.dunst_paused"
                                                 }
                                    ]
                                }
                    , KeyBinding { keybinding_description = "Fix audio" -- "Restart ALSA"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_a
                                 , keybinding_humankey    = [AlphaKey 'a']
                                 , keybinding_action      = do
                                     -- spawn "alsa-restart"
                                     spawnNohup "fix-audio"
                                     spawnOSD "A"
                                 }
                    , KeyBinding { keybinding_description = "Toggle XMobar"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_equal
                                 , keybinding_humankey    = [AlphaKey '=']
                                 , keybinding_action      = spawn "toggle_xmobar"
                                 }
                    , KeyBinding { keybinding_description = "Toggle Screenkey"
                                 , keybinding_mask        = 0
                                 , keybinding_key         = xK_k
                                 , keybinding_humankey    = [AlphaKey 'k']
                                 , keybinding_action      = spawn ".local/scripts/screenkey_toggle.sh"
                                 }
                    ]
                }
    , KeyHeading "Help"
    , KeyBinding { keybinding_description = "Show Help"
                 , keybinding_mask        = shiftMask
                 , keybinding_key         = xK_slash
                 , keybinding_humankey    = [AlphaKey '?']
                 , keybinding_action      = do
                     let keysToString :: String -> [KeyBinding] -> [String]
                         keysToString prefix keys = keys >>= \case
                            KeyBinding d _ _ h _ -> [prefix ++ humankeysToString h ++ "\t\t\t\t" ++ d]
                            KeySubmap  d _ _ h s -> (prefix ++ humankeysToString h ++ "…" ++ "\t\t\t\t" ++ d) : keysToString (prefix ++ humankeysToString h) s
                            KeyHeading h         -> ["", "** " ++ h, ""]
                            KeyMode{}            -> [] -- TODO
                         humankeysToString :: [HumanKey] -> String
                         humankeysToString hkey = intercalate "+" $ show <$> hkey
                     io $ writeFile "/tmp/xmonad-help.txt" $ unlines $ "* Csillag XMonad Help" : keysToString "" (myKeys config)
                     spawnNohup $ term_run $ "less /tmp/xmonad-help.txt"
                     return ()
                 }
    ]

myMouseBindings config = Map.fromList
    [ ((modMask config, button1), dragWindow)
    ]

systemFunctionKeys config =
    [ KeyHeading "System Function Keys"
    , KeyBinding { keybinding_description = "Raise Brightness"
                 , keybinding_mask        = 0
                 , keybinding_key         = xF86XK_MonBrightnessUp
                 , keybinding_humankey    = [BrightnessUpKey]
                 , keybinding_action      = spawn "lux -a 2%" >> spawnOSD brightnessUpIcon
                 }
    , KeyBinding { keybinding_description = "Lower Brightness"
                 , keybinding_mask        = 0
                 , keybinding_key         = xF86XK_MonBrightnessDown
                 , keybinding_humankey    = [BrightnessDownKey]
                 , keybinding_action      = spawn "lux -s 2%" >> spawnOSD brightnessDownIcon
                 }
    , KeyBinding { keybinding_description = "Lower Volume"
                 , keybinding_mask        = 0
                 , keybinding_key         = xF86XK_AudioLowerVolume
                 , keybinding_humankey    = [AudioLowerKey]
                 , keybinding_action      = do
                     spawnOSD volumeDownIcon
                     spawn "amixer sset Master 2%-"
                     spawn ("sleep 0.1; paplay " ++ volumeChangeSound)
                 }
    , KeyBinding { keybinding_description = "Raise Volume"
                 , keybinding_mask        = 0
                 , keybinding_key         = xF86XK_AudioRaiseVolume
                 , keybinding_humankey    = [AudioRaiseKey]
                 , keybinding_action      = do
                     spawnOSD volumeUpIcon
                     spawn "amixer sset Master 2%+"
                     spawn ("sleep 0.1; paplay " ++ volumeChangeSound)
                 }
    , KeyBinding { keybinding_description = "Toggle Mute"
                 , keybinding_mask        = 0
                 , keybinding_key         = xF86XK_AudioMute
                 , keybinding_humankey    = [AudioMuteKey]
                 , keybinding_action      = do
                     spawnOSD volumeMuteIcon
                     spawn "amixer sset Master toggle"
                     spawn ("sleep 0.1; paplay " ++ volumeChangeSound)
                 }
    , KeyBinding { keybinding_description = "Play Test Sound"
                 , keybinding_mask        = modMask config .|. controlMask
                 , keybinding_key         = xK_v
                 , keybinding_humankey    = [ModKey, ControlKey, AlphaKey 'v']
                 , keybinding_action      = do
                     spawnOSD volumePlayIcon
                     spawn $ "paplay " ++ volumeChangeSound
                 }
    , KeyBinding { keybinding_description = "Play/Pause"
                 , keybinding_mask        = modMask config
                 , keybinding_key         = xK_backslash
                 , keybinding_humankey    = [ModKey, AlphaKey '\\']
                 , keybinding_action      = spawn "playerctl play-pause"
                 }
    , KeyBinding { keybinding_description = "Next Track"
                 , keybinding_mask        = modMask config
                 , keybinding_key         = xK_Right
                 , keybinding_humankey    = [ModKey, RightArrowKey]
                 , keybinding_action      = spawn "playerctl next"
                 }
    , KeyBinding { keybinding_description = "Previous Track"
                 , keybinding_mask        = modMask config
                 , keybinding_key         = xK_Left
                 , keybinding_humankey    = [ModKey, LeftArrowKey]
                 , keybinding_action      = spawn "playerctl previous"
                 }
    ]
