{-# LANGUAGE BlockArguments #-}

module XMonad.Csillag.Commands
  ( commandHandler,
  )
where

import Control.Monad
import System.Exit (exitSuccess)
import Text.Read (readMaybe)
import XMonad
import XMonad.Actions.CopyWindow (copy, kill1)
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace, removeEmptyWorkspaceAfter, renameWorkspaceByName)
import XMonad.Actions.Navigation2D (Direction2D (..), screenSwap, windowGo, windowSwap)
import XMonad.Csillag.Scratchpads (myScratchpads)
import XMonad.Layout.Maximize (maximizeRestore)
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad (namedScratchpadAction)
import XMonad.Layout.LayoutCombinators (JumpToLayout (..))

commandHandler :: [String] -> X ()
-- Core
commandHandler ["restart-xmonad"] = spawn "restart-xmonad" -- TODO remove?
commandHandler ["exit-xmonad"] = io exitSuccess
-- Directional actions
commandHandler ["focus-left"] = windowGo L False
commandHandler ["focus-down"] = windowGo D False
commandHandler ["focus-up"] = windowGo U False
commandHandler ["focus-right"] = windowGo R False
commandHandler ["swap-left"] = windowSwap L False
commandHandler ["swap-down"] = windowSwap D False
commandHandler ["swap-up"] = windowSwap U False
commandHandler ["swap-right"] = windowSwap R False
-- Stack actions
commandHandler ["focus-master"] = windows W.focusMaster
commandHandler ["swap-master"] = windows W.swapMaster
commandHandler ["focus-prev"] = windows W.focusUp
commandHandler ["focus-next"] = windows W.focusDown
commandHandler ["swap-prev"] = windows W.swapUp
commandHandler ["swap-next"] = windows W.swapDown
-- Floating windows
commandHandler ["tile-focused"] = withFocused $ windows . W.sink
commandHandler ["float-focused-as-scratchpad"] = withFocused $ windows . flip W.float (W.RationalRect 0.05 0.05 0.9 0.9)
-- Close windows
commandHandler ["close-window"] = kill1
commandHandler ["close-program"] = kill
commandHandler ["close-copy"] = killCopy
-- Screens (Xinerama)
commandHandler ["focus-screen", screen] = whenJust (readMaybe screen) $ screenWorkspace >=> flip whenJust (windows . W.view)
commandHandler ["swap-screens"] = screenSwap U True
-- Workspaces
commandHandler ["get-workspace-list", output_file] = do
  wkss <- withWindowSet \ws ->
    return $
      filter (/= "NSP") $
        circshiftN (succ $ length $ W.visible ws) $
          map W.tag $
            W.workspaces ws
  io $ writeFile output_file $ unlines wkss
  where
    circshiftN :: Int -> [a] -> [a]
    circshiftN 0 lst = lst
    circshiftN k lst = circshiftN (pred k) $ circshift lst

    circshift :: [a] -> [a]
    circshift [] = []
    circshift (x : xs) = xs ++ [x]
commandHandler ["rename-workspace", new_name] = renameWorkspaceByName new_name
commandHandler ["delete-workspace-if-empty"] = removeEmptyWorkspaceAfter (windows \ws -> flip W.view ws $ W.tag $ head $ filter ((/= "NSP") . W.tag) $ W.hidden ws)
commandHandler ["go-to-workspace", workspace_name] = windows $ W.view workspace_name
commandHandler ["send-to-workspace", workspace_name] = windows $ W.shift workspace_name
commandHandler ["send-copy-to-workspace", workspace_name] = windows $ copy workspace_name
commandHandler ["create-workspace", name] = addHiddenWorkspace name
commandHandler ["switch-workspace"] = windows \ws -> flip W.view ws $ W.tag $ head $ filter ((/= "NSP") . W.tag) $ W.hidden ws
-- Layouts
commandHandler ["cycle-layouts"] = sendMessage NextLayout
commandHandler ["set-layout", new_layout] = sendMessage $ JumpToLayout new_layout
commandHandler ["shrink-master-area"] = sendMessage Shrink
commandHandler ["expand-master-area"] = sendMessage Expand
commandHandler ["increment-masters"] = sendMessage $ IncMasterN 1
commandHandler ["decrement-masters"] = sendMessage $ IncMasterN $ -1
commandHandler ["toggle-magnifier"] = withFocused $ sendMessage . maximizeRestore
-- Scratchpads
commandHandler ["toggle-scratchpad", scratchpad_name] = namedScratchpadAction myScratchpads scratchpad_name
-- No such command
commandHandler _ = spawn "notify-send XMonad 'bad XMonad command'"

killCopy =
  let delete'' w = W.modify Nothing $ W.filter (/= w)
   in withWindowSet \ss ->
        whenJust (W.peek ss) \w ->
          when (W.member w $ delete'' w ss) $
            windows $ delete'' w
