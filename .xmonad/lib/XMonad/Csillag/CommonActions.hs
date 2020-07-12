{-# LANGUAGE NumericUnderscores #-}

module XMonad.Csillag.CommonActions where

import Data.Maybe
import Data.List
import Data.Semigroup
import Data.Char (toLower)
import Data.Function (on)
import Control.Monad
import Control.Concurrent
import System.Exit

import XMonad hiding (config)
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageDocks
-- import XMonad.Hooks.DynamicLog

-- import XMonad.Actions.WorkspaceNames
import XMonad.Util.Dmenu
import XMonad.Actions.GridSelect

import XMonad.Util.XUtils             ( fi )
import XMonad.Prompt
import XMonad.Util.WindowProperties   ( getProp32 )

-- import XMonad.Csillag.Consts


-- Helper functions

-- -- Stability

keepFocusedWindow :: X a -> X a
keepFocusedWindow action = do
    currentWindow <- withWindowSet $ return . fmap W.focus . W.stack . W.workspace . W.current
    result <- action
    case currentWindow of
         Nothing -> return result
         Just win -> withWindowSet $ \ws -> do
             when (W.member win ws) $ windows $ W.focusWindow win
             return result

-- -- GridSelect

myGridSelectWorkspace config func = withWindowSet $ \ws -> do
  let wss =
        filter (/= "NSP")
          $ map W.tag
          -- $ filter (\x -> isJust (W.stack x) || isJust (lookup' $ W.tag x))
          $ filter ((/="NSP") . W.tag)
          $ circshiftN (succ $ length $ W.visible ws)
          $ W.workspaces ws
  gridselect config (zip wss wss) >>= flip whenJust func

myGridSelectConfig = (buildDefaultGSConfig colorizer)
  { gs_cellheight   = round size
  , gs_cellwidth    = round $ silver * size
  , gs_cellpadding  = 30
  , gs_font         = "xft:Lato-Regular:pixelsize=20"
  , gs_originFractX = 0.5
  , gs_originFractY = 0.5
  , gs_bordercolor  = "#777777"
  }
 where
  size   = 150.0 :: Double
  silver = 2.4142135623 :: Double
  colorizer :: WorkspaceId -> Bool -> X (String, String)
  colorizer wks active = withWindowSet $ \ws -> do
    let curWkss = map W.tag $ W.workspace (W.current ws) : map W.workspace
                                                               (W.visible ws)
    return $ if wks `elem` curWkss
      then if active then ("#aaaaaa", "#000000") else ("#777777", "#222222")
      else if active then ("#dddddd", "#000000") else ("#444444", "#bbbbbb")

-- -- Rofi

rofi :: MonadIO m => String -> Maybe String -> [String] -> m String
rofi prompt Nothing options =
  stripWhitespace
    <$> menuArgs "rofi" ["-dmenu", "-markup-rows", "-i", "-p", prompt] options
rofi prompt (Just msg) options =
  stripWhitespace
    <$> menuArgs "rofi"
                 ["-dmenu", "-markup-rows", "-i", "-p", prompt, "-e", msg]
                 options

-- showMessage :: String -> X ()
-- showMessage msg = void $ menuArgs "rofi" ["-e", msg] []

-- -- PP

polybarLabel :: String -> String -> String
polybarLabel _     ""  = ""
polybarLabel label str = "%{F#999999}%{T5}" ++ label ++ "%{T-}%{F-} " ++ str

wrapMonospace :: String -> String
wrapMonospace str = "%{T1}" ++ str ++ "%{T-}"

wrapStdFont :: String -> String
wrapStdFont str = "%{T2}" ++ str ++ "%{T-}"

-- -- Workspaces

getEmptyWorkspace :: WindowSet -> WindowSpace
getEmptyWorkspace ws = case filterEmptyWorkspaces ws $ W.workspaces ws of
  x : _ -> x
  []    -> W.workspace $ W.current ws

filterEmptyWorkspaces :: WindowSet -> [WindowSpace] -> [WindowSpace]
filterEmptyWorkspaces ws =
  filter $ null . (\\ stickyWindows) . W.integrate' . W.stack
  where stickyWindows = getStickyWindows ws

getStickyWindows :: WindowSet -> [Window]
getStickyWindows ws = filterInAll $ map (W.integrate' . W.stack) $ W.workspaces
  ws
 where
  filterInAll :: Eq a => [[a]] -> [a]
  filterInAll xs = filter (flip all xs . elem) $ concat xs

-- -- Misc

whenNonEmpty :: Monad m => ([a] -> m ()) -> [a] -> m ()
whenNonEmpty f lst | null lst  = return ()
                   | otherwise = f lst

circshiftN :: Int -> [a] -> [a]
circshiftN 0 lst = lst
circshiftN k lst = circshiftN (pred k) $ circshift lst

circshift :: [a] -> [a]
circshift []       = []
circshift (x : xs) = xs ++ [x]

stripWhitespace :: String -> String
stripWhitespace = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- Actions

spawnOSD :: String -> X ()
spawnOSD icon = spawn $ "show-osd '" ++ icon ++ "'"

-- spawnOSD' :: String -> String -> X ()
-- spawnOSD' icon compl = spawn $ "show-osd '" ++ icon ++ "' '" ++ compl ++ "'"

-- myWorkspaceNamesPP :: PP -> X PP
-- myWorkspaceNamesPP pp = do
--   names <- do
--     lookup' <- getWorkspaceNames'
--     return $ \wks ->
--       if wks == "NSP" then "" else maybe "" wrapStdFont (lookup' wks)
--   return $ pp { ppCurrent         = ppCurrent pp . names
--               , ppVisible         = ppVisible pp . names
--               , ppHidden          = ppHidden pp . names
--               , ppHiddenNoWindows = ppHiddenNoWindows pp . names
--               , ppUrgent          = ppUrgent pp . names
--               }

myFullscreenEventHook :: Event -> X All
myFullscreenEventHook (ClientMessageEvent _ _ _ dpy win typ (_ : dats)) = do
  wmstate <- getAtom "_NET_WM_STATE"
  fullsc  <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate  <- fromMaybe [] `fmap` getProp32 wmstate win

  let -- Constants for the _NET_WM_STATE protocol:
      ptype = 4 -- The atom property type for changeProperty
      chWstate f =
        io $ changeProperty32 dpy win wmstate ptype propModeReplace (f wstate)

  when (typ == wmstate && fi fullsc `elem` dats) $ do
    chWstate (fi fullsc :)
    -- withWindowSet $ \ws -> updateLayout (W.tag $ W.workspace $ W.current ws)
    -- withWindowSet $ \ws -> setLayout $ W.layout $ W.workspace $ W.current ws
    sendMessage ToggleStruts
    io $ threadDelay 50_000
    sendMessage ToggleStruts
    refresh
  return $ All True
myFullscreenEventHook _ = return $ All True

quitWithWarning :: X ()
quitWithWarning = io exitSuccess
-- quitWithWarning = do
--   let shutdown = "shutdown"
--       reboot   = "reboot"
--       lock     = "lock"
--       logout   = "logout"
--       suspend  = "suspend"
--       cancelop = "cancel"
--   s <- rofi "Quit" Nothing [cancelop, lock, suspend, logout, shutdown, reboot]
--
--   -- TODO when (s `elem` options) $ mapM_ killWindow
--   when (s == lock) $ spawnOSD lockIcon >> spawn "dm-tool lock"
--   when (s == suspend) $ spawn "systemctl suspend"
--   when (s == logout) $ io exitSuccess
--   when (s == shutdown) $ spawnOSD shutdownIcon >> spawn "systemctl poweroff"
--   when (s == reboot) $ spawnOSD rebootIcon >> spawn "systemctl reboot"

-- Prompt config

csillagPromptConfig :: XPConfig
csillagPromptConfig = def { bgColor             = "#221226"
                          , fgColor             = "#CCCCCC"
                          , bgHLight            = "#00e896"
                          , fgHLight            = "#000000"
                          , borderColor         = "#FFFFFF"
                          , promptBorderWidth   = 2
                          , position            = CenteredAt 0.2 0.5
                          , alwaysHighlight     = False
                          , font                = "xft:FantasqueSansMono Nerd Font:size=20"
                          , height              = 40
                          , maxComplRows        = Just 15
                          , historySize         = 200
                          -- , historyFilter       = id
                          -- , promptKeymap        = ???
                          , completionKey       = (0, xK_Tab)
                          , changeModeKey       = xK_F1
                          , defaultText         = ""
                          , autoComplete        = Nothing
                          , showCompletionOnTab = False
                          -- , searchPredicate     = isInfixOf `on` fmap toLower
                          -- , searchPredicate     = (==)
                          , searchPredicate     = isSubstringOf `on` fmap toLower
                          }

isSubstringOf :: String -> String -> Bool
s0 `isSubstringOf` s1 = s0 `isPrefixOf` s1 || s0 `isInfixOf` s1 || s0 `isSuffixOf` s1

myPromptCompletion :: [String] -> String -> [String]
myPromptCompletion l s = filter (\x -> fmap toLower s `isSubstringOf` fmap toLower x) l

myPromptCompletion' :: [String] -> String -> [String]
myPromptCompletion' l [] = l
myPromptCompletion' l s = filter (\x -> fmap toLower s `isSubstringOf` fmap toLower x) l
