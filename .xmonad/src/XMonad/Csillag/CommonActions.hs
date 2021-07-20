{-# LANGUAGE NumericUnderscores, LambdaCase #-}

module XMonad.Csillag.CommonActions where

import Data.Maybe
import Data.Semigroup
import Data.Char (isSpace)
import Control.Monad
import Control.Concurrent
import System.Exit

import XMonad hiding (config)
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageDocks

import XMonad.Actions.GridSelect

import XMonad.Util.XUtils             ( fi )
import XMonad.Prompt
import XMonad.Util.WindowProperties   ( getProp32 )
import XMonad.Prompt.FuzzyMatch


-- Helper functions

-- -- GridSelect

myGridSelectWorkspace config func = withWindowSet $ \ws -> do
  let wss =
        filter (/= "NSP")
          $ map W.tag
          $ filter ((/="NSP") . W.tag)
          $ circshiftN (succ $ length $ W.visible ws)
          $ W.workspaces ws
  gridselect config (zip wss wss) >>= flip whenJust func

myGridSelectConfig = (buildDefaultGSConfig colorizer) { gs_cellheight   = round size
                                                      , gs_cellwidth    = round $ silver * size
                                                      , gs_cellpadding  = 30
                                                      , gs_font         = "xft:Lato-Regular:pixelsize=20"
                                                      , gs_originFractX = 0.5
                                                      , gs_originFractY = 0.5
                                                      , gs_bordercolor  = "#666666"
                                                      }
    where size   = 150.0 :: Double
          silver = 2.4142135623 :: Double
          colorizer :: WorkspaceId -> Bool -> X (String, String)
          colorizer wks active = withWindowSet $ \ws -> do
              let curWkss = map W.tag $ W.workspace (W.current ws) : map W.workspace (W.visible ws)
              return $ if wks `elem` curWkss
                          then if active then ("#aaaaaa", "#000000") else ("#777777", "#000000")
                          else if active then ("#ffffff", "#000000") else ("#000000", "#ffffff")

-- -- Misc

circshiftN :: Int -> [a] -> [a]
circshiftN 0 lst = lst
circshiftN k lst = circshiftN (pred k) $ circshift lst

circshift :: [a] -> [a]
circshift []       = []
circshift (x : xs) = xs ++ [x]

-- Actions

spawnOSD :: String -> X ()
spawnOSD icon = spawn $ "show-osd '" ++ icon ++ "'"

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

-- Prompt config

csillagPromptConfig :: XPConfig
csillagPromptConfig = def { bgColor             = "#1b2326"
                          , fgColor             = "#FFFFFF"
                          , bgHLight            = "#00e896"
                          , fgHLight            = "#000000"
                          , borderColor         = "#AAAAAA"
                          , promptBorderWidth   = 0
                          , position            = CenteredAt 0.2 0.8
                          , alwaysHighlight     = True
                          , font                = "xft:FantasqueSansMono Nerd Font:size=20"
                          , height              = 50
                          , maxComplRows        = Just 12
                          , historySize         = 200
                          -- , historyFilter       = id
                          , completionKey       = (controlMask, xK_n)
                          , changeModeKey       = xK_F1
                          , defaultText         = ""
                          , autoComplete        = Nothing
                          , showCompletionOnTab = False
                          , searchPredicate     = fuzzyMatch
                          , sorter              = fuzzySort
                          , promptKeymap        = vimLikeXPKeymap' (\c -> c { bgNormal = "grey22" }) id id isSpace
                          }
