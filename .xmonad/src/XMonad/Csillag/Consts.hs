module XMonad.Csillag.Consts where

import XMonad hiding (config)
import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Actions.GridSelect
import XMonad.Layout.Tabbed

import Data.Char (isSpace)


volumeChangeSound = "~/.xmonad/audio-volume-change.wav"

-- Icons:
volumeUpIcon       = "ﱛ"
volumeDownIcon     = "ﱜ"
volumeMuteIcon     = "ﱝ"
volumePlayIcon     = "墳"
brightnessUpIcon   = "\xf5de + "
brightnessDownIcon = "\xf5de - "

workspaceTempFile = "/tmp/.xmonad_workspaces"

-- Window gap size

gapsize :: Integer
gapsize = 4

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

-- GridSelect config

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

-- Tabbed config

tabbedTheme = def { fontName            = "xft:FantasqueSansMono Nerd Font:size=12"
                  , activeColor         = "#707070"
                  , activeTextColor     = "#ffffff"
                  , activeBorderColor   = "#eeeeee"
                  , inactiveColor       = "#333333"
                  , inactiveTextColor   = "#EEEEEE"
                  , inactiveBorderColor = "#555555"
                  }
