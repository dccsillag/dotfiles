{-# LANGUAGE NumericUnderscores #-}

module XMonad.Csillag.Consts where

import XMonad hiding (config)
import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Actions.GridSelect

import Data.Char (isSpace, isPrint)


-- Simple global configs

volumeChangeSound = "~/.xmonad/audio-volume-change.wav"
workspaceTempFile = "/tmp/.xmonad_workspaces"

gapsize :: Integer
gapsize = 4

uiFont :: String
uiFont = "xft:FantasqueSansMono Nerd Font:size=18:antialias=true:autohint=True"

uiBorderColor :: String
uiBorderColor = "#2e343f"

uiNormalBgColor, uiNormalFgColor :: String
uiNormalBgColor = "#282C33"
uiNormalFgColor = "#9cb0ba"

uiPrimaryBgColor, uiPrimaryFgColor :: String
uiPrimaryBgColor = "#7F9BC7"
uiPrimaryFgColor = "#121212"

uiSecondaryBgColor, uiSecondaryFgColor :: String
uiSecondaryBgColor = "#31363f"
uiSecondaryFgColor = "#9cb0ba"

-- Prompt config

csillagPromptConfig :: XPConfig
csillagPromptConfig =
    XPC { font                 = uiFont
        , bgColor              = uiNormalBgColor
        , fgColor              = uiNormalFgColor
        , bgHLight             = uiPrimaryBgColor
        , fgHLight             = uiPrimaryFgColor
        , borderColor          = uiBorderColor
        , promptBorderWidth    = 1
        , position             = CenteredAt 0.25 0.5
        , alwaysHighlight      = True
        , height               = 60 -- +1?
        , maxComplRows         = Just 8
        -- , maxComplColumns      = Just 1
        , historySize          = 0
        , historyFilter        = historyFilter def
        , promptKeymap         = vimLikeXPKeymap' id ("[N] "++) (filter isPrint) isSpace
        , completionKey        = (0, xK_Tab)
        , changeModeKey        = xK_F1
        , defaultText          = ""
        , autoComplete         = Nothing
        , showCompletionOnTab  = False
        -- , complCaseSensitivity = CaseInSensitive
        , searchPredicate      = fuzzyMatch
        , defaultPrompter      = ("[I] "++)
        , sorter               = fuzzySort
        }

-- GridSelect config

myGridSelectConfig = GSConfig { gs_cellheight   = round size
                              , gs_cellwidth    = round $ silver * size
                              , gs_cellpadding  = 30
                              , gs_colorizer    = colorizer
                              , gs_font         = uiFont
                              , gs_navigate     = gs_navigate def
                              , gs_rearranger   = gs_rearranger def
                              , gs_originFractX = 0.5
                              , gs_originFractY = 0.5
                              , gs_bordercolor  = uiBorderColor
                              }
    where size   = 150.0 :: Double
          silver = 2.4142135623 :: Double
          colorizer :: WorkspaceId -> Bool -> X (String, String)
          colorizer wks active = withWindowSet $ \ws -> do
              let curWkss = map W.tag $ W.workspace (W.current ws) : map W.workspace (W.visible ws)
              return $ if active then (uiPrimaryBgColor, uiPrimaryFgColor)
                                 else if wks `elem` curWkss then (uiSecondaryBgColor, uiSecondaryFgColor)
                                                            else ( uiNormalBgColor,  uiNormalFgColor)
