{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}

module XMonad.Csillag.Layouts
    ( myLayouts
    , windowGap
    , MAGNIFIER(..)
    , WINDOWTITLES(..)
    )
where

import XMonad hiding ((|||))

import XMonad.Layout.LayoutCombinators ((|||))
import qualified Data.Map as M

import XMonad.Layout.Renamed
import XMonad.Layout.IfMax
import XMonad.Layout.Dwindle
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.OneBig
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MosaicAlt
import XMonad.Layout.StackTile

import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.Magnifier

import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Spacing
import XMonad.Layout.SimpleDecoration

data MAGNIFIER = MAGNIFIER deriving (Read, Show, Eq, Typeable)

data WINDOWTITLES = WINDOWTITLES deriving (Read, Show, Eq, Typeable)

instance Transformer MAGNIFIER Window where
    transform MAGNIFIER x k = k (magnifier x) $ \(ModifiedLayout _ x') -> x'

instance Transformer WINDOWTITLES Window where
    transform WINDOWTITLES x k = k (simpleDeco shrinkText windowbarTheme x) $ \(ModifiedLayout _ x') -> x'

myLayouts = draggingVisualizer $ mkToggle (single MAGNIFIER) $ mkToggle (single WINDOWTITLES) $
    renamed [Replace "Grid"]           (winSpaces $ IfMax 2 (Tall 1 (3/100) (1/2)) Grid) |||
    renamed [Replace "Mosaic"]         (winSpaces $ MosaicAlt M.empty)                   |||
    renamed [Replace "ThreeColMid"]    (winSpaces $ ThreeColMid 1 (3/100) (1/2))         |||
    renamed [Replace "Dishes"]         (winSpaces $ StackTile 2 (3/100) (5/6))           |||
    renamed [Replace "OneBig"]         (winSpaces $ OneBig (6/7) (6/7))                  |||
    renamed [Replace "Full"]           (tabbed shrinkText tabbedTheme)                   |||
    renamed [Replace "Dwindle"]        (winSpaces $ Dwindle R CW 1 1.1)                  |||
    renamed [Replace "Mirror Dwindle"] (Mirror $ winSpaces $ Dwindle R CW 1 1.1)
    where
        winSpaces = spacingRaw False
                               (Border 0 windowGap windowGap windowGap)
                               True
                               (Border windowGap windowGap windowGap windowGap)
                               True

tabbedTheme = def { fontName            = "xft:FantasqueSansMono Nerd Font:size=12"
                  , activeColor         = "#707070"
                  , activeTextColor     = "#ffffff"
                  , activeBorderColor   = "#eeeeee"
                  , inactiveColor       = "#333333"
                  , inactiveTextColor   = "#EEEEEE"
                  , inactiveBorderColor = "#555555"
                  }

windowbarTheme = def { fontName            = "xft:FantasqueSansMono Nerd Font:size=12"
                     , activeColor         = "#707070"
                     , activeTextColor     = "#ffffff"
                     , activeBorderColor   = "#eeeeee"
                     , inactiveColor       = "#333333"
                     , inactiveTextColor   = "#EEEEEE"
                     , inactiveBorderColor = "#555555"
                     , decoWidth           = 5000
                     }

windowGap = 2
