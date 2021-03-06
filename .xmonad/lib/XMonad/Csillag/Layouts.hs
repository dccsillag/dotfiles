{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}

module XMonad.Csillag.Layouts
    ( myLayouts
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

import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Maximize


myLayouts = draggingVisualizer $ maximize $
    renamed [Replace "Grid"]           (IfMax 2 (Tall 1 (3/100) (1/2)) Grid) |||
    renamed [Replace "Mosaic"]         (MosaicAlt M.empty)                   |||
    renamed [Replace "ThreeColMid"]    (ThreeColMid 1 (3/100) (1/2))         |||
    renamed [Replace "Dishes"]         (StackTile 2 (3/100) (5/6))           |||
    renamed [Replace "OneBig"]         (OneBig (6/7) (6/7))                  |||
    renamed [Replace "Full"]           (tabbed shrinkText tabbedTheme)       |||
    renamed [Replace "Dwindle"]        (Dwindle R CW 1 1.1)                  |||
    renamed [Replace "Mirror Dwindle"] (Mirror $ Dwindle R CW 1 1.1)
    where

tabbedTheme = def { fontName            = "xft:FantasqueSansMono Nerd Font:size=12"
                  , activeColor         = "#707070"
                  , activeTextColor     = "#ffffff"
                  , activeBorderColor   = "#eeeeee"
                  , inactiveColor       = "#333333"
                  , inactiveTextColor   = "#EEEEEE"
                  , inactiveBorderColor = "#555555"
                  }
