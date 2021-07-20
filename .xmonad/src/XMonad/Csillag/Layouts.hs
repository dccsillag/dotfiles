module XMonad.Csillag.Layouts
    ( myLayouts
    )
where

import XMonad hiding ((|||))

import XMonad.Layout.LayoutCombinators ((|||))

import XMonad.Layout.Renamed
import XMonad.Layout.IfMax
import XMonad.Layout.Dwindle
import XMonad.Layout.Grid
import XMonad.Layout.OneBig
import XMonad.Layout.ThreeColumns
import XMonad.Layout.StackTile

import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Maximize
import XMonad.Layout.SubLayouts
import XMonad.Layout.BoringWindows hiding (Replace)

import XMonad.Csillag.Consts ( gapsize )


myLayouts = draggingVisualizer
          $ maximize
          $ subLayout [] Full
          $ boringWindows
          $ spacing
          $ renamed [Replace "Grid"]           (IfMax 2 (Tall 1 (3/100) (1/2)) Grid) |||
            renamed [Replace "ThreeColMid"]    (ThreeColMid 1 (3/100) (1/2))         |||
            renamed [Replace "Dishes"]         (StackTile 2 (3/100) (5/6))           |||
            renamed [Replace "OneBig"]         (OneBig (6/7) (6/7))                  |||
            renamed [Replace "Dwindle"]        (Dwindle R CW 1 1.1)                  |||
            renamed [Replace "Mirror Dwindle"] (Mirror $ Dwindle R CW 1 1.1)
    where
        spacing = spacingRaw True (Border 0 gapsize gapsize gapsize) True (Border gapsize gapsize gapsize gapsize) True
