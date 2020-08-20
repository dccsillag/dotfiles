module XMonad.Csillag.Layouts
  ( myLayouts
  , windowGap
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

import XMonad.Layout.Spacing

myLayouts =
  renamed [Replace "Dwindle"] (winSpaces $ Dwindle R CW 1 1.1)
    ||| renamed [Replace "Mirror Dwindle"]
                (Mirror $ winSpaces $ Dwindle R CW 1 1.1)
    ||| renamed [Replace "Grid"]
                (winSpaces $ IfMax 2 (Tall 1 (3 / 100) (1 / 2)) Grid)
    ||| renamed [Replace "Full"]   simpleTabbed
    ||| renamed [Replace "OneBig"] (winSpaces $ OneBig (3 / 4) (3 / 4))
    ||| renamed [Replace "ThreeColMid"]
                (winSpaces $ ThreeColMid 1 (3 / 100) (1 / 2))
    ||| renamed [Replace "Mosaic"]
                (winSpaces $ MosaicAlt M.empty)
    -- ||| renamed [Replace "Circle"] Circle
 where
  winSpaces = spacingRaw True
                         (Border 0 windowGap windowGap windowGap)
                         True
                         (Border windowGap windowGap windowGap windowGap)
                         True

windowGap = 2
