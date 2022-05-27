{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Csillag.Layouts
    ( myLayouts
    )
where

import XMonad hiding ((|||))
import Data.Maybe (fromMaybe)
import qualified XMonad.StackSet as W

import XMonad.Layout.LayoutCombinators ((|||))

import XMonad.Layout.Renamed
import XMonad.Layout.IfMax
import XMonad.Layout.Dwindle
import XMonad.Layout.Grid
import XMonad.Layout.OneBig
import XMonad.Layout.ThreeColumns
import XMonad.Layout.StackTile
import XMonad.Layout.Accordion
import XMonad.Layout.Circle
import XMonad.Layout.Cross

import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Maximize
import XMonad.Layout.SubLayouts
import XMonad.Layout.BoringWindows hiding (Replace)


spacing' amount = spacingRaw False (Border gapsize' gapsize' (amount + gapsize') (amount + gapsize')) True (Border gapsize gapsize gapsize gapsize) True
    where
        gapsize = 4
        gapsize' = 10

-- spacing layout = withoutPadding
-- spacing layout = IfMax 3 withoutPadding withPadding
spacing layout = ifWider 2000 withPadding withoutPadding
    where
        withoutPadding = spacing' 0 layout
        withPadding = IfMax 1 (spacing' 1000 layout) $ IfMax 2 (spacing' 500 layout) $ withoutPadding

myLayouts = draggingVisualizer
          $ maximize
          $ subLayout [] Full
          $ boringWindows
          $ renamed [Replace "Grid"]           (spacing $ IfMax 2 (Tall 1 (3/100) (1/2)) Grid) |||
            renamed [Replace "ThreeColMid"]    (spacing $ ThreeColMid 1 (3/100) (1/2))         |||
            renamed [Replace "Dishes"]         (spacing $ StackTile 2 (3/100) (5/6))           |||
            renamed [Replace "OneBig"]         (spacing' 0 $ OneBig (6/7) (6/7))               |||
            renamed [Replace "Dwindle"]        (spacing $ Dwindle R CW 1 1.1)                  |||
            renamed [Replace "Mirror Dwindle"] (spacing $ Mirror $ Dwindle R CW 1 1.1)         |||
            renamed [Replace "Spiral"]         (spacing $ Spiral R CW 1 1.1)                   |||
            renamed [Replace "Accordion"]      (spacing' 0 Accordion)                          |||
            renamed [Replace "Circle"]         (spacing' 0 Circle)                             |||
            renamed [Replace "Plus"]           (spacing simpleCross)                           |||
            renamed [Replace "Tall"]           (spacing $ Tall 1 (3/100) (1/2))                |||
            renamed [Replace "Mirror Tall"]    (spacing $ Mirror $ Tall 1 (3/100) (1/2))       |||
            renamed [Replace "Full"]           Full

----------------------------------------------------
-- PerScreen
-- adapted from https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/src/XMonad.Layout.PerScreen.html

ifWider :: (LayoutClass l1 a, LayoutClass l2 a)
               => Dimension   -- ^ target screen width
               -> l1 a        -- ^ layout to use when the screen is wide enough
               -> l2 a        -- ^ layout to use otherwise
               -> PerScreen l1 l2 a
ifWider w = PerScreen w False

data PerScreen l1 l2 a = PerScreen Dimension Bool (l1 a) (l2 a) deriving (Read, Show)

instance (LayoutClass l1 Window, LayoutClass l2 Window) => LayoutClass (PerScreen l1 l2) Window where
    runLayout (W.Workspace i p@(PerScreen w _ lt lf) ms) r
        | rect_width r > w    = do (wrs, mlt') <- runLayout (W.Workspace i lt ms) r
                                   return (wrs, Just $ mkNewPerScreenT p mlt')
        | otherwise           = do (wrs, mlt') <- runLayout (W.Workspace i lf ms) r
                                   return (wrs, Just $ mkNewPerScreenF p mlt')
        where
            mkNewPerScreenT :: PerScreen l1 l2 a -> Maybe (l1 a) ->
                                  PerScreen l1 l2 a
            mkNewPerScreenT (PerScreen w' _ lt' lf') mlt' =
                (\lt'' -> PerScreen w' True lt'' lf') $ fromMaybe lt' mlt'

            mkNewPerScreenF :: PerScreen l1 l2 a -> Maybe (l2 a) ->
                                  PerScreen l1 l2 a
            mkNewPerScreenF (PerScreen w' _ lt' lf') mlf' =
                PerScreen w' False lt' $ fromMaybe lf' mlf'


    handleMessage (PerScreen w bool lt lf) m
        | bool      = handleMessage lt m >>= maybe (return Nothing) (\nt -> return . Just $ PerScreen w bool nt lf)
        | otherwise = handleMessage lf m >>= maybe (return Nothing) (return . Just . PerScreen w bool lt)

    description (PerScreen _ True  l1 _) = description l1
    description (PerScreen _ _     _ l2) = description l2
