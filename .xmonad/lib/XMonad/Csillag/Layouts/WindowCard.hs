{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, BlockArguments #-}

module XMonad.Csillag.Layouts.WindowCard
    ( windowCard
    )
where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.Decoration
import XMonad.Layout.LayoutModifier


data ReverseLayout a = ReverseLayout deriving (Show, Read)
reverseLayout = ModifiedLayout ReverseLayout
instance LayoutModifier ReverseLayout a where
    pureModifier ReverseLayout _ _ rects = (reverse rects, Nothing)

data WindowCard a = WindowCard Int deriving (Show, Read)
windowCard size = reverseLayout . decoration shrinkText theme (WindowCard size) . reverseLayout
    where
        bg = "#1D212A"
        bg' = "#00040A"
        theme = Theme
            { activeColor = bg'
            , inactiveColor = bg
            , urgentColor = bg
            , activeBorderColor = "#FFFFFF"
            , inactiveBorderColor = "#666666"
            , urgentBorderColor = "#666666"
            , activeBorderWidth = 0
            , inactiveBorderWidth = 0
            , urgentBorderWidth = 0
            , activeTextColor = bg'
            , inactiveTextColor = bg
            , urgentTextColor = bg
            , fontName = "xft:FantasqueSansMono Nerd Font:size=18:antialias=true:autohint=True"
            , decoWidth = 2 -- unused
            , decoHeight = 2 -- unused
            , windowTitleAddons = []
            , windowTitleIcons = []
            }

instance DecorationStyle WindowCard Window where
    describeDeco _ = "WindowCard"

    shrink (WindowCard dw) _ (Rectangle x y w h) = Rectangle (x + fi dw) y (w - fi dw) h
    pureDecoration (WindowCard dw) _ _ _ stack _ (win, Rectangle x y w h)
      | win `elem` W.integrate stack && w > fi dw = Just $ Rectangle x y w h
      | otherwise = Nothing
