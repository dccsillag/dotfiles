{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, BlockArguments #-}

module XMonad.Csillag.Layouts
    ( myLayouts
    , pickOrPlace
    , toggleCollapsed
    )
where

import XMonad hiding ((|||))
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutCombinators ((|||))

import XMonad.Layout.IfMax
import XMonad.Layout.Grid
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.PerScreen

import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Maximize
import XMonad.Layout.SubLayouts
import XMonad.Layout.BoringWindows hiding (Replace)
import XMonad.Layout.NoBorders


-- spacing layout = withoutPadding
-- spacing layout = IfMax 3 withoutPadding withPadding
spacing layout = ifWider 2000 withPadding withoutPadding
    where
        withoutPadding = spacing' 0 layout
        withPadding = IfMax 1 (spacing' 1000 layout) $ IfMax 2 (spacing' 500 layout) $ withoutPadding

        spacing' amount = spacingRaw False (Border gapsize' gapsize' (amount + gapsize') (amount + gapsize')) True (Border gapsize gapsize gapsize gapsize) True
            where
                gapsize = 4
                gapsize' = 10

myLayouts = draggingVisualizer $ maximize $ subLayout [] Full $ boringWindows $
    fallbackLayout ||| normalLayout ||| fullLayout
    where
        normalLayout = spacing treeLayout
        fallbackLayout = spacing $ IfMax 2 (Tall 1 (3/100) (1/2)) Grid
        fullLayout = noBorders Full

-- Implementation of the `TreeLayout` layout

data TreeLayout a = TreeLayout
    { treelayout_tree :: (Maybe (Tree a))
    , treelayout_old_focused :: (Maybe Window)
    , treelayout_picked :: (Maybe Window)
    } deriving (Show, Read)
data Tree a = Split (Tree a) (Tree a) | Leaf Collapse a deriving (Show, Read)
data Collapse = Collapsed | Expanded deriving (Eq, Enum, Show, Read)
treeLayout = TreeLayout Nothing Nothing Nothing

data PickOrPlace = PickOrPlace Window deriving (Read, Show, Eq)
instance Message PickOrPlace
pickOrPlace = PickOrPlace

data ToggleCollapsed = ToggleCollapsed Window deriving (Read, Show, Eq)
instance Message ToggleCollapsed
toggleCollapsed = ToggleCollapsed

instance LayoutClass TreeLayout Window where
    description _ = "Tree"

    doLayout (TreeLayout tree old_focused picked) master_rect stack = do
        let tree' = update_tree tree $ W.integrate stack
        return (show_tree master_rect tree', Just $ TreeLayout tree' (Just $ W.focus stack) picked)
        where
            show_tree :: Rectangle -> Maybe (Tree Window) -> [(Window, Rectangle)]
            show_tree rect@(Rectangle x y w h) (Just (Split l r))
                = show_tree rect_l (Just l) ++ show_tree rect_r (Just r)
                where
                    ratio = case (isCollapsed l, isCollapsed r) of
                        (False, False) -> 0.5
                        (False, True) -> 0.9
                        (True, False) -> 0.1
                        (True, True) -> 0.5 -- reachable, but `rect` will already be collapsed

                    (rect_l, rect_r)
                        = if w > h
                            then ( Rectangle x y hw h
                                 , Rectangle (x + fromIntegral hw) y (w - hw) h
                                 )
                            else ( Rectangle x y w hh
                                 , Rectangle x (y + fromIntegral hh) w (h - hh)
                                 )
                    hw = ceiling $ fromIntegral w * ratio
                    hh = ceiling $ fromIntegral h * ratio

                    isCollapsed :: Tree a -> Bool
                    isCollapsed (Leaf Collapsed _) = True
                    isCollapsed (Leaf Expanded _) = False
                    isCollapsed (Split l r) = isCollapsed l && isCollapsed r
            show_tree rect@(Rectangle x y w h) (Just (Leaf _ window))
              | Just window == picked = [(window, Rectangle (x-5) (y-5) (w+10) (h+10))]
              | otherwise = [(window, rect)]
            show_tree rect Nothing = []

            update_tree :: Maybe (Tree Window) -> [Window] -> Maybe (Tree Window)
            update_tree tree stack_windows
                = foldl (\acc x -> changeFocused old_focused' (addWindow x) acc) (foldl (flip removeWindow) tree removed_windows) new_windows
                where
                    getWindowsInTree :: Maybe (Tree Window) -> [Window]
                    getWindowsInTree (Just (Split l r)) 
                        = getWindowsInTree (Just l) ++ getWindowsInTree (Just r)
                    getWindowsInTree (Just (Leaf _ w)) = [w]
                    getWindowsInTree Nothing = []

                    windows_in_tree = getWindowsInTree tree :: [Window]
                    removed_windows = windows_in_tree \\ stack_windows :: [Window]
                    new_windows = stack_windows \\ windows_in_tree :: [Window]
                    old_focused' = case old_focused of
                        Nothing -> Nothing
                        Just w -> if w `elem` removed_windows then Nothing else Just w

    pureMessage (TreeLayout tree old_focused picked) message
        | Just (ToggleCollapsed w) <- fromMessage message
            = Just $ TreeLayout (toggleCollapsed' w tree) old_focused picked
        | Just (PickOrPlace w) <- fromMessage message
            = case picked of
                Nothing -> Just $ TreeLayout tree old_focused $ Just w
                Just picked -> Just $ TreeLayout (if picked == w then tree else changeFocused (Just w) (addWindow picked) $ removeWindow picked $ tree) old_focused Nothing
        | otherwise = Nothing
        where
            toggleCollapsed' :: Eq a => a -> Maybe (Tree a) -> Maybe (Tree a)
            toggleCollapsed' w (Just (Split l r)) = toggleCollapsed' w (Just l) `merge` toggleCollapsed' w (Just r)
            toggleCollapsed' w t@(Just (Leaf c w'))
                = if w' /= w then t else Just $ flip Leaf w' $ case c of
                    Collapsed -> Expanded
                    Expanded -> Collapsed
            toggleCollapsed' w Nothing = Nothing

changeFocused :: Maybe Window -> (Maybe (Tree Window) -> Maybe (Tree Window)) -> Maybe (Tree Window) -> Maybe (Tree Window)
changeFocused focused f t@(Just (Split l r)) = changeFocused focused f (Just l) `merge` changeFocused focused f (Just r)
changeFocused focused f t@(Just (Leaf _ w)) = if Just w == focused then f t else t
changeFocused focused f Nothing = f Nothing

addWindow :: Window -> Maybe (Tree Window) -> Maybe (Tree Window)
addWindow window (Just tree) = Just $ Split (Leaf Expanded window) tree
addWindow window Nothing = Just $ Leaf Expanded window

removeWindow :: Window -> Maybe (Tree Window) -> Maybe (Tree Window)
removeWindow window (Just (Split l r))
    = removeWindow window (Just l) `merge` removeWindow window (Just r)
removeWindow window t@(Just (Leaf _ w)) = if w == window then Nothing else t
removeWindow window Nothing = Nothing

merge :: Maybe (Tree a) -> Maybe (Tree a) -> Maybe (Tree a)
Just l `merge` Just r = Just $ Split l r
Just l `merge` Nothing = Just l
Nothing `merge` Just r = Just r
Nothing `merge` Nothing = Nothing
