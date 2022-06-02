{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, BlockArguments #-}

module XMonad.Csillag.Layouts.WindowCard
    ( windowCard
    )
where

import Data.List ((\\))
import Control.Monad
import qualified Data.Map as M
import Data.Map (Map)
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Util.XUtils


data WindowCard a = WindowCard { windowcard_windowmap :: (Map Window Window), windowcard_currentwindow :: Maybe Window } deriving (Show, Read)
windowCard = ModifiedLayout $ WindowCard M.empty Nothing

instance LayoutModifier WindowCard Window where
    modifierDescription (WindowCard _ _) = "WindowCard"

    redoLayout (WindowCard windowmap _) _ stack window_rects = do
        let windows_to_remove = [(w, w') | (w, w') <- M.toList windowmap, w `notElem` map fst window_rects]
        let windows_to_add = [(w, r) | (w, r) <- window_rects, w `M.notMember` windowmap, shouldDecorate r]

        -- remove and delete decoration windows that ought to be removed:
        let windowmap' = foldl (flip M.delete) windowmap $ map fst windows_to_remove
        deleteWindows $ map snd windows_to_remove
        -- create and add decoration windows that ought to be created:
        new_windows <- forM windows_to_add $ \(_, rect@(Rectangle _ _ width height)) -> do
            w <- createNewWindow rect Nothing "#000000" True
            d <- asks display
            io $ setClassHint d w $ ClassHint "xmonad-decoration" "xmonad"

            return w
        let windowmap'' = foldl (\acc (w, w') -> M.insert w w' acc) windowmap' $ zip (fst <$> windows_to_add) new_windows

        -- finally, assemble everything together for the return
        let window_rects' = window_rects >>= (\(w, r) -> [(w, shrinkWindowRectangle r), (windowmap'' M.! w, r { rect_width = barSize })])
        let layout' = WindowCard windowmap'' $ fmap W.focus stack
        return (window_rects', Just layout')

    handleMess (WindowCard windowmap current_window) message
        | Just e <- fromMessage message = do
            handleEvent windowmap current_window e
            return Nothing
        | Just Hide <- fromMessage message = do
            hideWindows $ M.elems windowmap
            return Nothing
        | Just ReleaseResources <- fromMessage message = do
            deleteWindows $ M.elems windowmap
            return $ Just $ WindowCard M.empty current_window
        | otherwise = return Nothing

handleEvent :: Map Window Window -> Maybe Window -> Event -> X ()
handleEvent windowmap current_window event
  | PropertyEvent {ev_window = w} <- event, Just w' <- M.lookup w windowmap = redrawWindow (Just w == current_window) w'
  | ExposeEvent   {ev_window = w} <- event, Just w' <- M.lookup w windowmap = redrawWindow (Just w == current_window) w'

redrawWindow :: Bool -> Window -> X ()
redrawWindow is_current w = do
    d <- asks display
    gc <- io $ createGC d w
    io $ setGraphicsExposures d gc False

    colorGray <- stringToPixel d "#333333"
    colorRed <- stringToPixel d "#e6194B"
    colorYellow <- stringToPixel d "#ffe119"
    colorGreen <- stringToPixel d "#3cb44b"

    let button i color = io $ do
        let x = fi $ (barSize - buttonSize) `div` 2 + 1
        let y = buttonSpacing*(i+1) + fi buttonSize*i

        setForeground d gc color
        -- fillRectangle d w gc x y buttonSize buttonSize
        fillArc d w gc x y buttonSize buttonSize 0 (360*64)

    button 0 $ if is_current then colorRed else colorGray
    button 1 $ if is_current then colorYellow else colorGray
    button 2 $ if is_current then colorGreen else colorGray

    io $ freeGC d gc


shouldDecorate :: Rectangle -> Bool
shouldDecorate (Rectangle _ _ w _) = w > barSize

shrinkWindowRectangle :: Rectangle -> Rectangle
shrinkWindowRectangle (Rectangle x y w h) = Rectangle (x+fi barSize) y (w-barSize) h

barSize = 24
buttonSize = 10
buttonSpacing = 10
