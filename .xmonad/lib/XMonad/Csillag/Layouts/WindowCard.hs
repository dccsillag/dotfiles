{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, BlockArguments, MultiWayIf, ViewPatterns #-}

module XMonad.Csillag.Layouts.WindowCard
    ( windowCard
    )
where

import Data.List ((\\), stripPrefix)
import Control.Monad
import qualified Data.Bimap as M
import Data.Bimap (Bimap)
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Util.XUtils


instance (Read a, Read b, Ord a, Ord b) => Read (Bimap a b) where
    readsPrec p = readParen (p > 10) $ \r -> do
        ("fromList",s) <- lex r
        (xs,t) <- reads s
        return (M.fromList xs, t)

newtype DecorationWindow = DecorationWindow { xwin :: Window } deriving (Show, Read, Eq, Ord)

data WindowCard a = WindowCard
    { windowcard_windowmap :: (Bimap Window DecorationWindow)
    , windowcard_currentwindow :: Maybe Window
    } deriving (Read, Show)
windowCard = ModifiedLayout $ WindowCard M.empty Nothing

instance LayoutModifier WindowCard Window where
    modifierDescription (WindowCard _ _) = "WindowCard"

    redoLayout (WindowCard windowmap _) _ stack window_rects = do
        let windows_to_remove = [(w, w') | (w, w') <- M.toList windowmap, w `notElem` map fst window_rects]
        let windows_to_add = [(w, r) | (w, r) <- window_rects, w `M.notMember` windowmap, shouldDecorate r]

        -- remove and delete decoration windows that ought to be removed:
        let windowmap' = foldl (flip M.delete) windowmap $ map fst windows_to_remove
        deleteWindows $ map (xwin.snd) windows_to_remove
        -- create and add decoration windows that ought to be created:
        new_windows <- forM windows_to_add $ \(_, rect@(Rectangle _ _ width height)) -> do
            let rect' = rect { rect_width = barSize }
            w <- createNewWindow rect' (Just $ exposureMask .|. buttonPressMask) "#000000" True
            d <- asks display
            io $ setClassHint d w $ ClassHint "xmonad-decoration" "xmonad"

            return $ DecorationWindow w
        let windowmap'' = foldl (\acc (w, w') -> M.insert w w' acc) windowmap' $ zip (fst <$> windows_to_add) new_windows

        -- finally, assemble everything together for the return
        let window_rects' = window_rects >>= (\(w, r) -> [(w, shrinkWindowRectangle r), (xwin $ windowmap'' M.! w, r { rect_width = barSize })])
        let layout' = WindowCard windowmap'' $ fmap W.focus stack
        return (window_rects', Just layout')

    handleMess (WindowCard windowmap current_window) message
        | Just e <- fromMessage message = do
            handleEvent windowmap current_window e
            return Nothing
        | Just Hide <- fromMessage message = do
            hideWindows $ map xwin $ M.elems windowmap
            return Nothing
        | Just ReleaseResources <- fromMessage message = do
            deleteWindows $ map xwin $ M.elems windowmap
            return $ Just $ WindowCard M.empty current_window
        | otherwise = return Nothing

handleEvent :: Bimap Window DecorationWindow -> Maybe Window -> Event -> X ()
handleEvent windowmap current_window event
    | PropertyEvent{ev_window = w} <- event, Just w' <- M.lookup w windowmap
        = redrawWindow (Just w == current_window) w'
    | ExposeEvent{ev_window = w} <- event, Just w' <- M.lookup w windowmap
        = redrawWindow (Just w == current_window) w'
    | ButtonEvent{ev_window = w', ev_event_type = et, ev_y = ey} <- event
    , et == buttonPress
    , Just w <- M.lookupR (DecorationWindow w') windowmap
        = if
            | 1*buttonSpacing + 0*fi buttonSize <= ey && ey <= 2*buttonSpacing + 1*fi buttonSize
                -> spawn "notify-send windowcard 1"
            | 2*buttonSpacing + 1*fi buttonSize <= ey && ey <= 3*buttonSpacing + 2*fi buttonSize
                -> spawn "notify-send windowcard 2"
            | 3*buttonSpacing + 2*fi buttonSize <= ey && ey <= 4*buttonSpacing + 3*fi buttonSize
                -> spawn "notify-send windowcard 3"
            | otherwise -> return ()
    | otherwise = return ()

redrawWindow :: Bool -> DecorationWindow -> X ()
redrawWindow is_current (DecorationWindow w) = do
    d <- asks display
    gc <- io $ createGC d w
    io $ setGraphicsExposures d gc False

    colorGray <- stringToPixel d "#333333"
    colorRed <- stringToPixel d "#e6194B"
    colorYellow <- stringToPixel d "#ffe119"
    colorGreen <- stringToPixel d "#3cb44b"

    let button i color = io $ do
        let x = fi $ (barSize - buttonSize) `div` 2
        let y = fi $ buttonSpacing*(i+1) + fi buttonSize*i

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
