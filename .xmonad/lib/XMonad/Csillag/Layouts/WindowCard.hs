{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, BlockArguments, MultiWayIf, ViewPatterns, NamedFieldPuns #-}

module XMonad.Csillag.Layouts.WindowCard
    ( windowCard
    , WindowCard
    , WindowCardConfig(..)
    , ButtonAction(..)
    , BarButton(..)
    , defaultWindowCardConfig
    )
where

import Foreign.C.Types (CInt)
import Data.List ((\\), stripPrefix)
import Control.Monad
import qualified Data.Bimap as M
import Data.Bimap (Bimap)
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Layout.DraggingVisualizer
import XMonad.Util.XUtils


instance (Read a, Read b, Ord a, Ord b) => Read (Bimap a b) where
    readsPrec p = readParen (p > 10) $ \r -> do
        ("fromList",s) <- lex r
        (xs,t) <- reads s
        return (M.fromList xs, t)

newtype DecorationWindow = DecorationWindow { xwin :: Window } deriving (Show, Read, Eq, Ord)

class (Show a, Read a) => ButtonAction a where
    runAction :: a -> Window -> X ()

data WindowCardConfig a = WindowCardConfig
    { barSize :: Int
    , buttonSize :: Int
    , buttonSizeIncrease :: Int
    , buttonSpacing :: Int
    , barButtons :: [BarButton a]
    , dragStartAction :: a
    , dragEndAction :: a
    } deriving (Show, Read)

data BarButton a = BarButton
    { button_color :: String
    , button_hoverColor :: String
    , button_onPress :: a
    } deriving (Show, Read)

data DefaultButtonAction = KillWindow | Noop deriving (Show, Read)
instance ButtonAction DefaultButtonAction where
    runAction KillWindow = killWindow
    runAction Noop = const $ return ()
defaultWindowCardConfig :: WindowCardConfig DefaultButtonAction
defaultWindowCardConfig = WindowCardConfig
    { barSize = 24
    , buttonSize = 10
    , buttonSizeIncrease = 1
    , buttonSpacing = 10
    , barButtons = [BarButton "#cc0000" "#ff0000" KillWindow]
    , dragStartAction = Noop
    , dragEndAction = Noop

    }

data WindowCard a w = WindowCard
    { windowcard_config :: WindowCardConfig a
    , windowcard_windowmap :: (Bimap Window DecorationWindow)
    , windowcard_currentwindow :: Maybe Window
    } deriving (Read, Show)
windowCard config = ModifiedLayout $ WindowCard config M.empty Nothing

instance ButtonAction a => LayoutModifier (WindowCard a) Window where
    modifierDescription (WindowCard _ _ _) = "WindowCard"

    redoLayout (WindowCard config@WindowCardConfig{barSize} windowmap _) _ stack window_rects = do
        let windows_to_remove = [(w, w') | (w, w') <- M.toList windowmap, w `notElem` map fst window_rects]
        let windows_to_add = [(w, r) | (w, r) <- window_rects, w `M.notMember` windowmap, shouldDecorate config r]

        -- remove and delete decoration windows that ought to be removed:
        let windowmap' = foldl (flip M.delete) windowmap $ map fst windows_to_remove
        deleteWindows $ map (xwin.snd) windows_to_remove
        -- create and add decoration windows that ought to be created:
        new_windows <- forM windows_to_add $ \(_, rect@(Rectangle _ _ width height)) -> do
            let rect' = rect { rect_width = fi barSize }
            let mask = exposureMask .|. buttonPressMask .|. pointerMotionMask .|. leaveWindowMask
            w <- createNewWindow rect' (Just mask) "#000000" True
            d <- asks display
            io $ setClassHint d w $ ClassHint "xmonad-decoration" "xmonad"

            return $ DecorationWindow w
        let windowmap'' = foldl (\acc (w, w') -> M.insert w w' acc) windowmap' $ zip (fst <$> windows_to_add) new_windows

        -- finally, assemble everything together for the return
        let window_rects' = window_rects >>= (\(w, r) -> [(w, shrinkWindowRectangle config r), (xwin $ windowmap'' M.! w, r { rect_width = fi barSize })])
        let layout' = WindowCard config windowmap'' $ fmap W.focus stack
        return (window_rects', Just layout')

    handleMess (WindowCard config windowmap current_window) message
        | Just e <- fromMessage message = do
            handleEvent config windowmap current_window e
            return Nothing
        | Just Hide <- fromMessage message = do
            hideWindows $ map xwin $ M.elems windowmap
            return Nothing
        | Just ReleaseResources <- fromMessage message = do
            deleteWindows $ map xwin $ M.elems windowmap
            return $ Just $ WindowCard config M.empty current_window
        | otherwise = return Nothing

handleEvent :: ButtonAction a => WindowCardConfig a -> Bimap Window DecorationWindow -> Maybe Window -> Event -> X ()
handleEvent c@WindowCardConfig{buttonSize, buttonSpacing, barButtons, dragStartAction, dragEndAction} windowmap current_window event
    | PropertyEvent{ev_window = w} <- event, Just w' <- M.lookup w windowmap
        = redrawWindow c (Just w == current_window) Nothing w'
    | ExposeEvent{ev_window = w} <- event, Just w' <- M.lookup w windowmap
        = redrawWindow c (Just w == current_window) Nothing w'
    | ButtonEvent{ev_window = w', ev_event_type = et, ev_y = ey} <- event
    , et == buttonPress
    , Just w <- M.lookupR (DecorationWindow w') windowmap
        = considerClick 0 barButtons ey w
    | MotionEvent{ev_window = w', ev_event_type = et, ev_y} <- event
    , et == motionNotify
    , Just w <- M.lookupR (DecorationWindow w') windowmap
        = redrawWindow c (Just w == current_window) (Just ev_y) (DecorationWindow w')
    | CrossingEvent{ev_window = w', ev_event_type = et} <- event
    , et == leaveNotify
    , Just w <- M.lookupR (DecorationWindow w') windowmap
        = redrawWindow c (Just w == current_window) Nothing (DecorationWindow w')
    | otherwise = return ()
    where
        considerClick :: ButtonAction a => Int -> [BarButton a] -> CInt -> Window -> X ()
        considerClick i ((BarButton _ _ action):buttons) y w
          | (i+1)*fi buttonSpacing + i*fi buttonSize <= fi y && fi y <= (i+2)*fi buttonSpacing + (i+1)*fi buttonSize = runAction action w
          | otherwise = considerClick (succ i) buttons y w
        considerClick _ [] _ w = do -- pressed on the bar, not on a button
            d <- asks display
            (_, _, _, win_w, win_h, _, _) <- io $ getGeometry d w

            runAction dragStartAction w
            mouseDrag (\x y -> sendMessage $ DraggingWindow w $ Rectangle x y win_w win_h) do
                sendMessage DraggingStopped
                runAction dragEndAction w

redrawWindow :: WindowCardConfig a -> Bool -> Maybe CInt -> DecorationWindow -> X ()
redrawWindow WindowCardConfig{barSize, buttonSize, buttonSizeIncrease, buttonSpacing, barButtons} is_current maybe_y (DecorationWindow w) = do
    d <- asks display
    gc <- io $ createGC d w
    io $ setGraphicsExposures d gc False

    colorBlack <- stringToPixel d "#000000"
    colorGray <- stringToPixel d "#333333"

    let button i hoverColor color = io $ do
        let x = fi $ (barSize - buttonSize) `div` 2
        let y = fi $ buttonSpacing*(i+1) + fi buttonSize*i

        let is_focused = case maybe_y of
                            Just y' | fi y <= y' && y' <= fi y + fi buttonSize -> True
                            _ -> False

        setForeground d gc colorBlack
        fillRectangle d w gc (x-fi buttonSizeIncrease) (y-fi buttonSizeIncrease) (fi buttonSize + 2*fi buttonSizeIncrease) (fi buttonSize + 2*fi buttonSizeIncrease)

        setForeground d gc $ if is_focused then hoverColor else color

        -- fillRectangle d w gc x y buttonSize buttonSize
        if is_focused
           then fillArc d w gc (x-fi buttonSizeIncrease) (y-fi buttonSizeIncrease) (fi buttonSize + 2*fi buttonSizeIncrease) (fi buttonSize + 2*fi buttonSizeIncrease) 0 (360*64)
           else fillArc d w gc x y (fi buttonSize) (fi buttonSize) 0 (360*64)

    forM_ (zip [0..] barButtons) \(i, BarButton color hoverColor onPress) -> do
        color' <- stringToPixel d color
        hoverColor' <- stringToPixel d hoverColor
        button i hoverColor' $ if is_current then color' else colorGray

    io $ freeGC d gc


shouldDecorate :: WindowCardConfig a -> Rectangle -> Bool
shouldDecorate WindowCardConfig{barSize} (Rectangle _ _ w _) = w > fi barSize

shrinkWindowRectangle :: WindowCardConfig a -> Rectangle -> Rectangle
shrinkWindowRectangle WindowCardConfig{barSize} (Rectangle x y w h)
    = Rectangle (x + fi barSize) y (w - fi barSize) h
