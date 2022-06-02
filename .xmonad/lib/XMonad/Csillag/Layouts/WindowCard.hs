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
    , buttonSpacing :: Int
    , barButtons :: [BarButton a]
    } deriving (Show, Read)

data BarButton a = BarButton
    { button_color :: String
    , button_onPress :: a
    } deriving (Show, Read)

data DefaultButtonAction = KillWindow deriving (Show, Read)
instance ButtonAction DefaultButtonAction where
    runAction KillWindow = killWindow
defaultWindowCardConfig :: WindowCardConfig DefaultButtonAction
defaultWindowCardConfig = WindowCardConfig
    { barSize = 24
    , buttonSize = 10
    , buttonSpacing = 10
    , barButtons = [BarButton "#ff0000" KillWindow]
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
            w <- createNewWindow rect' (Just $ exposureMask .|. buttonPressMask) "#000000" True
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
handleEvent c@WindowCardConfig{buttonSize, buttonSpacing, barButtons} windowmap current_window event
    | PropertyEvent{ev_window = w} <- event, Just w' <- M.lookup w windowmap
        = redrawWindow c (Just w == current_window) w'
    | ExposeEvent{ev_window = w} <- event, Just w' <- M.lookup w windowmap
        = redrawWindow c (Just w == current_window) w'
    | ButtonEvent{ev_window = w', ev_event_type = et, ev_y = ey} <- event
    , et == buttonPress
    , Just w <- M.lookupR (DecorationWindow w') windowmap
        = considerClick 0 barButtons ey w
    | otherwise = return ()
    where
        considerClick :: ButtonAction a => Int -> [BarButton a] -> CInt -> Window -> X ()
        considerClick i ((BarButton _ action):buttons) y w
          | (i+1)*fi buttonSpacing + i*fi buttonSize <= fi y && fi y <= (i+2)*fi buttonSpacing + (i+1)*fi buttonSize = runAction action w
          | otherwise = considerClick (succ i) buttons y w
        considerClick _ [] _ _ = return () -- didn't click anything

redrawWindow :: WindowCardConfig a -> Bool -> DecorationWindow -> X ()
redrawWindow WindowCardConfig{barSize, buttonSize, buttonSpacing, barButtons} is_current (DecorationWindow w) = do
    d <- asks display
    gc <- io $ createGC d w
    io $ setGraphicsExposures d gc False

    colorGray <- stringToPixel d "#333333"

    let button i color = io $ do
        let x = fi $ (barSize - buttonSize) `div` 2
        let y = fi $ buttonSpacing*(i+1) + fi buttonSize*i

        setForeground d gc color
        -- fillRectangle d w gc x y buttonSize buttonSize
        fillArc d w gc x y (fi buttonSize) (fi buttonSize) 0 (360*64)

    forM_ (zip [0..] barButtons) \(i, BarButton color onPress) -> do
        color' <- stringToPixel d color
        button i $ if is_current then color' else colorGray

    io $ freeGC d gc


shouldDecorate :: WindowCardConfig a -> Rectangle -> Bool
shouldDecorate WindowCardConfig{barSize} (Rectangle _ _ w _) = w > fi barSize

shrinkWindowRectangle :: WindowCardConfig a -> Rectangle -> Rectangle
shrinkWindowRectangle WindowCardConfig{barSize} (Rectangle x y w h)
    = Rectangle (x + fi barSize) y (w - fi barSize) h
