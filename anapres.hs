import Graphics.UI.Gtk
import Graphics.Rendering.Cairo 
import Text.Printf
import Data.IORef
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import System
import System.Time

import AnaPresParse
import TransMaker


transtime = 1500


empty :: Trans
empty = const []
initState :: [Trans] -> LetterState
initState xs = ([],xs,[])
next :: LetterState -> LetterState
next (ys,[],t)   = (ys,[],t)
next (ys,x:xs,t) = (x:ys,xs,t)
back :: LetterState -> LetterState
back ([],xs,t)   = ([],xs,t)
back (y:ys,xs,t) = (ys,y:xs,t)
curr :: LetterState -> Trans
curr (_, [],_) = empty
curr (_,x:_,_) = x
traces :: LetterState -> [Trace]
traces (_, _, t) = t
addTrace :: Trace -> LetterState -> LetterState
addTrace t (ys,xs,ts) = (ys, xs, t:ts)

type Direction = Double -> Double
fwd :: Direction
fwd i = i
rev :: Direction
rev i = (1-i)

mic_sec_gone (TOD s1 p1) (TOD s2 p2) = (s2 * 10^3 + p2 `div` 10^9) - (s1 * 10^3 + p1 `div` 10^9)

main = do
	[wordfile] <- getArgs
	trans <- readAnaPres wordfile

	currentConfig <- newIORef (initState trans)
	transState    <- newIORef (Nothing :: Maybe TransState)

        initGUI
        window <- windowNew
        canvas <- drawingAreaNew
        -- widgetSetSizeRequest window width height

	let draw_trans = do
		lt <- readIORef transState 
		state <- readIORef currentConfig
		case lt of 
		    Nothing ->	render canvas $ drawC $ curr state 1
		    Just (time, trans) -> do
		  	now <- getClockTime
		  	let s = mic_sec_gone time now
			if s > transtime then do
				let imp = filter (cI.snd) $ curr state 1
				modifyIORef currentConfig (addTrace (now,imp))
				writeIORef transState Nothing
				render canvas $ drawC $ curr state 1
			  else  let d = fromIntegral s / fromIntegral transtime
			        in render canvas $ drawC $ trans d

        onButtonPress window $ \e -> do
		let but = eventButton e
		unless (but == MiddleButton) $ do
			now <- getClockTime
			case but of
			    LeftButton -> do
				modifyIORef currentConfig next
				state <- readIORef currentConfig
				writeIORef transState (Just (now, curr state))
			    RightButton -> do
				state <- readIORef currentConfig
				modifyIORef currentConfig back
				writeIORef transState (Just (now, curr state . rev))

			widgetQueueDraw canvas
		return True

        onKeyPress window $ \e -> do
		when (eventKeyName e `elem` words "Escape q Q") $ widgetDestroy window
		return True
        onDestroy window mainQuit
        onExpose canvas $ const $ do
		draw_trans
		return True

	flip timeoutAdd 30 $ do 
		widgetQueueDraw canvas
		return True

        set window [containerChild := canvas]
	windowFullscreen window
        widgetShowAll window
        mainGUI

render canvas r = do
        win <- widgetGetDrawWindow canvas
        (w, h) <- widgetGetSize canvas
	let sx = fromIntegral w / width
	let sy = fromIntegral h / height
        renderWithDrawable win $ do
		drawbg 
		selectFontFace "Mono" FontSlantNormal FontWeightNormal
		scale sx sy
		r

drawC config = do
	mapM_ drawLetter config

drawLetter (l, c) = do
	unless (cF c == 0) $ do
		save
		setFontSize (cS c)
		TextExtents _ _ w h _ _ <- textExtents [l]
		translate (cX c) (cY c)
		scale (min (cF c) (cMW c/w)) 1
		moveTo (-w/2) (h/2)
		showText [l]
		restore

drawbg = do
        save
        setSourceRGB 1 1 1
        paint
	restore

