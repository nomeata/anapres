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

type LetterState = ( [Trans], [Trans], [Trace], Int )


transtime = 1500
tracetime = 10000


empty :: Trans
empty = const []
initState :: [Trans] -> LetterState
initState xs = ([],xs,[],0)
next :: LetterState -> LetterState
next (ys,[],t,n)   = (ys,[],t,n)
next (ys,x:xs,t,n) = (x:ys,xs,t,n)
back :: LetterState -> LetterState
back ([],xs,t,n)   = ([],xs,t,n)
back (y:ys,xs,t,n) = (ys,y:xs,t,n)
curr :: LetterState -> Trans
curr (_, [],_, _) = empty
curr (_,x:_,_, _) = x
traces :: LetterState -> [Trace]
traces (_, _, t, _) = t
addTrace :: Trace -> LetterState -> LetterState
addTrace t (ys,xs,ts,n) = (ys, xs, t:ts, n)
skips :: LetterState -> Int
skips (_, _, _, n) = n
modSkips :: (Int -> Int) -> LetterState -> LetterState
modSkips f (ys, xs, ts, n) = (ys, xs, ts, f n)

type Direction = Double -> Double
fwd :: Direction
fwd i = i
rev :: Direction
rev i = (1-i)

mil_sec_gone (TOD s1 p1) (TOD s2 p2) = (s2 * 10^3 + p2 `div` 10^9) - (s1 * 10^3 + p1 `div` 10^9)

main = do
	[wordfile] <- getArgs
	trans <- readAnaPres wordfile

	currentConfig <- newIORef (initState trans)
	transState    <- newIORef (Nothing :: Maybe TransState)

	frameRateRounter <- getFrameRateCounter

        initGUI
        window <- windowNew
        canvas <- drawingAreaNew
        -- widgetSetSizeRequest window width height

	let draw_trans = do
		lt <- readIORef transState 
		state <- readIORef currentConfig
		now <- getClockTime
		render canvas $ do

		save
        	setSourceRGB 0.6 0.6 0.6
		mapM_ (drawT now) $ traces state
        	restore

		case lt of 
		    Nothing ->	drawC $ curr state 1
		    Just (time, trans) -> do
		  	let s = mil_sec_gone time now
			if s > transtime then do
				if skips state > 0
				  then do
					drawC $ curr state 1
					liftIO $ do
						let imp = filter (cI.snd) $ curr state 1
						modifyIORef currentConfig (addTrace (now,imp))
						modifyIORef currentConfig next
						modifyIORef currentConfig (modSkips (subtract 1))
						state <- readIORef currentConfig
						writeIORef transState (Just (now, curr state))
				  else do
					let imp = filter (cI.snd) $ curr state 1
					liftIO $ modifyIORef currentConfig (addTrace (now,imp))
					liftIO $ writeIORef transState Nothing
					drawC $ curr state 1
			  else  let d = fromIntegral s / fromIntegral transtime
			        in drawC $ trans d

        onButtonPress window $ \e -> do
		let but = eventButton e
		unless (but == MiddleButton) $ do
			now <- getClockTime
			case but of
			    LeftButton -> do
				modifyIORef currentConfig next
				modifyIORef currentConfig (modSkips (const 0))
				state <- readIORef currentConfig
				writeIORef transState (Just (now, curr state))
			    RightButton -> do
				state <- readIORef currentConfig
				modifyIORef currentConfig back
				modifyIORef currentConfig (modSkips (const 0))
				writeIORef transState (Just (now, curr state . rev))

			widgetQueueDraw canvas
		return True

        onKeyPress window $ \e -> do
		when (eventKeyName e `elem` words "Escape q Q") $ widgetDestroy window
		when (eventKeyName e == "space") $ do
			lt <- readIORef transState 
			case lt of
			    Nothing -> do
				now <- getClockTime
				modifyIORef currentConfig next
				modifyIORef currentConfig (modSkips (const 0))
				state <- readIORef currentConfig
				writeIORef transState (Just (now, curr state))
		            Just _ -> do
				modifyIORef currentConfig (modSkips (+1))
		return True
        onDestroy window mainQuit
        onExpose canvas $ const $ do
		--frameRateRounter
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

drawT now (start,letters) = do
	let time = mil_sec_gone start now
	when (time < tracetime) $ do
	mapM_ (drawLetter . traceAway (fromIntegral time/fromIntegral tracetime)) letters

traceAway :: Double -> (Char, Coord) -> (Char, Coord)
traceAway d (l,c) = (l, c
		{ cY = cY c - d * height
		, cX = width/2 + (1-d) * (cX c - width/2)
		, cS = (1-d) * cS c
		, cMW = (1-d) * cMW c
		})
	
drawbg = do
        save
        setSourceRGB 1 1 1
        paint
	restore

getFrameRateCounter :: IO (IO ())
getFrameRateCounter = do
	now <- getClockTime
	lastRef <- newIORef now
	return $ do
		now <- getClockTime
		last <- readIORef lastRef
		putStrLn $ "Framerate: " ++ show (1000 / fromIntegral (mil_sec_gone last now))
		writeIORef lastRef now

