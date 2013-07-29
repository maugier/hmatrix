{-#LANGUAGE NoMonomorphismRestriction #-}

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Data.Array.IO
import Data.Char
import Data.IORef
import Data.List (transpose)
import Data.Text (pack)
import UI.NCurses
import System.Random

-- config

message = "+*=-.;THEGAME"

delay = 50000

on = (1, 7)
off = (9, 19)

-- end config

data Cell = Empty | Normal | Bright | Null
	deriving Show

splitr x = getSplit >>= return . evalRand x

-- Permet de faire tourner en parallèle des structures
-- de données aléatoires et infinies
prseq = mapM splitr

column :: (MonadRandom m, Functor m) => m [Cell]
column = fmap concat . sequence . repeat $ do
	e <- getRandomR off
	n <- getRandomR on
	return ([Empty] ++ replicate e Null
                        ++ [Bright, Normal]
                        ++ replicate n Null)


slice n = map (take n) . takeWhile (not.null) . iterate (drop n)

fi = fromInteger

backdrop y x = take (fi y) . slice (fi x) . cycle

createColors = sequence [ newColorID ColorBlack ColorBlack 1 
                        , newColorID ColorGreen ColorBlack 2 ]

mutateArray f a = getBounds a >>= mapM_ (\i -> readArray a i >>= f >>= writeArray a i) . range

updateCounter :: (Int, Cell) -> IO (Int, Cell)
updateCounter (0, Empty) = return (0, Bright)
updateCounter (0, Bright) = randomRIO on >>= \x -> return (x, Normal)
updateCounter (0, Normal) = randomRIO off >>= \x -> return (x, Empty)
updateCounter (n, s) = return (n-1, s)

setStyle [black,_] Empty  = setColor black >> setAttribute AttributeBold False
setStyle [_,green] Normal = setColor green >> setAttribute AttributeBold False
setStyle [_,green] Bright = setColor green >> setAttribute AttributeBold True
setStyle _         _      = return ()

drawSingleChar _      _ _ _  Null = return ()
drawSingleChar colors y x ch s = moveCursor y x >> setStyle colors s >> drawText (pack [ch])	

deepzip f back mask = [ f y x c s | (y,(bl,ml)) <- zip [0..] (zip back mask)
                                , (x,(c,s)) <- zip [0..] (zip bl ml)]

roll m l = modifyIORef m ((l:) . init)

redrawMatrix colors = (sequence_.) . deepzip (drawSingleChar colors)

initcounters :: Integer -> IO (IOArray Integer (Int,Cell))
initcounters width = newArray (0, width - 1) (0, Normal)

main = runCurses $ do
	(y, x') <- screenSize
	let x = x' - 1 -- work around UI.Ncurses bug
	colors <- createColors
	win    <- defaultWindow
	counters <- liftIO $ initcounters x
	let back = backdrop y x message
	mask <- liftIO $ newIORef (backdrop y x [Empty])
	forever $ do
		liftIO $ do 
			mutateArray updateCounter counters
			getElems counters >>= roll mask . map snd
		liftIO (readIORef mask) >>= (updateWindow win . redrawMatrix colors back)
		render
		liftIO $ threadDelay delay
