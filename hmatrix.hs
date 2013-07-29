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

on = (2, 8)
off = (10, 20)

-- end config

data Cell = Empty | Normal | Bright
	deriving Show

column :: MonadRandom m => m [Cell]
column = do
	e <- getRandomR off
	n <- getRandomR on
	return (replicate e Empty ++ [Bright] ++ replicate n Normal)


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
