import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.State
import Data.Text (pack)
import UI.NCurses
import System.Random

-- config

hira = ['ぁ'..'ゔ']
kata = ['ァ'..'ヺ']

message = kata ++ "THEGAME"

delay = 500

on = (4, 10)
off = (2, 20)

-- end config

data Cell = CEmpty | CNormal | CBright

slice n = map (take n) . takeWhile (not.null) . iterate (drop n)

fi = fromInteger

backdrop x y = take (fi y) . slice (fi x) . cycle

allblack x y = replicate (fi y) (replicate (fi x) CEmpty)

createColors = sequence [ newColorID ColorRed ColorBlack 1 
                        , newColorID ColorGreen ColorBlack 2 ]

crand :: (Integer,Integer) -> StateT a Curses Integer
crand = lift . liftIO . randomRIO

setStyle [black,_] CEmpty  = setColor black
setStyle [_,green] CNormal = setColor green >> setAttribute AttributeBold False
setStyle [_,green] CBright = setColor green >> setAttribute AttributeBold True


drawSingleChar colors x y ch s = setStyle colors s >> moveCursor x y >> drawText (pack [ch])	

deepzip f back mask = [ f x y c s | (x,(bl,ml)) <- zip [0..] (zip back mask)
                                , (y,(c,s)) <- zip [0..] (zip bl ml)]

redrawMatrix colors back mask = sequence_ $ deepzip (drawSingleChar colors) back mask


renderAndPause = render >> liftIO (threadDelay delay)

main = runCurses $ do
	--(x, y) <- screenSize
	let (x,y) = (10,10)
	colors <- createColors
	win    <- defaultWindow
	let back = backdrop x y message
	let mask = backdrop x y [CEmpty, CNormal, CBright]
	flip runStateT (allblack x y) . forever $ do
		x <- crand (10,20)
		mask <- get
		lift . updateWindow win $ redrawMatrix colors back mask
		lift renderAndPause
