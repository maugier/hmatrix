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
    deriving (Show, Eq)

-- Evalue une structure de données aléatoire en
-- splittant l'état du RNG - ceci permet de générer
-- une structure de données aléatoire infinie sans
-- bloquer le RNG courant
splitr x = getSplit >>= return . evalRand x

prseq = mapM splitr

liftrand = liftIO . evalRandIO

-- Génère une colonne de cellules Matrix-style, avec
-- une quantité aléatoire de blancs, une cellule claire,
-- et une quantité aléatoire de cellules foncées
column :: (MonadRandom m, Functor m) => m [Cell]
column = fmap concat . sequence . repeat $ do
    e <- getRandomR off
    n <- getRandomR on
    return ([Empty] ++ replicate e Null
                    ++ [Bright, Normal]
                    ++ replicate n Null)

-- Découpe une liste en morceaux de longueur n
slice n =  map (take n) . takeWhile (not.null) . iterate (drop n)

fi = fromInteger

-- Crée le fond en répétant le message horizontalement, on le transpose
backdrop y x = take (fi y) . slice (fi x) . cycle

createColors = sequence [ newColorID ColorBlack ColorBlack 1 
                        , newColorID ColorGreen ColorBlack 2 ]

setColorBold c b = setColor c >> setAttribute AttributeBold b

main = runCurses $ do
    (height, width') <- screenSize
    let width = width' - 1 -- work around UI.Ncurses bug

    [black, green] <- createColors
    win    <- defaultWindow
    let back = backdrop height width message
    mask <- liftrand . prseq $ replicate (fi width) column

    let setStyle = \s -> case s of
                             Empty  -> setColorBold black False
                             Normal -> setColorBold green False
                             Bright -> setColorBold green True
                             _      -> return ()

    let drawSingleChar y x ch s = when (s /= Null) $ do
        moveCursor y x
        setStyle s
        drawText (pack [ch])

    let redrawMatrix back mask = forM_ (zip3 (reverse [0..height-1]) back mask) (\(y,bl,ml) -> 
                                     forM_ (zip3 [0..width-1] bl ml) (\(x,b,m) -> 
                                         drawSingleChar y x b m))

    let displayFrame mask = do  
        updateWindow win $ redrawMatrix back mask
        render
        liftIO $ threadDelay delay

    mapM_ displayFrame (iterate tail (transpose mask)) where

