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


setStyle [black,_] Empty  = setColor black >> setAttribute AttributeBold False
setStyle [_,green] Normal = setColor green >> setAttribute AttributeBold False
setStyle [_,green] Bright = setColor green >> setAttribute AttributeBold True
setStyle _         _      = return ()

drawSingleChar _      _ _ _  Null = return ()
drawSingleChar colors y x ch s = moveCursor y x >> setStyle colors s >> drawText (pack [ch])    

deepzip f back mask = [ f y x c s | (y,(bl,ml)) <- zip [0..] (zip back mask)
                                , (x,(c,s)) <- zip [0..] (zip bl ml)]

redrawMatrix colors = (sequence_.) . deepzip (drawSingleChar colors)

nextFrame = map tail

displayFrame win colors back mask = do
    updateWindow win $ redrawMatrix colors back mask
    render
    liftIO $ threadDelay delay
    displayFrame win colors back (nextFrame mask)

main = runCurses $ do
    (y, x') <- screenSize
    let x = x' - 1 -- work around UI.Ncurses bug
    colors <- createColors
    win    <- defaultWindow
    let back = backdrop y x message
    mask <- liftrand . prseq $ replicate (fi x) column
    displayFrame win colors back (transpose mask)
