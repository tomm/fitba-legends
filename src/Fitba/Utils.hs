module Fitba.Utils where

import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Control.Monad.Random as Random
 
fromFreqList :: (RandomGen g) => [(a, Double)] -> Random.Rand g (Maybe a)
fromFreqList xs =
    let s = sum (fmap snd xs)
        cums = scanl1 (\ ~(_,q) ~(y,s') -> (y, s'+q)) xs
    in case s of
        0 -> return Nothing
        _ -> Random.getRandomR (0, s) >>= \p -> (return . Just . fst . head . dropWhile ((< p) . snd)) cums

shuffle :: StdGen -> [a] -> ([a],StdGen)
shuffle gen xs =
    runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- fmap (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
    where
        n = length xs
        newArray :: Int -> [a] -> ST s (STArray s Int a)
        newArray n xs = newListArray (1,n) xs
