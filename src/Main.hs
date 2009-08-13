module Main where

import Control.Monad.Random
import Data.Char
import Data.List
import FUtil
import qualified Data.IntMap as IntMap

lD :: Int -> [String] -> IO ()
lD n wds = do
  cols <- evalRandIO $ mapM shuffle $ IntMap.elems $ lDrop n wds
  let
    colSize = maximum $ map length cols
    colsPad = map (\ s -> s ++ replicate (colSize - length s) ' ') cols
  putStr $ unlines $ transpose colsPad
  putStrLn ""
  putStrLn ""
  putStr $ unlines $ splitN n $ map (\ c -> if c == ' ' then c else '_') $
    intercalate " " wds

lDrop :: Int -> [String] -> IntMap.IntMap [Char]
lDrop = lDropCol 0

lDropCol :: Int -> Int -> [String] -> IntMap.IntMap [Char]
lDropCol i n [] = IntMap.fromList $ zip [0..] $ replicate n []
lDropCol i n ([]:wds) = lDropCol ((i + 1) `mod` n) n wds
lDropCol i n ((c:rest):wds) = IntMap.insertWith (++) i [c] cols
  where
  cols = lDropCol ((i + 1) `mod` n) n (rest:wds)

main :: IO ()
main = do
  lD 6 $ words $ map toUpper "aggregate back-end partnerships"
