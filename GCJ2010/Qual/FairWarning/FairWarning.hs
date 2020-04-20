module FairWarning where

import Data.List

--{-# LANGUAGE ScopedTypeVariables #-}
--
--import System.IO
--import System.Environment
--import Control.Monad
--
--main :: IO ()
--main = do
--       [input,output] <- getArgs
--       hIn <- openFile input ReadMode
--       nbC :: Int <- hGetLine hIn >>= (return . read)
--       testCases <- mapM (const (parseCase hIn)) [1..nbC]
--       hClose hIn
--       let results = map (\(x,y) -> solve x y) testCases
--       hOut <- openFile output WriteMode
--       mapM (\(i,x) -> hPutStrLn hOut $ "Case #"++show i++": "++show x) $ zip [1..] results
--       hClose hOut
--
--parseCase :: Handle -> IO (Int,Int)
--parseCase h = hGetLine h >>= (return . (\[x,y] -> (x,y)) . map (read)  . words)

fairWarning :: [Integer] -> Integer
fairWarning xs
  | 1 `elem` xs = 0
  | otherwise = head [ y | t <- [minimum xs,(minimum xs) - 1..1],
                           (t - (sumElem `mod` t)) `mod` len == 0,
                           let  y = (t - (sumElem `mod` t)) `div` len]
                           --all (\x -> t - (x `rem` t) == y) xs]
  where sumElem = sum xs
        len = toInteger $ length xs
