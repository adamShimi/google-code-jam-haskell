{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import System.Environment
import Control.Monad

import Data.List (span, maximumBy)

data TestCase = TestCase { nbSearch :: Int,
                           searchs :: [String],
                           nbQueries :: Int,
                           queries :: [String]} deriving (Eq,Show)

main :: IO ()
main = do
       [input,output] <- getArgs
       hIn <- openFile input ReadMode
       nbC :: Int <- hGetLine hIn >>= (return . read)
       testCases <- parseCases nbC hIn
       hClose hIn
       let results = solve testCases
       hOut <- openFile output WriteMode
       mapM (\(i,x) -> hPutStrLn hOut $ "Case #"++show i++": "++show x) $ zip [1..] results
       hClose hOut

parseCases :: Int -> Handle -> IO [TestCase]
parseCases 0 _ = return []
parseCases n h = do
                 nbS :: Int <- hGetLine h >>= (return . read)
                 s <- replicateM nbS (hGetLine h)
                 nbQ :: Int <- hGetLine h >>= (return . read)
                 q <- replicateM nbQ (hGetLine h)
                 nexts <- parseCases (n-1) h
                 return ((TestCase nbS s nbQ q):nexts)

solve :: [TestCase] -> [Int]
solve = map (\x -> if x == 0 then 0 else x-1)
        . map (\test -> solveTest (searchs test) (queries test))
          where solveTest :: [String] -> [String] -> Int
                solveTest _ [] = 0
                solveTest s qs =
                  1 + solveTest s suff
                  where (_,suff) = maximumBy (\(_,suff1) (_,suff2) -> compare (length suff2)
                                                                              (length suff1))
                                             [(searchE,suff) | searchE <- s,
                                                               let (_,suff) = span (/= searchE) qs]
