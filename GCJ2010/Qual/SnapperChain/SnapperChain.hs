{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import System.Environment
import Control.Monad

data SnapState = ON | OFF deriving (Eq, Show)

main :: IO ()
main = do
       [input,output] <- getArgs
       hIn <- openFile input ReadMode
       nbC :: Int <- hGetLine hIn >>= (return . read)
       testCases <- mapM (const (parseCase hIn)) [1..nbC]
       hClose hIn
       let results = map (\(x,y) -> slowSnapperChain x y) testCases
       hOut <- openFile output WriteMode
       mapM (\(i,x) -> hPutStrLn hOut $ "Case #"++show i++": "++show x) $ zip [1..] results
       hClose hOut

parseCase :: Handle -> IO (Int,Int)
parseCase h = hGetLine h >>= (return . (\[x,y] -> (x,y)) . map (read)  . words)

fastSnapperChain :: Int -> Int -> SnapState
fastSnapperChain n k
  | (k `rem` (2 ^ n)) == (2 ^ n)-1 = ON
  | otherwise = OFF

slowSnapperChain :: Int -> Int -> SnapState
slowSnapperChain _ 0 = OFF
slowSnapperChain 1 1 = ON
slowSnapperChain nbSnap nbFingSnap
  | all (== ON) $ update nbFingSnap (take nbSnap $ repeat OFF) = ON
  | otherwise = OFF

update :: Int -> [SnapState] -> [SnapState]
update 0 snaps = snaps
update n snaps = update (n-1) $
                        case suff of
                              [] -> (map (const OFF) pref)
                              x:xs -> (map (const OFF) pref) ++ (ON:xs)
                 where (pref,suff) = span (== ON) snaps

