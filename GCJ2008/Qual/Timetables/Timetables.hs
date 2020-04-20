{-# LANGUAGE ScopedTypeVariables #-}

-- Based on the solution here: https://crypto.stanford.edu/~blynn/haskell/2008-qual.html

import System.IO
import System.Environment
import Control.Monad

import Data.List

data Trip = Trip { start :: Int,
                   end :: Int} deriving (Eq,Show)

data Testcase = Testcase { fromA :: [Trip],
                           fromB :: [Trip]}

data Dir = Dep | Arr

main :: IO ()
main = do
       [input,output] <- getArgs
       hIn <- openFile input ReadMode
       nbC :: Int <- hGetLine hIn >>= (return . read)
       testCases <- mapM (const (parseCase hIn)) [1..nbC]
       hClose hIn
       let results = map (\x -> timetables (fromA x) (fromB x)) testCases
       hOut <- openFile output WriteMode
       mapM (\(i,(x,y)) -> hPutStrLn hOut $ "Case #"++show i++": "++show x++ " " ++ show y)
            $ zip [1..] results
       hClose hOut

parseCase :: Handle -> IO Testcase
parseCase h = do
              turn :: Int <- hGetLine h >>= (return . read)
              [nbA,nbB] :: [Int] <- hGetLine h >>= (return . map read . words)
              tripsA <- replicateM nbA (hGetLine h >>=
                                       return .
                                       (\[s,e] -> Trip s (e+turn))
                                       . map (\time -> let (hours,':':mins) = break (== ':') time
                                                       in  (read hours)*60
                                                           + (read mins))
                                       . words)
              tripsB <- replicateM nbB (hGetLine h >>=
                                       return .
                                       (\[s,e] -> Trip s (e+turn))
                                       . map (\time -> let (hours,':':mins) = break (== ':') time
                                                       in  (read hours)*60
                                                           + (read mins))
                                       . words)
              return $ Testcase tripsA tripsB



timetables :: [Trip] -> [Trip] -> (Int, Int)
timetables tripsA tripsB =
  (fst $
   foldl (\(nbT,nbA) (dir,time) -> case dir of
                                        Dep -> if nbA > 0 then (nbT,nbA-1)
                                                          else (nbT+1,nbA)
                                        Arr -> (nbT,nbA+1))
         (0,0)
         $ sortBy (\(dx,x) (dy,y) -> if x /= y then compare x y
                                               else case (dx,dy) of
                                                         (Dep,Arr) -> GT
                                                         (Arr,Dep) -> GT
                                                         _ -> EQ)
                  (map (\t -> (Dep,start t)) tripsA ++ (map (\t -> (Arr,end t)) tripsB)),
   fst $
   foldl (\(nbT,nbA) (dir,time) -> case dir of
                                        Dep -> if nbA > 0 then (nbT,nbA-1)
                                                          else (nbT+1,nbA)
                                        Arr -> (nbT,nbA+1))
         (0,0)
         $ sortBy (\(dx,x) (dy,y) -> if x /= y then compare x y
                                               else case (dx,dy) of
                                                         (Dep,Arr) -> GT
                                                         (Arr,Dep) -> GT
                                                         _ -> EQ)
                  (map (\t -> (Dep,start t)) tripsB ++ (map (\t -> (Arr,end t)) tripsA)))
