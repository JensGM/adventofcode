{-# LANGUAGE LambdaCase #-}
import Data.List.Split
import Data.List
import qualified Data.Map as Map
import qualified Debug.Trace as D (trace)

main = do
    c <- getContents
    run part1 c
    run part2 c
run f = putStrLn . show . f . parse

-------------------------------------------------------------------------------

parse = map (\x -> (head x, length x))
      . group
      . sort
      . map (\x -> read x :: Int)
      . splitOn ","

part1 l = minimum [part1' p l | p <- [fst..lst]] where
    (fst, _) = head l 
    (lst, _) = last l
part1' p ((x, count):xs) = count * abs (p - x) + part1' p xs
part1' _ [] = 0

part2 l = minimum [part2' p l | p <- [fst..lst]] where
    (fst, _) = head l 
    (lst, _) = last l
part2' p ((x, count):xs) = count * (sum [0..abs (p - x)]) + part2' p xs 
part2' _ [] = 0
