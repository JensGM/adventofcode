{-# LANGUAGE LambdaCase #-}
import Data.List.Split
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Debug.Trace as D (trace)

main = do
    c <- getContents
    run part1 c
    run part2 c
run f = putStrLn . show . f . parse

-------------------------------------------------------------------------------

parse = map (map (\x -> read [x] :: Int))
      . lines

-- Part 1

part1 m = sum . map risk .  map (height m) $ lowPoints m

risk :: Int -> Int
risk height = 1 + height

height :: [[Int]] -> (Int, Int) -> Int
height m (x, y) = m !! y !! x

lowPoints :: [[Int]] -> [(Int, Int)]
lowPoints m = filter allNeighboursHigher points where
    nrows = length m
    ncols = length (head m)
    points = [(x, y) | x <- [0..ncols - 1], y <- [0..nrows - 1]]
    allNeighboursHigher p = all (\q -> height m q > height m p) (n p)
    n = neighbours nrows ncols

neighbours :: Int -> Int -> (Int, Int) -> [(Int, Int)]
neighbours nrows ncols (x, y) = filter withinBounds n where
    n = [(x + 𝛅x, y + 𝛅y) | (𝛅x, 𝛅y) <- [(-1, 0), (0, 1), (1, 0), (0, -1)]]
    withinBounds (x, y) = 0 <= x && x < ncols && 0 <= y && y < nrows

-- Part 2

part2 m = product . take 3 . reverse . sort . map (basinSize m) $ lowPoints m

basinSize :: [[Int]] -> (Int, Int) -> Int
basinSize m p = length (basin nrows ncols m [p] Set.empty) where
    nrows = length m
    ncols = length (head m)

basin :: Int
      -> Int
      -> [[Int]]
      -> [(Int, Int)]
      -> Set.Set (Int, Int)
      -> [(Int, Int)]
basin _ _ _ [] seen = Set.toList seen
basin nrows ncols m next seen =
    basin nrows ncols m next' seen' where
        next' = filter isRelevant . concat $ map (neighbours nrows ncols) next
        seen' = seen `Set.union` Set.fromList next
        isRelevant p = not (Set.member p seen) && height m p < 9
