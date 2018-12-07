{-# LANGUAGE LambdaCase #-}
import Data.List.Split
import Data.List
import Data.Matrix
import qualified Debug.Trace as D (trace)

main = do
    c <- getContents
    run part1 c
    run part2 c
run func = putStrLn . show . func . parse

-------------------------------------------------------------------------------
parse :: String -> [((Int, Int), (Int, Int))]
parse = map parse_points . (map (splitOn " -> ")) . lines
parse_points [a, b] = (point a, point b) where
    point = \s -> let
        p = map (\x -> read x :: Int) $ splitOn "," s in (p!!0, p!!1)

-- Part 1
part1 input = atleastTwo where
    atleastTwo = sum . (map (\_ -> 1)) . (filter (>=2)) $ concat chart
    chart = plot (ventPts False input) [[0 | n' <- [0..n]] | m' <- [0..m]] 
    (n, m) = bounds input

bounds :: [((Int, Int), (Int, Int))] -> (Int, Int)
bounds [] = (minBound, minBound)
bounds (((x0, y0), (x1, y1)):xs) = (x, y) where
    (x', y') = bounds xs
    x = maximum [x0, x1, x']
    y = maximum [y0, y1, y']

replace (x, y) chart = chart' where
    chart' = above ++ [new_row] ++ below
    new_row = left ++ [value + 1] ++ right
    row = chart !! y
    above = take y chart
    below = drop (y + 1) chart
    value = row !! x
    left = take x row
    right = drop (x + 1) row

-- Part 2
part2 input = atleastTwo where
    atleastTwo = sum . (map (\_ -> 1)) . (filter (>=2)) $ concat chart
    chart = plot (ventPts True input) [[0 | n' <- [0..n]] | m' <- [0..m]]
    (n, m) = bounds input

plot [] chart = chart
plot (pnt:pnts) chart = replace pnt (plot pnts chart)

ventPts diag (((x0, y0), (x1, y1)):xs)
    | x0 == x1 = [(x0, y) | y <- [min y0 y1 .. max y0 y1]] ++ ventPts diag xs
    | y0 == y1 = [(x, y0) | x <- [min x0 x1 .. max x0 x1]] ++ ventPts diag xs
    | not diag = ventPts diag xs
    | diag = let (dx, dy, dist) = dir (x0, y0) (x1, y1) in
        [(x0 + dx * d, y0 + dy * d) | d <- [0..dist]] ++ ventPts diag xs
ventPts _ [] = []

dir (x0, y0) (x1, y1) = (dx, dy, dist) where
    dist = abs (x1 - x0)
    dx = signum (x1 - x0)
    dy = signum (y1 - y0)
