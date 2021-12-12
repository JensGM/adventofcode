import qualified Debug.Trace as D (trace)
import Data.Either
import Data.List
import qualified Data.Set as Set

main = do
    c <- getContents
    run part1 c
    run part2 c
run f = putStrLn . show . f . parse

-------------------------------------------------------------------------------

display :: Field -> String
display = map (\x -> if x == '0' then '.' else x) . intercalate "\n" .  map (concat . map show)

parse = map (map (\x -> read [x] :: Int)) . lines
part1 octs = n where
    Counted _ n = loop 100 (reset . flash . charge) (Counted octs 0)
part2 = part2' 1
part2' :: Int -> Field -> Int
part2' step octs
    | nflashed == nocts = step
    | otherwise = part2' (step + 1) octs'
    where
        (Counted octs' nflashed) = reset . flash . charge $ Counted octs 0
        nocts = length (concat octs)
-------------------------------------------------------------------------------

type Field = [[Int]]
data Point = Point Int Int deriving (Eq, Ord, Show)
data Counted a = Counted a Int deriving Show

loop n f a | n > 1 = loop (n - 1) f (f a)
           | otherwise = (f a)

charge :: Counted Field -> Counted Field
charge (Counted octs flashed) = Counted octs' flashed where
    octs' = map (map (+1)) octs
    canFlash _ = True

flash :: Counted Field -> Counted Field
flash (Counted octs flashed) = Counted octs' flashed where
        octs' = induce (points octs) Set.empty octs

reset :: Counted Field -> Counted Field
reset (Counted octs flashed) = Counted octs' flashed' where
    octs' = map (map (\o -> if 9 < o then 0 else o)) octs
    flashed' = flashed + (length . filter (>9) . concat $ octs)

points :: Field -> [Point]
points octs = [Point x y | x <- [0..ncols - 1], y <- [0..nrows - 1]] where
    nrows = length octs
    ncols = length (head octs)

get :: Point -> Field -> Int
get (Point x y) octs = octs !! y !! x

induce :: [Point] -> Set.Set Point -> Field -> Field
induce (p:pnts) flashed octs
    | get p octs > 9 && p `Set.notMember` flashed =
        induce (points octs) flashed' octs'
    | otherwise = induce pnts flashed octs
    where
        octs' = foldl (set (+1)) octs (neighbours p octs)
        flashed' = Set.insert p flashed
induce [] _ octs = octs

set :: (Int -> Int) -> Field -> Point -> Field
set f octs (Point x y) = octs' where
    octs' = above ++ [row'] ++ below
    row' = before ++ [v] ++ after
    v = f (row !! x)
    before = take x row
    after = drop (x + 1) row
    above = take y octs
    below = drop (y + 1) octs
    row = octs !! y

neighbours :: Point -> Field -> [Point]
neighbours (Point x y) octs = n where
    n = filter inBounds [Point (x + dx) (y + dy) | dx <- [-1..1]
                                                 , dy <- [-1..1]
                                                 , dx /= 0 || dy /= 0]
    inBounds (Point x' y') = 0 <= x' && x' < ncols && 0 <= y' && y' < nrows
    nrows = length octs
    ncols = length (head octs)
