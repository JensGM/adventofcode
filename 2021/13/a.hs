import Data.Either
import Data.List
import Data.List.Split
import Data.Char (isLower)
import qualified Data.Map as Map
import qualified Data.Set as Set

main = do
    c <- getContents
    run part1 c
    run part2 c
run f = putStrLn . f . parse

-------------------------------------------------------------------------------

parse s = (paper, folds) where
    paper = [[mark [x, y] | x <- [0..ncols]] | y <- [0..nrows]]
    mark [x, y] = if [x, y] `Set.member` dots then '#' else '.'
    ncols = (head . Set.findMax) dots
    nrows = (head . Set.findMax) (Set.map reverse dots)
    dots = Set.fromList $ (\x -> read x :: Int) <$$> splitOn "," <$> d
    folds = (\[f, v] -> fromString f v) <$> splitOn "=" <$> f
    [d, f] = splitOn [""] (lines s)
part1 (paper, (f:_)) = show . ndots $ fold paper f
part2 (paper, folds) = intercalate "\n" $ foldl fold paper folds

-------------------------------------------------------------------------------

(<$$>) f = fmap (fmap f)

data Fold = X Int | Y Int deriving Show
fromString "fold along x" v = X (read v :: Int)
fromString "fold along y" v = Y (read v :: Int)

ndots = length . filter (=='#') . concat

fold :: ([[Char]] -> Fold -> [[Char]])
fold paper (X col) = merge left right where
    split = splitAt col <$> paper
    left = fst <$> split
    right = reverse . tail . snd <$> split
fold paper (Y row) = merge top bottom where
    split = splitAt row paper
    top = fst split
    bottom = reverse . tail . snd $ split

merge :: [[Char]] -> [[Char]] -> [[Char]]
merge (a:as) (b:bs) = innerMerge a b : merge as bs where
    innerMerge ('#':as') (_:bs') = '#' : innerMerge as' bs'
    innerMerge (_:as') ('#':bs') = '#' : innerMerge as' bs'
    innerMerge (_:as') (_:bs') = '.' : innerMerge as' bs'
    innerMerge [] bs' = bs'
    innerMerge as' [] = as'
merge [] bs = []
merge as [] = []
