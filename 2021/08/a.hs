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

parse = map (map words)
      . map (splitOn "|")
      . lines

-- Part 1

part1 ([_, output]:xs) = length (filter isUnique output) + part1 xs
part1 [] = 0

isUnique = f . length where
    f = \case
        2 -> True -- 1
        4 -> True -- 4
        3 -> True -- 7
        7 -> True -- 8
        otherwise-> False

-- Part 2

part2 :: [[[String]]] -> Int
part2 ([signalPatterns, output]:xs) = s + part2 xs where
    s = read [decode dict o | o <- output] :: Int
    dict = findDict signalPatterns
part2 [] = 0

decode :: Map.Map Char Char -> String -> Char
decode dict word = decoded where
    decoded = toDigit (sort [dict Map.! c | c <- word])

toDigit "abcefg" = '0'
toDigit "cf" = '1'
toDigit "acdeg" = '2'
toDigit "acdfg" = '3'
toDigit "bcdf" = '4'
toDigit "abdfg" = '5'
toDigit "abdefg" = '6'
toDigit "acf" = '7'
toDigit "abcdefg" = '8'
toDigit "abcdfg" = '9'

findDict :: [String] -> Map.Map Char Char
findDict signalPatterns = dict where
    dict = Map.fromList [(a, 'a'),
                         (b, 'b'),
                         (c, 'c'),
                         (d, 'd'),
                         (e, 'e'),
                         (f, 'f'),
                         (g, 'g')]
    (\\) = (Set.\\)
    (<|>) = Set.intersection
    Just _1 = Set.fromList <$> find (\x -> length x == 2) signalPatterns
    Just _4 = Set.fromList <$> find (\x -> length x == 4) signalPatterns
    Just _7 = Set.fromList <$> find (\x -> length x == 3) signalPatterns
    Just _8 = Set.fromList <$> find (\x -> length x == 7) signalPatterns
    a = (head . Set.toList) (_7 \\ _1)
    c_and_f = _7 <|> _1
    b_and_d = _4 \\ _1
    e_and_g = _8 \\ _4 \\ _7
    [(_, c), (_, f)] = sort [(occurs x, x) | x <- Set.toList c_and_f]
    [(_, b), (_, d)] = sort [(occurs x, x) | x <- Set.toList b_and_d]
    [(_, e), (_, g)] = sort [(occurs x, x) | x <- Set.toList e_and_g]
    occurs x = length $ filter (elem x) signalPatterns

