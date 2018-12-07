{-# LANGUAGE LambdaCase #-}
import Data.Char
import Data.List

main = do
    c <- getContents
    run part1 c
    run part2 c
run func = putStrLn . show . func . parse

-------------------------------------------------------------------------------
parse = (\xs -> (xs, length . head $ xs)) . lines

-- Part 1

column :: [[Char]] -> Int -> [Char]
column [] _ = []
column (x:xs) idx = (x !! idx:column xs idx)

mostCommon :: [Char] -> Char
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort
leastCommon = snd . minimum . map (\xs -> (length xs, head xs)) . group . sort

bin2int :: String -> Int
bin2int = foldl (\acc x -> acc * 2 + digitToInt x) 0

mostCommonBits report indices = (map (mostCommon . (column report))) $ indices
leastCommonBits report indices = (map (leastCommon . (column report))) $ indices

part1 (report, wordSize) = gamma * epsilon where
    gamma = bin2int $ mostCommonBits report indices
    epsilon = bin2int $ leastCommonBits report indices
    indices = [0, 1..wordSize - 1]

-- Part 2
part2 (report, wordSize) = ogr * csr where
    ogr = bin2int $ oxygenGeneratorRating report indices mostCommonBits 0
    csr = bin2int $ oxygenGeneratorRating report indices leastCommonBits 0
    indices = [0, 1..wordSize - 1]

oxygenGeneratorRating [rating] _ _ _ = rating
oxygenGeneratorRating report indices pattern col =
    oxygenGeneratorRating filtered indices pattern (col + 1) where
        filtered = [r | r <- report, r !! col == bits!! col]
        bits = pattern report indices
