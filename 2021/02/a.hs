{-# LANGUAGE LambdaCase #-}

main = do
    c <- getContents
    run part1 c
    run part2 c
run func = putStrLn . show . func . parse

-------------------------------------------------------------------------------

data Movement = Forward Int | Down Int | Up Int deriving Show

parse :: String -> [Movement]
parse = (map parse_movement) . (map words) . lines where
    parse_movement = \case
        ["forward", x] -> Forward (read x :: Int)
        ["down", x] -> Down (read x :: Int)
        ["up", x] -> Up (read x :: Int)
        s -> error ("No way to parse '" ++ unwords s ++ "'")

-- Part 1
part1 :: [Movement] -> Int
part1 = part1' 0 0
part1' horiz depth (Forward x:xs) = part1' (horiz + x) depth xs
part1' horiz depth (Down x:xs) = part1' horiz (depth + x) xs
part1' horiz depth (Up x:xs) = part1' horiz (depth - x) xs
part1' horiz depth [] = horiz * depth

-- Part 2
part2 = part2' 0 0 0
part2' horiz depth _ [] = horiz * depth
part2' horiz depth aim (Down x:xs) = part2' horiz depth (aim + x) xs
part2' horiz depth aim (Up x:xs) = part2' horiz depth (aim - x) xs
part2' horiz depth aim (Forward x:xs) =
    part2' (horiz + x) (depth + aim * x) aim xs

