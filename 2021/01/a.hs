main = do
    c <- getContents
    run part1 c
    run part2 c
run func = putStrLn . show . func . parse

-------------------------------------------------------------------------------
parse = ints . lines where ints = map (\ln -> read ln :: Int)

-- Part 1
part1 (x:y:xs) = if x < y then 1 + part1 (y:xs)
                 else part1 (y:xs)
part1 _ = 0

-- Part 2
part2 xs = part1 . part2' $ xs
part2' (x:y:z:xs) = (x + y + z:part2' (y:z:xs))
part2' _ = []

