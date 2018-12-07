{-# LANGUAGE LambdaCase #-}
import Data.List.Split
import Data.Matrix

main = do
    c <- getContents
    run part1 c
    run part2 c
run func = putStrLn . show . func . parse

-------------------------------------------------------------------------------
parse input = (draw, boards) where
    draw = map readInt . splitOn "," $ head lns
    boards = map (fromList 5 5) (chunksOf 25 flat)
    flat = map readInt . words . unwords . tail $ lns
    lns = lines input
    readInt = \x -> read x :: Int

-- Part 1
part1 :: ([Int], [Matrix Int]) -> Int
part1 ((draw:draws), boards) =
    if win then draw * unmarked else part1 (draws, boards') where
        boards' = [mark draw board | board <- boards]
        win = length winning_boards > 0
        winning_boards = filter has_won boards'
        unmarked = sum . (filter (>= 0)) . toList . head $ winning_boards
part1 ([], boards) = error "No winning board"

mark draw board = fromList 5 5 . mark_draw . toList $ board where
    mark_draw = \case
        (x:xs) -> if x == draw then (-1:mark_draw xs) else (x:mark_draw xs)
        [] -> []

has_won :: Matrix Int -> Bool
has_won board = any id [all marked vec | vec <- columns ++ rows] where
    marked x = x < 0
    columns = [getCol i board | i <- [1..5]]
    rows = [getRow i board | i <- [1..5]]

-- Part 2
part2 ((draw:draws), [board]) =
    if has_won board' then (draw * unmarked, draw, board, board') else part2 (draws, [board']) where
        board' = mark draw board
        unmarked = sum . (filter (>= 0)) . toList $ board'
part2 ((draw:draws), boards) = part2 (draws, boards') where 
    boards' = filter (not . has_won) [mark draw board | board <- boards]

