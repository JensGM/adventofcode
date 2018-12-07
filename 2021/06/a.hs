{-# LANGUAGE LambdaCase #-}
import Data.List.Split
import Data.List
import Data.Map
import qualified Debug.Trace as D (trace)

main = do
    c <- getContents
    run part1 c
    run part2 c
run func = putStrLn . show . func . parse

-------------------------------------------------------------------------------
parse :: String -> Map Int Int
parse c = Main.init (fromList [(i, 0) | i <-[0..8]]) state where
    state = (Data.List.map (\x -> read x :: Int)) . (splitOn ",") $ c
init :: Map Int Int -> [Int] -> Map Int Int
init fish [] = fish
init fish (x:xs) = alter ((1 +) <$>) x fish' where
    fish' = Main.init fish xs

-- Part 1

popKey :: Ord k => k -> Map k a -> (Maybe a, Map k a)
popKey = updateLookupWithKey (\_ _ -> Nothing) 

part1 = part1' 80
part1' 0 fish = sum . elems $ fish
part1' day fish = part1' (day - 1) fish' where
    fish' = resetSpawning . addSpawned $ decr
    (spawns, decr) = (popKey (-1)) . (mapKeys (\k -> k - 1)) $ fish
    addSpawned = alter increaseBySpawns 8
    resetSpawning = alter increaseBySpawns 6 
    increaseBySpawns = \case
        Nothing -> spawns
        Just a -> (+ a) <$> spawns

-- Part 2

part2 = part1' 256

