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
run f = print . f . parse

-------------------------------------------------------------------------------

parse s = fromEdges $ tuples <$> splitOn "-" <$> lines s where
    tuples [a, b] = (a, b)

part1 g = length p where Just p = explore "start" [] Set.empty False g
part2 g = length p where Just p = explore "start" [] Set.empty True g

-------------------------------------------------------------------------------

type VertexSet a = Set.Set a
type Graph a = Map.Map a (VertexSet a)

(<$$>) f = fmap (fmap f)

fromEdges :: Ord a => [(a, a)] -> Graph a
fromEdges = foldl build Map.empty where
    build graph (a, b) = insert a b . insert b a $ graph
    insert a b g = Map.alter (adjacent b) a g
    adjacent b (Just adj) = Just (Set.insert b adj)
    adjacent b Nothing = Just (Set.singleton b)

explore :: String
        -> [String]
        -> VertexSet String
        -> Bool
        -> Graph String
        -> Maybe [[String]]
explore v path seen spareTime graph
    | v == "end" = Just [[v]]
    | v == "start" && isSeen = Nothing
    | not spareTime && isSmallCave v && isSeen = Nothing
    | otherwise = ([v] ++) <$$> paths
    where
        paths = Just . concat $ concat <$> [explore' v' | v' <- neighbours]
        explore' v' = explore v' path seen' spareTime' graph
        neighbours = Set.toList (graph Map.! v)
        seen' = Set.insert v seen
        isSeen = v `Set.member` seen
        spareTime' = if isSmallCave v && isSeen then False else spareTime

isSmallCave (c:_) = isLower c
