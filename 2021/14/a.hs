import Data.Either
import Data.List
import Data.List.Split
import Data.Char (isLower)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Debug.Trace as D (trace)

main = do
    c <- getContents
    run part1 c
    run part2 c
run f = print . f . parse

-------------------------------------------------------------------------------

parse s = (template, rules) where
    rules = Map.fromList $ (\[[a, b], [c]] -> ([a, b], c))
                         . splitOn " -> "
                         <$> rs
    [[template], rs] = splitOn [""] $ lines s
part1 (template, rules) = mostCommon - leastCommon where
    mostCommon = (fst . maximum) counted
    leastCommon = (fst . minimum) counted
    counted = fmap (\g -> (length g, head g)) . group . sort $ expanded
    expanded = expand 10 template rules

part2 :: ([Char], Map.Map [Char] Char) -> Int
part2 (template, rules) = mostCommon - leastCommon where
    mostCommon = (\x -> div x 2) . fst . maximum $ sortable
    leastCommon = (\x -> div x 2) . fst . minimum $ sortable
    sortable = fmap (\(a, b) -> (b, a))
             . Map.toList
             $ chars'
    chars' = Map.unionWith (+) chars
           $ Map.fromList [(head template, 1), (last template, 1)]
    chars = Map.fromListWith (+)
          . concat
          . map (\([a, b], v) -> [(a, v), (b, v)])
          . Map.toList
          $ simulated

    pairs = mkPairs template Map.empty
    mkPairs (a:b:xs) p =
        Map.unionWith (+) (mkPairs (b:xs) p) (Map.fromList [([a, b], 1)])
    mkPairs [_] p = p
    simulated = simulate 40 pairs rules

-------------------------------------------------------------------------------

(<$$>) f = fmap (fmap f)

expand 0 template _ = template
expand n template rules = expand (n - 1) template' rules where
    template' = expand' template
    expand' (a:b:ts) = [a] ++ [rules Map.! [a, b]] ++ expand' (b:ts)
    expand' [a] = [a]

simulate 0 pairs _ = pairs
simulate n pairs rules = simulate (n - 1) pairs' rules where
    pairs' = Map.filter (>0) (Map.unionWith (+) pairs (diff keys Map.empty))
    diff (k:ks) p = Map.unionWith (+) (diff ks p) (diffSet k)
    diff [] pairs' = pairs'
    diffSet :: [Char] -> Map.Map [Char] Int
    diffSet [a, b] = m where
        c = rules Map.! [a, b]
        v = pairs Map.! [a, b]
        m = Map.fromListWith (+) [([a, b], (-v)), ([a, c], v), ([c, b], v)]
    keys = Map.keys pairs
