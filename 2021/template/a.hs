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

parse s = s
part1 a = a
part2 a = a

-------------------------------------------------------------------------------

(<$$>) f = fmap (fmap f)

