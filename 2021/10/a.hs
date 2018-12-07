import Data.Either
import Data.List

main = do
    c <- getContents
    run part1 c
    run part2 c
run f = putStrLn . show . f . parse

-------------------------------------------------------------------------------

parse = lines
part1 lns = sum . map errorScore . lefts . map (_error []) $ lns
part2 lns = mid
          . sort
          . map score
          . map (map correctionScore)
          . rights
          . map (_error [])
          $ lns where
    score = foldl (\accumulated score -> 5 * accumulated + score) 0
    mid xs = xs !! (length xs `div` 2)

-------------------------------------------------------------------------------

_error :: String -> String -> Either Char String
_error [] (x:xs) = _error [x] xs
_error (top:stack) (x:xs)
    | isClosing x && closing top == x = _error stack xs
    | isClosing x = Left x
    | otherwise = _error (x:top:stack) xs
_error stack [] = Right . map closing $ stack

correctionScore ')' = 1
correctionScore ']' = 2
correctionScore '}' = 3
correctionScore '>' = 4
errorScore ')' = 3
errorScore ']' = 57
errorScore '}' = 1197
errorScore '>' = 25137
isClosing ')' = True
isClosing ']' = True
isClosing '}' = True
isClosing '>' = True
isClosing _ = False
closing '(' = ')'
closing '[' = ']'
closing '{' = '}'
closing '<' = '>'
