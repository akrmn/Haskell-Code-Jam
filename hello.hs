import Data.Array (Array, listArray, (!), (//))
import Text.Printf (printf)

(.:) :: (b -> c) -> (a -> d -> b) -> a -> d -> c
(.:) = (.) . (.)

(.:.) :: (b -> c) -> (a -> d -> e -> b) -> a -> d -> e -> c
(.:.) = (.:) . (.)

doIt :: String -> String -> String
doIt = printf "%04d" .: hello

hello :: String -> String -> Int
hello message text = solution ! len `mod` 1000
  where
    len = length message
    indexedMessage = zip [1..] message
    base = (listArray (1, len) (repeat 0))
    solution = foldl (aux indexedMessage) base text

    aux :: [(Int, Char)] -> Array Int Int -> Char -> Array Int Int
    aux indexedMessage outer textLetter = foldl (aux2 textLetter) outer indexedMessage

    aux2 :: Char -> Array Int Int -> (Int, Char) -> Array Int Int
    aux2 textLetter inner (index, messageLetter)
      | messageLetter == textLetter =
          if index == 1
            then inner // [(1, inner!1 + 1)]
            else inner // [(index, (inner!index + inner!(index-1)))]
      | otherwise = inner
