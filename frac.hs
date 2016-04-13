import Data.List (group, intersperse)
import Data.List.Split (chunksOf)
import Data.Digits(unDigits)

doIt :: (Integer, [Char]) -> [Char]
doIt (i, s) = "Case #" ++ (show i) ++ ": " ++ (frac1 . map read . words $ s)

frac1 :: [Int] -> String
frac1 [k,c,s]
  | possible k c s = concat . intersperse " " . map (show . succ) $ (frac k c s)
  | otherwise = "IMPOSSIBLE"

-- frac :: Int -> Int -> Int -> [Int]
-- frac k c s = map (unDigits k) (chunksOf c [0..k-1])

frac :: Int -> Int -> Int -> [Int]
frac k c s = map (index k) (chunksOf c [0..k-1])

index :: Int -> [Int] -> Int
index k ns = foldl (\acc x -> acc * k + x) 0 ns

possible :: Int -> Int -> Int -> Bool
possible k c s = needed k c <= s

needed :: Int -> Int -> Int
needed k c = (k+c-1) `div` c

main :: IO ()
main = do
  tests <- getLine
  contents <- getContents
  mapM_ (putStrLn . doIt) $ zip [1..] (take (read tests) (lines contents))
