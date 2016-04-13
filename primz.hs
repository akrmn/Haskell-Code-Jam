import Data.Digits (digits, unDigits)

doIt :: [Char] -> [Char]
doIt x = doIt2 (map read . words $ x)

doIt2 :: [Int] -> [Char]
doIt2 [n, j] = concatMap ((++" 3 2 3 2 7 2 3 2 3\n"). show) $ ns
  where
    first2 = unDigits 2 . digits 10 $ 10^(n-1)+1
    last2 = unDigits 2 . replicate (n) $ 1
    ns = take j [unDigits 10 . digits 2 $ x | x <- [first2, first2 + 2.. last2]
              ,                           x  `mod` 3 == 0
              , (unDigits  3 . digits 2 $ x) `mod` 2 == 0
              , (unDigits  4 . digits 2 $ x) `mod` 3 == 0
              , (unDigits  5 . digits 2 $ x) `mod` 2 == 0
              , (unDigits  6 . digits 2 $ x) `mod` 7 == 0
              , (unDigits  7 . digits 2 $ x) `mod` 2 == 0
              , (unDigits  8 . digits 2 $ x) `mod` 3 == 0
              , (unDigits  9 . digits 2 $ x) `mod` 2 == 0
              , (unDigits 10 . digits 2 $ x) `mod` 3 == 0
              ]

main :: IO ()
main = do
  tests <- getLine
  contents <- getContents
  mapM_ (putStrLn . ("Case #1:\n" ++) . doIt) (take (read tests) (lines contents))
