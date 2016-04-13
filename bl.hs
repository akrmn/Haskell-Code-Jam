import Data.Digits(digitsRev)
import Data.Bits((.|.), Bits)

doIt :: (Integer, Integer) -> [Char]
doIt (i, n) = "Case #" ++ (show i) ++ ": " ++ (blea n)

blea :: Integer -> [Char]
blea 0 = "INSOMNIA"
blea x = aux 0 [x, 2*x..] 0
  where
    aux p _ 1023 = show p
    aux _ (x:xs) digits = aux x xs (digits .|. (toBits x))

toBits :: Integer -> Integer
toBits x = foldr (.|.) 0 (map (2^) (digitsRev 10 x))

main :: IO ()
main = do
  tests <- getLine
  contents <- getContents
  mapM_ (putStrLn . doIt . (\(i, n) -> (i, read n))) $ zip [1..] (take (read tests) (lines contents))
