import Data.List (group)

doIt :: (Integer, [Char]) -> [Char]
doIt (i, s) = "Case #" ++ (show i) ++ ": " ++ (show (flp s))

flp :: [Char] -> Int
flp = length . dropWhile (=='+') . reverse . map head . group

main :: IO ()
main = do
  tests <- getLine
  contents <- getContents
  mapM_ (putStrLn . doIt) $ zip [1..] (take (read tests) (lines contents))
