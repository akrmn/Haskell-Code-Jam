lastword :: String -> String
lastword letters = foldl aux "" letters
  where
    aux "" letter = letter:[]
    aux acc@(l:_) letter
      | letter < l = acc ++ (letter:[])
      | otherwise = letter:acc

doIt :: (Integer, String) -> String
doIt (i, s) = "Case #" ++ (show i) ++ ": " ++ (lastword s)

main :: IO ()
main = do
  tests <- getLine
  contents <- getContents
  mapM_ (putStrLn . doIt) $ zip [1..] (take (read tests) (lines contents))
