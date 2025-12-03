-- 謝謝 StackOverflow for wordsWhen!
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

splitOn :: Char -> String -> [String]
splitOn c = wordsWhen (== c)

rangeBounds :: String -> (Int, Int)
rangeBounds s = (read $ head numStrings, read $ head $ tail numStrings)
                where numStrings = splitOn '-' s

isDoubleSequence :: Int -> Bool
isDoubleSequence x
    | length xStr /= halfLength * 2      = False -- Could also `mod`, but needed halfLength anyway
    | otherwise                          = (take halfLength xStr) == (drop halfLength xStr)
    where
        xStr = show x
        halfLength = (length xStr) `div` 2 -- `div` is Python's //, / is Python's /

rangeBoundToInvalidSum :: Int -> (Int, Int) -> Int
rangeBoundToInvalidSum sum (lower, upper)
    | lower > upper          = sum
    | isDoubleSequence lower = rangeBoundToInvalidSum (sum + lower) (lower + 1, upper)
    | otherwise              = rangeBoundToInvalidSum sum (lower + 1, upper)

main :: IO ()
main = do
    contents <- readFile "inputs/input_2"
    let ranges = (splitOn ',' contents) :: [String]
        rangeList = (map rangeBounds ranges) :: [(Int, Int)]
        invalidSums = map (rangeBoundToInvalidSum 0) rangeList
    putStrLn $ show $ foldl (+) 0 invalidSums
