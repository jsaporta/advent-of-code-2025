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






-- Double, Triple, Quadruple, etc. -> "Nple"
isNpleSequence :: Int -> Int -> Bool
isNpleSequence x n = (foldl (++) "" (replicate n (take fracLength xStr))) == xStr
    where
        xStr = show x :: String
        fracLength = (length xStr) `div` n -- `div` is Python's //, / is Python's /

isMultiSequence :: Int -> Bool
isMultiSequence x = any (isNpleSequence x) [2..(length $ show x)]
--     where halfLength = ((length (show x)) `div` 2) + 1
--           applyOnMe  = [2..(length show x)]










rangeBoundToInvalidSum :: Int -> (Int, Int) -> Int
rangeBoundToInvalidSum sum (lower, upper)
    | lower > upper          = sum
    | isMultiSequence lower = rangeBoundToInvalidSum (sum + lower) (lower + 1, upper)
    | otherwise              = rangeBoundToInvalidSum sum (lower + 1, upper)

main :: IO ()
main = do
    contents <- readFile "inputs/input_2"
    let ranges = (splitOn ',' contents) :: [String]
        rangeList = (map rangeBounds ranges) :: [(Int, Int)]
        invalidSums = map (rangeBoundToInvalidSum 0) rangeList
    putStrLn $ show $ foldl (+) 0 invalidSums
