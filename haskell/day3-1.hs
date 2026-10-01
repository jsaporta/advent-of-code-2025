import Data.Char

main :: IO ()
main = do
    contents <- fmap lines $ readFile "inputs/input_3" :: IO [String]
    print $ foldl (+) 0 (map getHighestJoltage contents)

getHighestJoltage :: String -> Int
getHighestJoltage batteryBank = foldl max 11 $ zipWith calcJoltage maxToLeft maxToRight
    where maxToLeft = cumMax FromLeft batteryBank :: [Int]
          maxToRight = cumMax FromRight batteryBank :: [Int]

calcJoltage :: Int -> Int -> Int
calcJoltage a b = (a * 10) + b

data Direction = FromLeft | FromRight

cumMax :: Direction -> String -> [Int]
cumMax FromLeft batteryBank = init $ tail $ scanl max 1 $ map digitToInt batteryBank
cumMax FromRight batteryBank = init $ tail $ scanr max 1 $ map digitToInt batteryBank
