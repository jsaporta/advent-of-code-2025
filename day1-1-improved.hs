processItem :: String -> Int
processItem ('L':xs) = negate (read xs)
processItem (_  :xs) = read xs
processItem []       = error "Empty input line"

main :: IO ()
main = do
    contents <- readFile "input"
    let locations  = map processItem (lines contents)
        prefixSum  = scanl (+) 50 locations
        count      = length $ filter ((== 0) . (`mod` 100)) prefixSum
    print count
