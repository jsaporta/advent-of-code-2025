processItem :: String -> Int
processItem s
	| head s == 'L' = -(read $ tail s)
	| otherwise     = read $ tail s

main :: IO ()
main = do
	contents <- fmap lines $ readFile "inputs/input_1"
	let locations = map processItem contents in
		let prefixSum = scanl (+) 50 locations in
			putStrLn $ show (length (filter (\x -> x `mod` 100 == 0) prefixSum))
