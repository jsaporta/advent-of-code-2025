processItem :: String -> Int
processItem ('R' : xs) = read xs
processItem (_ : xs) = negate $ read xs

-- moveDial :: (Int, Int) -> Int -> (Int, Int)
-- moveDial (a, b) x 
-- 	| x < 0 = (fst next_neg, (snd next_neg) + (fromEnum $ a == 1))
-- 	| x > 0 = (fst next_pos, (snd next_pos) + (fromEnum $ a == (-1)))
-- 	| otherwise = (a, b)
-- 	where
-- 		next_neg = moveDial (a - 1, b) (x + 1)
-- 		next_pos = moveDial (a + 1, b) (x - 1)

moveDial :: (Int, Int) -> Int -> (Int, Int)
moveDial (pos, cnt) step
  | step == 0 = (pos, cnt)
  | step > 0  =
      let newPos  = (pos + 1) `mod` 100
          newCnt  = cnt + if newPos == 0 then 1 else 0
      in moveDial (newPos, newCnt) (step - 1)
  | step < 0  =
      let newPos  = (pos - 1) `mod` 100
          newCnt  = cnt + if newPos == 0 then 1 else 0
      in moveDial (newPos, newCnt) (step + 1)

main :: IO ()
main = do
	contents <- readFile "inputs/input_1"
	let locations   = map processItem (lines contents)
	    my_fold = foldl moveDial (50, 0) locations
	print (snd my_fold)
