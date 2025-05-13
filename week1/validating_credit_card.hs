lenOfArgs :: Integer -> Int
lenOfArgs n = length (show n)

toDigitsBase :: Integer -> [Integer]
toDigitsBase n
  | n <= 0 = []
  | otherwise = [n]

-- Convert Integers to List of integers. Ex: 1234 -> [1,2,3,4]
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
  | lenOfArgs n <= 1 = toDigitsBase n
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | lenOfArgs n <= 1 = toDigitsBase n
  | otherwise = (n `div` (10 ^ (lenOfArgs n - 1))) : toDigits (n `mod` (10 ^ (lenOfArgs n - 1)))

main :: IO ()
num :: Integer
num = 1234

list1 :: [Integer]
list2 :: [Integer]
list1 = toDigitsRev num

list2 = toDigits num

main = mapM_ print [list1, list2]
