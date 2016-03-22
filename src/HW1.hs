toDigits :: Integer -> [Integer]
toDigits n
    | n >= 0 = map (\c -> read [c]) $ show n
    | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . f . reverse
  where
    -- easier to double from the left
    f = zipWith (\i x -> if odd i then x * 2 else x) ([0..] :: [Integer])

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
