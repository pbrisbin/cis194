module HW1 where

toDigits :: Integer -> [Integer]
toDigits n
    | n > 0 = map (\c -> read [c]) $ show n
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

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = concat
    [ hanoi (n - 1) a c b
    , [(a, b)]
    , hanoi (n - 1) c b a
    ]
