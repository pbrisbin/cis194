module HW4 where

data Tree a
    = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

fun1 :: [Integer] -> Integer
fun1 = product . map f
  where
    f x | even x = x - 2
        | otherwise = 1

-- This function is dumb and I won't do it
-- fun2 :: Integer -> Integer
-- fun2 = undefined

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- | Insert, preserving balance
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ l y r)
    | height l < height r = Node (height l + 1) (insert x l) y r
    | otherwise = Node (height r + 1) l y $ insert x r

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ l _ r) = abs (height l - height r) <= 1

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h

xor :: [Bool] -> Bool
xor = foldl check False
  where
    check :: Bool -> Bool -> Bool
    check a True = not a
    check a _ = a

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x -> (f x :)) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (2:)
    $ map ((+1) . (*2))
    $ filter (`notElem` drops) [1..m]
  where
    m = n `div` 2
    drops =
        [ i + j + (2 * i * j)
        | i <- [1..m], j <- [i..m]
        , 1 <= i, i <= j
        ]
