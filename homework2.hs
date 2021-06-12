module Main where

import Prelude hiding (Maybe (..))

data Maybe a = Nothing | Just a deriving (Eq, Ord, Read, Show)

data Heap n = Leaf n | Branch (Maybe n, (Maybe (Heap n), Maybe (Heap n))) deriving (Eq, Ord, Read, Show)

-- Branch (Just 6,(Just (Branch (Just 4,(Just (Branch (Just 5,(Just (Leaf 1),Just (Leaf 2)))),Just (Leaf 3)))),Nothing))

-- Branch (Just 4,(Just (Branch (Just 5,(Just (Leaf 1),Just (Leaf 2)))),Just (Leaf 3)))

-- Branch (Just 5,(Just (Leaf 1),Just (Leaf 2)))

-- It creates empty heap
empty :: Heap n
empty = Branch (Nothing, (Nothing, Nothing))

insert :: Ord n => Heap n -> n -> Heap n
insert (Branch(t1, (t2, t3))) x
    | t1 == Nothing = Branch(Just x, (t2, t3))
    | t2 == Nothing = Branch(t1, (Just (Leaf x), t3))
    | t3 == Nothing = Branch(t1, (t2, Just (Leaf x)))
    | otherwise = Branch(Just x, (Just (Branch(t1, (t2, t3))), Nothing))

fromList :: Ord n => [n] -> Heap n
fromList = foldl insert empty 

lookUp :: Ord n => Heap n -> n -> Int
lookUp (Branch(Nothing,(Nothing,Nothing))) x = 0
lookUp (Branch(Just t1, (Just (Leaf t2), t3))) x
    | t1 == x = 1
    | t2 == x = 1
    | t3 == Just (Leaf x) = 1
    | otherwise = 0
lookUp (Branch(Just t1, (Just t2, t3))) x
    | t1 == x = 1
    | t3 == Just (Leaf x) = 1
    | otherwise = lookUp t2 x

maxElement :: Ord n => Heap n -> Maybe n
maxElement (Branch(Nothing,(Nothing,Nothing))) = Nothing


main = do
    let a = fromList [5,1,2,4,3,6]
    putStrLn $ show a