-- solutions for https://wiki.haskell.org/99_questions/1_to_10
-- less ugly solutions can be found there
module Problems1to10 where

-- first
myLast :: [a] -> a
myLast []     = error "empty list"
myLast [last] = last
myLast (_:ls) = myLast ls

-- second
myButLast :: [a] -> a
myButLast []       = error "too few elements"
myButLast [_]      = error "too few elements"
myButLast (x:_:[]) = x
myButLast (_:xs)   = myButLast xs

-- third
elementAt :: [a] -> Int -> a
elementAt [] _     = error "index out of bounds"
elementAt (x:_) 1  = x
elementAt (_:xs) n | n < 1     = error "index should be positive"
                   | otherwise = elementAt xs (n - 1)

-- fourth
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- fifth
myReverse :: [a] -> [a]
myReverse list = myReverse' list []
    where myReverse' [] result     = result
          myReverse' (x:xs) result = myReverse' xs (x:result)

-- sixth
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == (reverse list)

-- seventh
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten list = flatten' list []
    where flatten' (Elem x) result      = (x:result)
          flatten' (List []) result     = result
          flatten' (List (l:ls)) result = flatten' l $ flatten' (List ls) result

-- eighth
compress :: Eq a => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (a:b:as) = if a == b then compress (a:as) else (a:(compress(b:as)))

-- ninth
pack :: Eq a => [a] -> [[a]]
pack list = pack' list
    where pack' [] = [[]]
          pack' [a] = [[a]]
          pack' (x:xs) = if x == (head l) then ((x:l):ls) else ([x]:l:ls)
              where (l:ls) = pack' xs

-- tenth
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode list = map (\l -> (length l, head l)) $ pack list