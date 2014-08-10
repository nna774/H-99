-- http://www.haskell.org/haskellwiki/99_questions/1_to_10

module OneToTen where

-- >>> myLast [1,2,3,4]
-- 4
-- >>> myLast ['x','y','z']
-- 'z'
myLast :: [a] -> a
myLast [] = error "myLast: empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- >>> myButLast [1,2,3,4]
-- 3
-- >>> myButLast ['a'..'z']
-- 'y'
myButLast :: [a] -> a
myButLast [] = error "myButLast: empty list"
myButLast [_] = error "myButLast: too little list"
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

-- >>> elementAt [1,2,3] 2
-- 2
-- >>> elementAt "haskell" 5
-- 'e'
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

-- >>> myLength [123, 456, 789]
-- 3
-- >>> myLength "Hello, world!"
-- 13
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- >> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- >>> myReverse [1,2,3,4]
-- [4,3,2,1]
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- >>> isPalindrome [1,2,3]
-- False
-- >>> isPalindrome "madamimadam"
-- True
-- >>> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

data NestedList a = Elem a | List [NestedList a]
-- >>> flatten (Elem 5)
-- [5]
-- >>> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- >>> flatten (List [])
-- []
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List xs) = concat $ map flatten xs

-- >>> compress "aaaabccaadeeee"
-- "abcade"
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = x: iter x xs
  where iter :: Eq a => a -> [a] -> [a]
        iter _ [] = []
        iter x (y:xs) = if x == y
                        then iter x xs
                        else y : iter y xs

-- >>> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = iter x 1 xs
  where iter :: Eq a => a -> Int -> [a] -> [[a]]
        iter x n [] = [replicate n x]
        iter x n (y:xs) = if x == y
                          then iter x (n+1) xs
                          else replicate n x : iter y 1 xs

-- >>> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode [x] = [(1, x)]
encode (x:xs) = iter x 1 xs
  where iter :: Eq a => a -> Int -> [a] -> [(Int, a)]
        iter x n [] = [(n, x)]
        iter x n (y:xs) = if x == y
                          then iter x (n+1) xs
                          else (n, x) : iter y 1 xs
