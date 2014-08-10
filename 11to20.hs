{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- http://www.haskell.org/haskellwiki/99_questions/11_to_20

module ElevenToTwenty where

import OneToTen

-- >>> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
data Encoded a where
  Single :: Eq a => a -> Encoded a
  Multiple :: Eq a => Int -> a -> Encoded a

deriving instance Eq a => Eq (Encoded a)
deriving instance Show a => Show (Encoded a)

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified = map (\(n, a) -> if n == 1 then Single a else Multiple n a) . encode

-- >>> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeModified :: [Encoded a] -> [a]
decodeModified [] = []
decodeModified (Single x:xs) = x : decodeModified xs
decodeModified (Multiple n x:xs) = replicate n x ++ decodeModified xs

-- >>> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect = encodeModified
-- ??? どう違うのだろう

-- >>> dupli [1, 2, 3]
-- [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli = (>>= (\x -> [x, x]))

-- >>> repli "abc" 3
-- "aaabbbccc"
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

-- >>> dropEvery "abcdefghik" 3
-- "abdeghk"
dropEvery :: [a] -> Int -> [a]
--dropEvery [] _ = []
dropEvery xs n = iter xs n n
  where iter :: [a] -> Int -> Int -> [a]
        iter [] _ _ = []
        iter (x:xs) m n = if m == 1
                          then iter xs n n
                          else x: iter xs (m-1) n

-- >>> split "abcdefghik" 3
-- ("abc", "defghik")
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- >>> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
slice :: [a] -> Int -> Int -> [a]
slice xs n m = take (m-n+1) $ drop (n-1) xs

-- >>> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
-- >>> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
rotate :: [a] -> Int -> [a]
rotate xs n = take (length xs) $ if n < 0 
              then rotate (xs ++ xs) (n + length xs)
              else drop n $ cycle xs

-- >>> removeAt 2 "abcd"
-- ('b',"acd")
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n-1), map fst $ filter (((n-1)/=).snd) $ zip xs [0..] )
