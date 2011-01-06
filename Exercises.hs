{- Exerise 1.1
double (double 2)
double 2 + double 2
(2 + 2) + double 2
(2 + 2) + (2 + 2)
4 + (2 + 2)
4 + 4
8 -}

{- Exercise 1.2
sum [x]
x + sum[]
x + 0
x -}

-- Exercise 1.3
productEx [] = 1
productEx (x:xs) = x * (productEx xs)

-- Exercise 1.4
qsortRev [] = []
qsortRev (x : xs) = qsortRev larger ++ [x] ++ qsortRev smaller
                where
                  smaller = [a | a <- xs, a <= x ] 
                  larger = [ b | b <- xs , b > x ]

{- Exercise 1.5
qsort [2,2,3,1,1]
qsort [1,1] ++ [2] ++ qsort [3]
(qsort [] ++ [1] ++ qsort []) ++ [2] ++ (qsort [] ++ [3] ++ qsort [])
([] ++ [1] ++ []) ++ [2] ++ ([] ++ [3] ++ [])
[1] ++ [2] ++ [3]
[1,2,3] -}

{- Exercise 2.1
(2 ^ 3) * 4
(2 * 3) + (4 * 5)
2 + (3 * (4 ^ 5)) -}

-- Exercise 2.3
n = a `div` length xs 
  where
    a = 10
    xs = [1,2,3,4,5]
    
-- Exercise 2.4
lastEx xs = head (reverse xs)

-- Exercise 2.5
init1 xs = reverse (tail (reverse xs))
init2 xs = take ((length xs) - 1) xs

{- Exercise 3.1
['a','b','c'] :: [Char]
('a','b','c') :: (Char, Char, Char)
[(False ,'O'), (True,'1')] :: [(Bool,Char)]
([False,True],['0','1']) :: ([Bool],[Char])
[tail,init,reverse] :: [[a] -> [a]] -}

{- Exercise 3.2
second :: [a] -> a
swap :: (a,b) -> (b,a)
pair :: a -> b -> (a,b)
double :: Num a => a -> a
palindrome :: Eq a => [a] -> Bool
twice :: (a -> a) -> a -> a -}

-- Exercise 4.1
halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
        where 
          n = ((length xs) `div` 2)
          
-- Exercise 4.2
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs = []
             | otherwise =  tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

{- Exercise 4.3 
False V False = False
False V True = True
True V False = True
True V True = True

False V False = False
_ V _ = True

False V b = b
True V _ = True

b V c | b == c = b
      | otherwise = True -}

-- Exercise 4.4
{- a ^ b = if a then
          if b then True else False
        else False -}

-- Exercise 4.5        
{- a ^ b = if a then b else False -}

-- Exercise 4.6
mult x y z = \x -> (\y -> (\z -> x * y * z))

find :: Eq a => a -> [(a,b)] -> [b]
find k xs = [v | (k',v) <- xs, k==k']

pos :: Eq a => a -> [a] -> [Int]
pos x xs = [i | (x',i) <- zip xs [0..n], x'==x]
  where n = length xs - 1
  
-- Exercise 5.1
{- sum [x^2| x <- [1..100]] -}

-- Exercise 5.2
