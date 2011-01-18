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

-- Exercise 

product' :: Num a => [a] -> a
product'[] = 1
product' (x:xs) = x * product' xs 

insert                     :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x:y:ys
                | otherwise = y:insert x ys

reverse'       :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' (n+1) [] = []
drop' (n+1) (_:xs) = drop' n xs

fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (n+2) = fibonacci n + fibonacci (n+1)

{-
fibonacci 6
fibonacci 4 + fibonacci 5
(fibonacci 2 + fibonacci 3) + (fibonacci 3 + fibonacci 4)
((0 + 1) + (1 + fibonacci 2)) + ((1 + fibonacci 2) + (fibonacci 2 + fibonacci 3))
((0 + 1) + (1 + (0 + 1))) + ((1 + (0 + 1)) + ((0 + 1) + (1 + fibonacci 2)))
((0 + 1) + (1 + (0 + 1))) + ((1 + (0 + 1)) + ((0 + 1) + (1 + (0 + 1))))
-}

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
	where 
		smaller = [a | a <- xs, a < x]
		larger = [a | a <- xs, a >= x]
		
(^^^) :: Int -> Int -> Int
m ^^^ 0 = 1
m ^^^ (n + 1) = m * (m ^^^ n)

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

init' :: [a] -> [a]
init' [] = []
init' [_] = []
init' (x:xs) = x : init' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' (n+1) a = a : replicate' n a

nth :: [a] -> Int -> a
nth (x:_) 0 = x
nth (_:xs) (n+1) = nth xs n

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) | a == x = True
			   | otherwise = elem' a xs
			
-- Exercise 6.4
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs 						= xs
merge xs [] 						= xs
merge (x:xs) (y:ys) | x <= y 		= x : merge xs (y:ys)
                    | otherwise 	= y : merge (x:xs) ys

halve' :: [a] -> ([a],[a])
halve' xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
		   where (ys,zs) = halve' xs
		
sum' :: Num a => [a] -> a
sum' = foldr (+) 0

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' (n+1) [] = []
take' (n+1) (x:xs) = x : take' n xs 

last' :: [a] -> a
last' [x] = x
last' (_:xs) = last' xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (x:xs) = (f x) : map2 f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) | p x 		= x : filter' p xs