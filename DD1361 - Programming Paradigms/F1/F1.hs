-- Konstantin Sozinov 
module F1 where

import Data.Char
	

	-- 1. Fibonacci-talen
	
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

	-- 2. Rövarspråket

rovarsprak :: [Char] -> [Char]
rovarsprak [] = []
rovarsprak (x:xs) = 
		case vokal x of
		-- Recurse through list and update chars depending on if a char 
		-- is a vocal or consonant 
			False -> x:'o':x:rovarsprak xs
			True -> x:rovarsprak xs

karpsravor :: [Char] -> [Char]
karpsravor [] = []
karpsravor (x:y:z:zs) =
		-- Recurse through list and update chars.
		-- If a char is consonant and it repeats again after one more char
		-- take it and throw next two charachters
		-- If a char is consonant and it does not repeats, take it and check other chars
		-- If a char is vocal just take it and move on
		case vokal x of 
			False -> 
				if x == z then x:karpsravor zs
				else x:karpsravor (y:z:zs)
			True ->  x:karpsravor (y:z:zs) 
karpsravor (x:xs) =	(x:xs) 

vokal :: Char -> Bool
vokal v = v `elem` ['a','e','u','i','y','o']
	
	-- 3. Medellängd

medellangd :: [Char] -> Double
medellangd [] = 0.0
medellangd text = 
	let 
	-- split the text, take only alphabetic characters 
	words = split text
	-- calculate amount of chars 
	chars = foldr (\x acc -> length x + acc) 0 words 
	-- calculate average length
	in  fromIntegral chars / fromIntegral (length words) 

split :: String -> [String]  
split s = 
	-- remove all non alphabetic characters from beginning of the list
	let s' = dropWhile (not . isAlpha) s
	-- take one word and recurse with the rest of the list  
	in case span isAlpha s' of
			([],[]) -> []
			(w,w')  -> w:split w' 

	-- 4. Listskyffling

skyffla :: [a] -> [a]
skyffla [] = []
skyffla  l = skyffla' l []

skyffla' :: [a] -> [a] -> [a]
skyffla' [] [] = []
-- when done collecting every second element, do the same with acc list 
skyffla' [] l  = skyffla' l []
-- take the first element from the list, save the second in acc 
-- and recurse with the rest of the list 
skyffla' (x:y:zs) l = x:(skyffla' zs (l++[y]))
-- in case when it is only one char left
skyffla' [x] l = x:skyffla' l [] 