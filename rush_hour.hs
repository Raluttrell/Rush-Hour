

rushHour initialState 
	| null initialState = return ()
	| otherwise = do 
		print(length (head initialState))
		rushHour (tail initialState)


replaceSegment oldList pos segment
   | pos == 0  = segment ++ drop (length segment) oldList
   | otherwise = 
        (head oldList):
        (replaceSegment (tail oldList) (pos - 1) segment)

--moveLeft :: (Eq a) => [String] -> [String]
moveLeft oldList
	| (head oldList) == '-' = (drop 1 oldList) ++ "-"
	| otherwise    = oldList

-- move cars to the right one square at a time
--moveRight :: (Eq a) => a -> [a] -> [String]
moveRight letter oldList = begining ++ "_" ++ (rmLast end)
	where (begining, end) = splitAt (getSplitPoint letter oldList) oldList

rmLast inlist = reverse ( tail (reverse inlist))

moveHorizontal oldList
	| null oldList = oldList
	| otherwise	   = transpose oldList

-- move the cars 
checkStates inlist  
	| (validateX 'X' inlist) /= 3 = putStrLn("X cars are in a wrong row")
	| 	otherwise = return()

moveXCars (x:xs)
	| not (elem 'X' x) = x:(moveXCars xs)
	| (last x) == 'X'  = x:xs
	| otherwise 	   = moveOthers (x:xs)

moveOthers ([]:_) = []
moveOthers (x:xs) 
	| not (elem 'X' x) 						  = x:(moveOthers xs)
	| x!!(1 + (getSplitPoint 'X' x) + (getNumberOfLetter 'X' x)) == '-' = (moveRight 'X' x):(moveOthers (x:xs))

-- returns the postions of the split point in a row
getSplitPoint :: (Eq a) => a -> [a] -> Int
getSplitPoint letter [] = 0 
getSplitPoint letter (x:xs)
	| x == letter = 0
	| otherwise   = 1 + getSplitPoint letter xs

-- returns the numebr of lettger cars in the row
getNumberOfLetter :: Char -> [Char] -> Int
getNumberOfLetter letter [] = 0
getNumberOfLetter letter (x:xs)
	| x == letter  = 1 + getNumberOfLetter letter xs
	| otherwise    = getNumberOfLetter letter xs



transpose ([]:_) = []
transpose inlist = (map head inlist):transpose (map tail inlist)

getfacts n = [factorial e | e <- [1 .. n]]
	where factorial x = foldr1 (*) [1 .. x]

-- returs the row numebr in which X cars are located
validateX :: Char -> [String] -> Integer
validateX charX inlist 
	| null inlist = 0
	| elem charX (head inlist) = 1
	| otherwise 			   = 1 + (validateX charX (tail inlist))	
