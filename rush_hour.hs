
elem2d a (x:xs)
    | null xs = elem a x
    | null x = False
    | elem a x  = True
    | not (elem a x) = elem2d a xs
     

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

-- moves the car to the left
moveLeft letter oldList = reverse (moveRight letter (reverse oldList))

-- move cars to the right one square at a time
--moveLeft :: (Eq a) => [String] -> [String]
moveLeft letter oldList = reverse (moveRight letter (reverse oldList))

-- move cars to the right one square at a time
--moveRight :: (Eq a) => a -> [a] -> [String]

moveRight letter oldList = begining ++ "-" ++ (rmLast end)
	where (begining, end) = splitAt (getSplitPoint letter oldList) oldList

rmLast inlist = reverse ( tail (reverse inlist))

moveHorizontal oldList
	| null oldList = oldList
	| otherwise	   = transpose oldList

-- move the cars 
checkStates inlist  
	| (validateX 'X' inlist) /= 3 = putStrLn("X cars are in a wrong row")
	| 	otherwise = return()

-- checks whether we can move a letter to the right in a row
movable letter [] = False
movable letter row 
	| ((getSplitPoint letter row) + (getNumberOfLetter letter row)) == 6      = False
	| row!!((getSplitPoint letter row) + (getNumberOfLetter letter row)) == '-' = True
	
moveOthers letter [] = []
moveOthers letter (x:xs) 
	| not (elem letter x) 						                          = x:(moveOthers letter xs)
	| ((getSplitPoint letter x) + (getNumberOfLetter letter x)) == 6      = x:(moveOthers letter xs)
	| x!!((getSplitPoint letter x) + (getNumberOfLetter letter x)) == '-' = (moveOthers letter ((moveRight letter x):xs))  
	| otherwise = (x:xs)

-- returns the postions of the split point in a row
--getSplitPoint :: (Eq a) => a -> [a] -> Int
getSplitPoint letter [] = 0 
getSplitPoint letter (x:xs)
	| x == letter = 0
	| otherwise   = 1 + getSplitPoint letter xs

-- returns the numebr of lettger cars in the row
--getNumberOfLetter :: Char -> [Char] -> Int
getNumberOfLetter letter [] = 0
getNumberOfLetter letter (x:xs)
	| x == letter  = 1 + getNumberOfLetter letter xs
	| otherwise    = getNumberOfLetter letter xs

-- gives distinct elements in a list 
nub l                  = nub' l []
  where
    nub' [] _          = []
    nub' (x:xs) ls
        | x `elem` ls  = nub' xs ls
        | otherwise    = x : nub' xs (x:ls)


transpose ([]:_) = []
transpose inlist = (map head inlist):transpose (map tail inlist)

-- returs the row numebr in which X cars are located
validateX :: Char -> [String] -> Integer
validateX charX inlist 
	| null inlist = 0
	| elem charX (head inlist) = 1
	| otherwise 			   = 1 + (validateX charX (tail inlist))	
