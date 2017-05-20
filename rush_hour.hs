
elem2d a (x:xs)
    | null xs = elem a x
    | null x = False
    | elem a x  = True
    | not (elem a x) = elem2d a xs
     
{-statesearch :: [String] -> String -> [String] -> [String]
statesearch unexplored goal path
   | null unexplored              = []
   | goal == head unexplored      = goal:path
   | (not (null result))          = result
   | otherwise                    = 
        statesearch (tail unexplored) goal path
     where result = statesearch 
                       (generateNewStates (head unexplored)) 
                       goal 
                       ((head unexplored):path)-}


-- construct a list of horizontal cars and vertical cards for each row

rm :: Eq a => a -> [a] -> [a]
rm item inlist 
    |null inlist = []
    |(head inlist) == item = rm item (tail inlist)
    |otherwise        = (head inlist) : rm item (tail inlist)


myremoveduplicates :: Eq a => [a] -> [a] 
myremoveduplicates inlist
    |null inlist = []
    |elem (head inlist) (tail inlist) = (head inlist) : myremoveduplicates (rm (head inlist) (tail inlist))
    |otherwise          = (head inlist) : myremoveduplicates (tail inlist)


-- removes duplicates from the rows(this matters for cars of length 3 or more)
findCars row = myremoveduplicates (findCars2 row)

-- finds all the cars in a row
findCars2 row
    |null (firstCarInRow row) = []
    |otherwise              = (firstCarInRow row) ++ findCars2 (tail row)

-- finds the first horizontal car in the row
firstCarInRow row
    |null (tail row) = []
    |head row /= '-' &&  (elem (head row) (tail row)) = (head row):[]
    |otherwise = firstCarInRow (tail row)

findHorzCars inlist = reverse (findHorzCars2 inlist [])

-- find horizontal cars with accumulator
findHorzCars2 [] acc = acc
findHorzCars2 (x:xs) acc = findHorzCars2 xs $!(findCars x:acc)

-- find vertical cars in the list
findVertCars inlist = findHorzCars (transpose inlist)



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
