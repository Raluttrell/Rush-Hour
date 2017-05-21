
elem2d a (x:xs)
    | null xs = elem a x
    | null x = False
    | elem a x  = True
    | not (elem a x) = elem2d a xs
     

--statesearch :: [String] -> String -> [String] -> [String]
statesearch unexplored hcl vcl path
   | null unexplored              = []
   | not  (elem2d 'X' (head unexplored)) = (head unexplored):path
   | (not (null result))          = result
   | otherwise                    = 
        statesearch (tail unexplored) hcl vcl path
     where result = statesearch 
                       (generateNewStates hcl vcl (head unexplored)) hcl vcl
                       ((head unexplored):path)

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

--solve the rushHour puzzle
solveRush inlist = 
    reverse (statesearch [inlist] (findHorzCars inlist) (findVertCars inlist) [])


replaceSegment oldList pos segment
   | pos == 0  = segment ++ drop (length segment) oldList
   | otherwise = 
        (head oldList):
        (replaceSegment (tail oldList) (pos - 1) segment)

-- moves the car to the left
moveLeft letter oldList = reverse (moveRight letter (reverse oldList))


-- move cars to the right one square at a time
--moveRight :: (Eq a) => a -> [a] -> [String]

moveRight letter oldList = let 
                            p1 = (getSplitPoint letter oldList)
                            p2 = (getSplit2 letter oldList)
                            in ((slice 0 p1 oldList) ++ "-" ++ (slice p1 p2 oldList) ++ (slice (p2 + 1) 6 oldList))  
	 


slice from to xs = take (to - from) (drop from xs)

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


-- getSplit2 does not work
getSplit2 letter (x:xs)
    | x == letter && not (elem x xs) = 1
    | otherwise                      = 1 + getSplit2 letter xs


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

-- generateNewStates
generateNewStates currState hcl vcl = []
--    concat  [generateEastMoves currstate hcl, generateWestMoves currState hcl,
 --            generateNorthMoves currState vcl, generateSouthMoves currState vcl]


generateEastMoves [] hcl acc = acc
generateEastMoves (x:xs) (y:ys) acc = generateEastMoves xs ys ((generateMovesOneRow x y []):acc)

-- generateMovesOneRow row carlist acc
generateMovesOneRow row [] acc = acc
generateMovesOneRow row (x:xs) acc = generateMovesOneRow row xs ((moveRight x row):acc)

transpose ([]:_) = []
transpose inlist = (map head inlist):transpose (map tail inlist)

-- returs the row numebr in which X cars are located
validateX :: Char -> [String] -> Integer
validateX charX inlist 
	| null inlist = 0
	| elem charX (head inlist) = 1
	| otherwise 			   = 1 + (validateX charX (tail inlist))	
