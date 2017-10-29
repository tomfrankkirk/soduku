module Soduku  where 

   -- A Soduku solving algorithm. Tom Kirk, October 2017 
   -- Works on moderately puzzles, not hard ones yet. 

   -- Dependencies ----------------------------------------------------------------------
   import Data.List
   import Data.Maybe
   import Control.Monad
   import Data.List.Index


   -- Type declaration ------------------------------------------------------------------
   type Soduku = [Integer]


   -- "Static constant" for all Soduku - used to index rows, columns, boxes and elements. 
   fullSet :: [Integer]
   fullSet = [1..9]


   -- Utility functions -----------------------------------------------------------------

   -- Empty positions are stored as 0 in the matrix, but these must be dropped before 
   -- performing solution logic. Return an array without 0s. 
   dropZeros :: [Integer] -> [Integer]
   dropZeros = filter (/= 0) 

   -- Get value at position (x,y)
   getElem :: Soduku -> (Integer, Integer) -> Integer 
   getElem sod (row,col) = sod !! fromIntegral (rc2Index (row, col)) 

   -- Get values at positions [(x1,y1), (x2,y2)...]
   get :: Soduku -> [(Integer, Integer)] -> [Integer]     
   get sod coords = map (getElem sod) coords

   -- Convert (x,y) to array index
   rc2Index :: (Integer, Integer) -> Integer 
   rc2Index (row, col) = ((row - 1) * 9) + (col - 1)

   -- Convert array index to (x,y)
   index2RC :: Integer -> (Integer, Integer)
   index2RC ind = ( (ind `div` 9) + 1 , (ind `mod` 9) + 1 )

   -- Print an array of ints (used for printing whole soduku)
   prIntegerRow :: [Integer] -> IO ()
   prIntegerRow [] = return()
   prIntegerRow row = let filtered = map (\x -> if x /= 0 then show x ++ " " else "  ") row ++ ["\n"] in 
                        mapM_ putStr filtered

   -- Print whole soduku                       
   prIntegerSoduku :: Soduku -> IO ()
   prIntegerSoduku sod = do 
      mapM_ (prIntegerRow . (get sod) . (rowCoords))  fullSet 
      putStrLn ""

   -- Extract all array values in range [x..y]
   slice :: Integer -> Integer -> [Integer] -> [Integer]
   slice from to arr = let f = fromIntegral from 
                           t = fromIntegral to in 
                              take (t - f + 1) (drop f arr)

   -- Get (x,y) coord pairs for row n                              
   rowCoords :: Integer -> [(Integer, Integer)]                           
   rowCoords row = map (\col -> (row,col)) fullSet

   -- Get (x,y) coord pairs for col n 
   colCoords :: Integer -> [(Integer, Integer)]  
   colCoords col = map (\row -> (row,col)) fullSet

   -- Get (x,y) coord pairs for 3x3 box containing (r,c)
   boxCoords :: (Integer, Integer) -> [(Integer, Integer)]  
   boxCoords (row,col) = let (rows, cols) = mapTuple mapIntToThird (row,col) in 
                            concatMap (\x -> zip [x,x,x] cols) rows 
   
   -- Used for boxCoords, map an (r,c) pair to its respective box.                           
   mapIntToThird :: Integer -> [Integer]
   mapIntToThird x = let y = x `div` 3 in 
                        if (x `mod` 3) == 0 then [(y-1)*3 + 1 .. y*3]
                        else [(y*3) + 1 .. (y+1) * 3] 
  
   -- Map function for tuples, used for transforming coordinate pairs (which are tuples!)                     
   mapTuple :: (a -> b) -> (a, a) -> (b, b)
   mapTuple f (x, y) = (f x, f y)

   -- Used in place of a "set" function: values cannot be changed so instead generate
   -- a new soduku with the element to be set in its proper position. 
   formNewSoduku :: [Integer] -> Integer -> Integer -> [Integer]
   formNewSoduku sod newElem newIndex = 
      (slice 0 (newIndex - 1) sod) ++ [newElem] ++ (slice (newIndex + 1) (fromIntegral (length sod)) sod) 


   -- Solution logic functions ----------------------------------------------------------

   -- Determinacy is taken here to mean that at least one position within the matrix has a  
   -- unique value that it can be assigned on the next iteration - ie, there is at least one  
   -- way to move the solution forwards. It does not mean that all future steps are determinate! 

   -- The potentials matrix is an array of the same size as the soduku itself that contains 
   -- lists of potential values for each position. Solution progression is based on the premise 
   -- that at each stage, at least one position in the matrix will have a unique value that it
   -- can take, which it will then duly be assigned, and this can be repeated at the next iteration. 

   -- Is soduku complete? Yes, if no elements are zero. 
   isComplete :: Soduku -> Bool
   isComplete sod = 0 `notElem` sod 

   -- Is soduku determinate? Yes, if at least one element in the potentials is of length zero. 
   isDeterminate :: Soduku -> Bool 
   isDeterminate sod = any (\x -> length x == 1) (potentials sod)

   -- Explain me!
   isIndirectDeterminate :: Soduku -> Bool 
   isIndirectDeterminate sod = any (\x -> length x == 1) (indirectPotentials sod)

   -- Get the potentials for a (r,c) position. Look at the respective row, column and box for the 
   -- current position and take the union of all values in this range that are already determined. 
   -- Take the difference (\\) of this with the full set [1..9] and the result is the values that 
   -- could be placed in this position. 
   potentialsForPosition :: Soduku -> (Integer, Integer) -> [Integer]
   potentialsForPosition sod (row,col) = 
      let coords = (rowCoords row) `union` (colCoords col) `union` (boxCoords (row,col)) 
          numbers = dropZeros (get sod coords) in 
             fullSet \\ numbers

   -- Simply map the above function onto all positions of the matrix.           
   potentials :: Soduku -> [[Integer]]
   potentials sod = map (potentialsForPosition sod . index2RC) [0..80]

   -- Helper function for dealing with the potentials matrix. Get potential lists for all (r,c) pairs
   -- in the coords list.  As it is the same size as the puzzle matrix, the same coordinate mappings
   -- for row, col and box can be used. The fold function is used to concatenate all the potential arrays
   -- into one without duplicating elements (via union). The result is thus a single list, even if
   -- multiple coordinates have been passed in. 
   potentialsAccess :: [[Integer]] -> [(Integer,Integer)] -> [Integer]
   potentialsAccess pots coords = let allInRange = map (\c -> pots !! (fromIntegral (rc2Index c))) coords in 
                                       foldl (union) [] allInRange

   -- Get indirect potentials for position                                 
   indirectPotentialsForPosition :: Soduku -> (Integer, Integer) -> [Integer]
   indirectPotentialsForPosition sod (row,col) =  
      let   pots = potentials sod 
            coords = (rowCoords row) `union` (colCoords col) `union` (boxCoords (row,col))
            filtered = delete (row,col) coords 
            otherPots = potentialsAccess pots coords 
            thesePots = potentialsAccess pots [(row,col)] in
            thesePots \\ otherPots       

   -- Map the indirect version onto all positions. 
   indirectPotentials :: Soduku -> [[Integer]]
   indirectPotentials sod = map (indirectPotentialsForPosition sod . index2RC) [0..80]

   -- Next determinate element: the first position for which there is a unique potential value. 
   nextDeterminateElement :: Soduku -> Integer
   nextDeterminateElement sod = let index = ifindIndex (\i x -> (length x == 1) && (sod!!i == 0)) (potentials sod) in 
                                       if isNothing index then -1 else fromIntegral (fromJust index)
    
   -- Perform one iteration of the solution algorithm. If the soduku is incomplete and determinate, 
   -- get the next determinate element and "set" it to form the basis for the next solution step.                                     
   solveStep :: Soduku -> Soduku
   solveStep sod = 
      if isComplete sod then sod
      else 
         let nextElem = nextDeterminateElement sod in 
            if nextElem == -1 then sod
            else let next = (potentials sod) !! (fromIntegral nextElem) 
                     newSod = (formNewSoduku sod (head next) nextElem) in 
                        newSod
                          
   -- Solve: call solveStep recursively. Check for determiacy and completeness at each step.                      
   solve :: [Integer] -> IO [Integer]      
   solve sod = do 
      let new = solveStep sod 
      prIntegerSoduku new
      
      if isComplete new then do
         putStrLn "Solution is complete."

         if isCorrect new then do 
            putStrLn "Solution is correct."
            return (new)
         else do 
            putStrLn "Solution is not correct."
            return (new)

      else
         if isDeterminate new then 
            solve new 
         else do 
            putStrLn "Soduku is not determinate"
            return (new)

   elementIsCorrect :: Soduku -> Integer -> Bool 
   elementIsCorrect sod ind = 
      let   (row,col) = index2RC ind 
            rowSet = get sod (rowCoords row)
            colSet = get sod (colCoords col) 
            boxSet = get sod (boxCoords (row,col)) in
               (rowSet == colSet) && (colSet == boxSet) && (boxSet == rowSet)   
               
   -- isCorrect :: Soduku -> Bool 
   isCorrect sod = 
     all (\x -> x) (map (elementIsCorrect sod) [0..80])

