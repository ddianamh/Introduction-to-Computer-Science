module EditDistance (ed) where


-- ed function on lists [a] where a is of some type that accepts equality so Eq a 
ed :: Eq a => [a] -> [a] -> Int
ed [] [] = 0 -- base case, 2 empty lists
ed xs [] = length xs  -- one list and one empty list case
ed [] xs = length xs
ed (x:xs) (y:ys) --comparing recursively, element by element
    | x == y  =  ed xs ys  --if the head elts are equal, do ed for the tails
    | otherwise = 1 + minimum [( ed (y: (x:xs)) (y:ys) ),  --adding first elt of the 2nd list to the 1st list
                                 ed xs (y:ys), --removing the head of the 1st list
                                 ed (y:xs) (y:ys)] --replacing 1st elt of the 1st list with the 1st from the 2nd list