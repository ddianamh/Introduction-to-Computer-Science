quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
where
        smaller = filter (< x) xs
        larger  = filter (>= x) xs