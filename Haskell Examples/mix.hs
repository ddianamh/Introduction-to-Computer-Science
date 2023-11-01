mix :: [a] -> [a] -> [a]
mix [] [] = []
mix [] ys = ys
mix xs [] = xs
mix (x:xs) (y:ys) = [x,y] ++ mix xs ys
