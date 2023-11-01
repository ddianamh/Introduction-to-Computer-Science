map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr ((:) . f) [] xs
--most likely applies f on every element of the list

op :: a -> b -> c 
op e x = x
op x e = x
(x `op` y) `op` z = x `op` (y `op` z)

