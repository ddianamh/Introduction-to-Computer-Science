divisors :: Int -> [Int]
divisors n = [x | x <- [1,2..n], mod n x == 0] 
-- gets x from {1,2,...,n} and adds to the list only those for which n%x=0

sigma :: Int -> Int -> Int
sigma z n = sum [x^z | x <- [1,2..n], mod n x == 0]
-- sums all the elements x^z from the list, where x is a divisors of n
-- i.e. where x is in {1,2,...,n} and n%x=0








--checking the functions
main = do   
   print(divisors 15)    --calling the divisors function
   print(sigma 2 15)    --calling the sigma function