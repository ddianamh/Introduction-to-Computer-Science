--convert bool to string
showBool :: Bool -> String
showBool True = "True"
showBool False = "False"

--return first if false, second if true
bool :: a -> a -> Bool -> a 
bool f _ False = f 
bool _ g True = g
--example bool 1 2 (5==10) will return 1 because 5==10 false
--bool 1 2 (5<10) will return 2 because 5<10 true

showBool' :: Bool -> String
showBool' b = bool "False" "True" b 
--uses the function bool so returns the first="False" if b is false etc

--Return the zodiac sign for a year
zodiac :: Integral a => a -> String
zodiac 0 = "monkey"
zodiac 1 = "rooster"
zodiac 2 = "dog"
zodiac 3 = "pig"
zodiac 4 = "rat"
zodiac 5 = "ox"
zodiac 6 = "tiger"
zodiac 7 = "rabbit"
zodiac 8 = "dragon"
zodiac 9 = "snake"
zodiac 10 = "horse"
zodiac 11 = "sheep"
zodiac year = zodiac (year `rem` 12)




