type Day = Int
type Month = Int
type Year = Int
data Date = Date Year Month Day
   deriving (Show)

type Name = String
type Birthday = Date
type Salary = Double
type Manager = [Name]
data Employee = Employee Name Birthday Salary Manager
    deriving (Show)
    
p = Employee "Joe Sample" (Date 1983 06 17) 86123.35 ["Lucy Boss"]
--print p