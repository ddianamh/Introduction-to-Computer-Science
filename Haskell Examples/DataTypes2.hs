type Day = Int
type Month = Int
type Year = Int

data Date = Date { year :: Year, month :: Month, day :: Day }
    deriving (Show)

type Name = String

type Euro = Double 

data Sex = Female | Male | Diverse
    deriving (Show)

data Employee = Employee { name :: Name
                         , birthday :: Date
                         , sex :: Sex
                         , salary :: Euro
                         , manager :: Maybe Employee }
    deriving (Show)

lucy = Employee { name = "Lucy Boss"
                , birthday = Date { year = 1967, month = 02, day = 20 }
                , sex = Female
                , salary = 113213.23
                , manager = Nothing }

joe = Employee { name = "Joe Sample"
               , birthday = Date { year = 1983, month = 06, day = 17 }
               , sex = Diverse
               , salary = 86123.35
               , manager = Just lucy }

--print joe
--print (manager joe)
--print lucy
--print (manager lucy)