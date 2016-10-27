import Colonnade.Encoding
import Colonnade.Types
import Data.Functor.Contravariant

data Color = Red | Green | Blue deriving (Show)
data Person = Person { personName :: String, personAge :: Int }
data House = House { houseColor :: Color, housePrice :: Int }

encodingPerson :: Encoding Headed String Person
encodingPerson = mconcat
  [ headed "Name" personName
  , headed "Age" (show . personAge)
  ]

encodingHouse :: Encoding Headed String House
encodingHouse = mconcat
  [ headed "Color" (show . houseColor)
  , headed "Price" (('$':) . show . housePrice)
  ]

encodingPerson2 :: Encoding Headless String Person
encodingPerson2 = mconcat
  [ headless personName
  , headless (show . personAge)
  ]

people :: [Person]
people = [Person "David" 63, Person "Ava" 34, Person "Sonia" 12]

houses :: [House]
houses = [House Green 170000, House Blue 115000]

peopleInHouses :: [(Person,House)]
peopleInHouses = (,) <$> people <*> houses

encodingPersonHouse :: Encoding Headed String (Person,House)
encodingPersonHouse = mconcat
  [ contramap fst encodingPerson
  , contramap snd encodingHouse
  ]

owners :: [(Person,Maybe House)]
owners =
  [ (Person "Jordan" 18, Nothing)
  , (Person "Ruth" 25, Just (House Red 125000))
  , (Person "Sonia" 12, Just (House Green 145000))
  ]

encodingOwners :: Encoding Headed String (Person,Maybe House)
encodingOwners = mconcat
  [ contramap fst encodingPerson
  , contramap snd (fromMaybe "(none)" encodingHouse)
  ]

main :: IO ()
main = do
  putStr $ ascii encodingPerson people
  putStrLn ""
  putStr $ ascii encodingHouse houses
  putStrLn ""
  putStr $ ascii encodingOwners owners
  putStrLn ""

