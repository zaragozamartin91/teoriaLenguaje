module Sgz.Data where

import qualified Data.Map as Map
import Control.Exception
import Data.Typeable

data PersonException = NotEnoughDataException
    deriving (Show, Typeable)

instance Exception PersonException

type Username = String
data Person = Person {username :: Username, email :: String, birth :: String} 
    deriving (Show, Eq)

type PersonTuple = (String,String,String)

type PeopleMap = Map.Map Username Person

parsePerson :: [String] -> Person
parsePerson [username, email, birth] = Person username email birth


tupleToPerson :: PersonTuple -> Person
tupleToPerson (u,e,b) = Person u e b


addPeopleToMap :: PeopleMap -> [Person] -> PeopleMap
addPeopleToMap peopleMap people = 
    foldr (\p acc -> Map.insert (username p) p acc) peopleMap peopleMap