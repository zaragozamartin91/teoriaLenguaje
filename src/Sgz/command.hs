module Sgz.Command where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.List.Split as S
import qualified Sgz.Data as D
import System.IO
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Csv as Csv

type CommandId = String
type CommandAction = IO ()

type CommandArg = String
data CommandInput = CommandInput {commandId :: CommandId , commandArgs :: [CommandArg]} deriving (Show)

readCommand :: IO CommandInput
readCommand = 
    getLine >>= \input ->
    let inputList = S.splitOn " " input in
    return (CommandInput (head inputList) (tail inputList))


runCommand :: D.PeopleMap -> CommandInput -> IO D.PeopleMap
runCommand peopleMap (CommandInput "help" _) = help >> return peopleMap
runCommand peopleMap (CommandInput "menu" _) = printMenu >> return peopleMap
runCommand peopleMap (CommandInput "add" []) = ioError . userError $ ("Se debe ingresar: usuario email fechaNacimiento")
runCommand peopleMap (CommandInput "add" args) = putStrLn ("Agregando: " ++ show args) >> add peopleMap args
runCommand peopleMap (CommandInput "get" []) = ioError . userError $ ("No se ingreso nombre de usuario!")
runCommand peopleMap (CommandInput "get" (usrName:_)) = get peopleMap usrName
runCommand peopleMap (CommandInput "load" []) = ioError . userError $ ("Se debe ingresar el path del archivo!")
runCommand peopleMap (CommandInput "load" (filePath:_)) = load peopleMap filePath
runCommand peopleMap (CommandInput c _) = ioError . userError $ ("Comando " ++ c ++ " no reconocido!")


printMenu :: IO ()
printMenu = 
    putStr "\n" >>
    putStrLn "*---------------------------------------------*" >> 
    putStrLn "| Bienvenido al registro de personas.         |" >> 
    putStrLn "| Guarde y recupere datos sobre individuos.   |" >>
    putStrLn "*---------------------------------------------*\n"


help :: IO ()
help = 
    putStrLn "Comandos\n-------------------------------------------------------- " >>
    putStrLn "> help: Imprime este menu de ayuda." >> 
    putStrLn "> add username email fechaNacimiento: Agregar persona." >> 
    putStrLn "> get username: Buscar persona." >>
    putStrLn "> load archivo: Carga archivo *.csv.\n"


add :: D.PeopleMap -> [CommandArg] -> IO D.PeopleMap
add peopleMap args@[u, e, b] = let 
    parsedPerson = D.parsePerson args
    personUsername = D.username parsedPerson in
    return $ Map.insert personUsername parsedPerson peopleMap
add _ args = ioError . userError $ ("Error al ingresar persona: " ++ (show args))


get :: D.PeopleMap -> D.Username -> IO D.PeopleMap
get peopleMap usrName = let 
    maybePerson = Map.lookup usrName peopleMap in
    case maybePerson of
        Nothing -> ioError . userError $ ("Persona: " ++ (show usrName) ++ " no existe!")
        Just person -> putStrLn (show person) >> return peopleMap


load :: D.PeopleMap -> FilePath -> IO D.PeopleMap
load peopleMap path = do
    csvData <- BL.readFile path
    let values = Csv.decode Csv.NoHeader csvData :: Either String (V.Vector D.PersonTuple)
    case values of 
        Left strError -> ioError . userError $ ("Error al leer el archivo: " ++ strError)
        Right tuples -> do
            let people = fmap D.tupleToPerson tuples
            let peopleList = V.toList people
            putStrLn $ ("Agregando: " ++ (show peopleList))
            return $ D.addPeopleToMap peopleMap peopleList

