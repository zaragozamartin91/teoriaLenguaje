module Sgz.Command where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.List.Split as S
import qualified Sgz.Data as D

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
runCommand peopleMap (CommandInput c _) = ioError . userError $ ("Comando " ++ c ++ " no reconocido!")


printMenu :: IO ()
printMenu = 
    putStrLn "Bienvenido al registro de personas." >> 
    putStrLn "Guarde y recupere datos sobre individuos."

help :: IO ()
help = 
    putStrLn "Comandos: " >>
    putStrLn "help: Imprime este menu de ayuda" >> 
    putStrLn "menu: Imprime el menu principal" >>
    putStrLn "add: Agregar persona" >> 
    putStrLn "get: Buscar persona"

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


