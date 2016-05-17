module Sgz.Command where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.List.Split as S
import qualified Sgz.Data as D

type CommandId = String
type CommandAction = IO ()

type CommandArg = String
data CommandInput = CommandInput {commandId :: CommandId , commandArgs :: [CommandArg]}
-- type CommandInput = (CommandId, [Arg])

readCommand :: IO CommandInput
readCommand = 
    getLine >>= \input ->
    let inputList = S.splitOn " " input in
    return (CommandInput (head inputList) (tail inputList))


runCommand :: D.PeopleMap -> CommandInput -> IO D.PeopleMap
runCommand peopleMap (CommandInput "help" _) = help >> return peopleMap
runCommand peopleMap (CommandInput "menu" _) = printMenu >> return peopleMap
runCommand peopleMap (CommandInput "add" args) = return $ add peopleMap args
runCommand peopleMap (CommandInput c _) = ioError . userError $ ("Comando " ++ c ++ " no reconocido!")


printMenu :: IO ()
printMenu = 
    putStrLn "Bienvenido al registro de personas." >> 
    putStrLn "Guarde y recupere datos sobre individuos."

help :: IO ()
help = 
    putStrLn "Comandos: " >>
    putStrLn "help: Imprime este menu de ayuda" >> 
    putStrLn "menu: Imprime el menu principal" 

add :: D.PeopleMap -> [CommandArg] -> D.PeopleMap
add peopleMap args = let 
    parsedPerson = D.parsePerson args
    personUsername = D.username parsedPerson in
    Map.insert personUsername parsedPerson peopleMap
