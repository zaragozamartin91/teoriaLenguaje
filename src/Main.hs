module Main where

import qualified Sgz.Command as C
import Control.Monad
import qualified Data.List.Split as S
import Control.Exception
import Sgz.Data as D
import Data.Map as Map

mainLoop :: D.PeopleMap -> IO ()
mainLoop peopleMap = 
    (C.readCommand >>= C.runCommand peopleMap >>= mainLoop) `catch` handleError >>
    mainLoop peopleMap
    

handleError :: IOError -> IO ()
handleError err = putStrLn $ "Error: " ++ (show err)


main:: IO ()
main = 
    let people = (Map.empty :: D.PeopleMap) in
    C.printMenu >>
    C.help >>
    mainLoop people

