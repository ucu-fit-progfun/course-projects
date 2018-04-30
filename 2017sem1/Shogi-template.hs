{- Shogi -------------------------------------------------------------------------------------------
 
Plantilla de código para el proyecto del curso 2017 de "Programación Funcional" para las carreras de 
Ingeniería y Licenciatura en Informática de la FIT (UCU).

Por Leonardo Val.
-}
module Shogi where

import Data.Maybe
import System.Random

{-- Lógica de juego --------------------------------------------------------------------------------

Funciones de marca sin ninguna implementación útil. Reemplazar por el código apropiado o por imports
a los módulos necesarios.
-}

data ShogiPlayer = Sente | Gote deriving (Eq, Show, Enum)
data ShogiGame = ShogiGame Bool deriving (Eq, Show) --TODO
data ShogiAction = ShogiAction deriving (Eq, Show) --TODO

beginning :: ShogiGame
beginning = ShogiGame False --TODO

activePlayer :: ShogiGame -> Maybe ShogiPlayer
activePlayer (ShogiGame f) = if f then Nothing else Just Sente --TODO

actions :: ShogiGame -> ShogiPlayer -> [ShogiAction]
actions _ _ = [ShogiAction] --TODO

nextState :: ShogiGame -> ShogiPlayer -> ShogiAction -> ShogiGame
nextState _ _ _ = ShogiGame True --TODO

isFinished :: ShogiGame -> Bool
isFinished (ShogiGame f) = f --TODO

score :: ShogiGame -> ShogiPlayer -> Maybe Int
score (ShogiGame f) _ = if f then Just 0 else Nothing --TODO

showBoard :: ShogiGame -> String
showBoard g = show g --TODO

showAction :: ShogiAction -> String
showAction a = show a --TODO
   
readAction :: String -> ShogiAction
readAction _ = ShogiAction --TODO
   
{-- Match controller -------------------------------------------------------------------------------

Código de prueba. Incluye una función para correr las partidas y dos agentes: consola y aleatorio.

-}
type ShogiAgent = ShogiGame -> IO (Maybe ShogiAction)

{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos 
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
runMatch :: (ShogiAgent, ShogiAgent) -> ShogiGame -> IO (Int, Int)
runMatch ags@(agSente, agGote) g = do
   putStrLn (showBoard g)
   case (activePlayer g) of
      Nothing -> return (fromJust (score g Sente), fromJust (score g Gote))
      Just p -> do
         move <- (if p == Sente then agSente else agGote) g
         runMatch ags (nextState g p (fromJust move))

{- La función ´runOnConsole´ ejecuta toda la partida a partir del estado inicial usando dos agentes
de consola.
-}
runOnConsole :: IO (Int, Int)
runOnConsole = do
   runMatch (consoleAgent Sente, consoleAgent Gote) beginning

{- El agente de consola ´consoleAgent´ muestra el estado de juego y los movimientos disponibles por
consola, y espera una acción por entrada de texto.
-}
consoleAgent :: ShogiPlayer -> ShogiAgent
consoleAgent player state = do
   let moves = (actions state player)
   if null moves then do
      putStrLn "No moves!"
      getLine
      return Nothing
   else do
      putStrLn ("Select one move:" ++ concat [" "++ show m | m <- moves])
      line <- getLine
      let input = readAction line
      if elem input moves then return (Just input) else do 
         putStrLn "Invalid move!"
         consoleAgent player state

randomAgent :: ShogiPlayer -> ShogiAgent
randomAgent player state = do
    let moves = (actions state player)
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       i <- randomRIO (0, (length moves) - 1)
       return (Just (moves !! i))
