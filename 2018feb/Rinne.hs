{- Rinne -------------------------------------------------------------------------------------------
 
Plantilla de c�digo para el proyecto del curso de febrero de 2018 de _Programaci�n Funcional_ para 
las carreras de Ingenier�a y Licenciatura en Inform�tica de la FIT (UCU).

Por Leonardo Val.
-}
module Rinne where

import Data.Maybe
import System.Random

{- Es posible que el paquete System.Random no est� disponible si se instal� el core de la Haskell 
Platform en el sistema. Para instalarlo, ejecutar los siguientes comandos:

> stack update
> stack install random

La herramienta `stack` es el manejador de paquetes usado por el GHC. Deber�a estar disponible junto 
con el `ghci`.

-}

{-- L�gica de juego --------------------------------------------------------------------------------

Funciones de marca sin ninguna implementaci�n �til. Reemplazar por el c�digo apropiado o por imports
a los m�dulos necesarios.
-}

data RinnePlayer = WhitePlayer | BlackPlayer deriving (Eq, Show, Enum)
data RinneGame = RinneGame Bool deriving (Eq, Show) --TODO
data RinneAction = RinneAction deriving (Eq, Show) --TODO

beginning :: RinneGame
beginning = RinneGame False --TODO

activePlayer :: RinneGame -> Maybe RinnePlayer
activePlayer (RinneGame f) = if f then Nothing else Just WhitePlayer --TODO

actions :: RinneGame -> RinnePlayer -> [RinneAction]
actions _ _ = [RinneAction] --TODO

nextState :: RinneGame -> RinnePlayer -> RinneAction -> RinneGame
nextState _ _ _ = RinneGame True --TODO

isFinished :: RinneGame -> Bool
isFinished (RinneGame f) = f --TODO

score :: RinneGame -> RinnePlayer -> Maybe Int
score (RinneGame f) _ = if f then Just 0 else Nothing --TODO

showBoard :: RinneGame -> String
showBoard g = show g --TODO

showAction :: RinneAction -> String
showAction a = show a --TODO
   
readAction :: String -> RinneAction
readAction _ = RinneAction --TODO
   
{-- Match controller -------------------------------------------------------------------------------

C�digo de prueba. Incluye una funci�n para correr las partidas y dos agentes: consola y aleatorio.

-}
type RinneAgent = RinneGame -> IO (Maybe RinneAction)

{- La funci�n �runMatch� corre la partida completa a partir del estado de juego dado, usando los dos 
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
runMatch :: (RinneAgent, RinneAgent) -> RinneGame -> IO (Int, Int)
runMatch ags@(agWhite, agBlack) g = do
   putStrLn (showBoard g)
   case (activePlayer g) of
      Nothing -> return (fromJust (score g WhitePlayer), fromJust (score g BlackPlayer))
      Just p -> do
         move <- (if p == WhitePlayer then agWhite else agBlack) g
         runMatch ags (nextState g p (fromJust move))

{- La funci�n �runOnConsole� ejecuta toda la partida a partir del estado inicial usando dos agentes
de consola.
-}
runOnConsole :: IO (Int, Int)
runOnConsole = do
   runMatch (consoleAgent WhitePlayer, consoleAgent BlackPlayer) beginning

{- El agente de consola �consoleAgent� muestra el estado de juego y los movimientos disponibles por
consola, y espera una acci�n por entrada de texto.
-}
consoleAgent :: RinnePlayer -> RinneAgent
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

randomAgent :: RinnePlayer -> RinneAgent
randomAgent player state = do
    let moves = (actions state player)
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       i <- randomRIO (0, (length moves) - 1)
       return (Just (moves !! i))
