{- # Shogi #########################################################################################

Implementación de referencia para el proyecto del curso 2017 de "Programación Funcional" para las
carreras de Ingeniería y Licenciatura en Informática de la FIT (UCU).

Por Leonardo Val.
-}
module Shogi where

import Data.Maybe
import System.Random

-- ## Lógica de juego ##############################################################################

data ShogiPlayer = Sente | Gote deriving (Eq, Show, Enum, Read)

data ShogiPiece =
     Pawn   { position :: (Int, Int), owner :: ShogiPlayer, promoted :: Bool }
   | Rook   { position :: (Int, Int), owner :: ShogiPlayer, promoted :: Bool }
   | Bishop { position :: (Int, Int), owner :: ShogiPlayer, promoted :: Bool }
   | Lancer { position :: (Int, Int), owner :: ShogiPlayer, promoted :: Bool }
   | Knight { position :: (Int, Int), owner :: ShogiPlayer, promoted :: Bool }
   | Silver { position :: (Int, Int), owner :: ShogiPlayer, promoted :: Bool }
   | Golden { position :: (Int, Int), owner :: ShogiPlayer }
   | King   { position :: (Int, Int), owner :: ShogiPlayer }
   deriving (Eq, Show, Read)

type ShogiBoard = [ShogiPiece]

data ShogiGame = ShogiGame {
      board :: ShogiBoard,
      moveCount :: Int,
      hands :: ([ShogiPiece], [ShogiPiece])
   }  deriving (Eq, Show)

data ShogiAction = Move { from, to :: (Int, Int), promote :: Bool }
   | Drop { piece :: Int, to :: (Int, Int) }
   deriving (Eq, Show, Read) --TODO

-- ### Funciones requeridas por el planteo #########################################################

beginning :: ShogiGame
beginning = error "beginning is not implemented!"

activePlayer :: ShogiGame -> Maybe ShogiPlayer
activePlayer g = if isFinished g then Nothing else Just (toEnum (mod (moveCount g) 2))

actions :: ShogiGame -> ShogiPlayer -> [ShogiAction]
actions g p
   | (activePlayer g) /= Just p = []
   | otherwise = concat (map (pieceActions g) [x | x <- board g, owner x == p])

nextState :: ShogiGame -> ShogiPlayer -> ShogiAction -> ShogiGame
nextState g _ _ = error "nextState is not implemented!"

isFinished :: ShogiGame -> Bool
isFinished g = (score g Sente) /= Nothing

score :: ShogiGame -> ShogiPlayer -> Maybe Int
score g p
   | moveCount g > 50 = Just 0
   | null [-1 | King _ o <- board g, o == p] = Just (-1)
   | null [1 | King _ o <- board g, o /= p] = Just 1
   | otherwise = Nothing

showBoard :: ShogiGame -> String
showBoard _ = error "showBoard is not implemented!"

showAction :: ShogiAction -> String
showAction _ = error "showAction is not implemented!"

readAction :: String -> ShogiAction
readAction _ = error "readAction is not implemented!"

-- ### Cálculo de movimientos ######################################################################

pieceActions :: ShogiGame -> ShogiPiece -> [ShogiAction]
pieceActions g p@(Pawn from@(row,col) owner False) =
   validMoves g p (deltas from [(forward owner, 0)])
pieceActions g p@(Pawn from@(row,col) owner True) = validMoves g p (goldenMoves owner from)
pieceActions g p@(Lancer from@(row,col) owner True) = validMoves g p (goldenMoves owner from)
pieceActions g p@(Silver from@(row,col) owner False) =
   validMoves g p (deltas from [])
pieceActions g p@(Silver from@(row,col) owner True) =
   validMoves g p (goldenMoves owner from)
pieceActions g p@(Knight from@(row,col) owner False) =
   validMoves g p (deltas from [((forward owner)*2,-1), ((forward owner)*2,1)])
pieceActions g p@(Knight from@(row,col) owner True) = validMoves g p (goldenMoves owner from)
pieceActions g p@(Golden from@(row,col) owner) = validMoves g p (goldenMoves owner from)

goldenMoves :: ShogiPlayer -> (Int, Int) -> [(Int, Int)]
goldenMoves p pos = deltas pos [(f,-1), (f,0), (f,1), (0,-1), (0,1), (-f,0)]
   where f = forward p

deltas :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
deltas (row, col) ds = [(row + dr, col + dr) | (dr, dc) <- ds]

forward :: ShogiPlayer -> Int
forward p = 1 - 2 * (fromEnum p)

validMoves :: ShogiGame -> ShogiPiece -> [(Int, Int)] -> [ShogiAction]
validMoves g p poss = [Move from to prom | to <- coords, prom <- proms]
   where proms = if mustPromote p then [True] else if not (canPromote p) then [False] else [False, True]
         from@(row, col) = position p
         coords = [pos | pos <- poss, inBoard pos, (playerAt pos) /= Just p]

canPromote :: ShogiPiece -> (Int, Int) -> Bool
canPromote (King _ _) = False
canPromote (Golden _ _) = False
canPromote p = not (promoted p) && (if (owner p == Sente) then row >= 6 && row <= 8 else row >= 0 && row <= 2)
   where (row, _) = position p

mustPromote :: ShogiPiece -> (Int, Int) -> Bool
mustPromote (Pawn (row, col) owner False) = if (owner p == Sente) then row == 8 else row == 0
mustPromote (Lancer (row, col) owner False) = if (owner p == Sente) then row == 8 else row == 0
mustPromote (Knight (row, col) owner False) = if (owner p == Sente) then row >= 7 else row <= 1
mustPromote _ = False

inBoard :: (Int, Int) -> Bool
inBoard (row, col) = row >= 0 && row < 9 && col >= 0 && col < 9

pieceAt :: ShogiGame -> (Int, Int) -> Maybe ShogiPiece
pieceAt g pos = listToMaybe [p | p <- board g, (position p) == pos]

playerAt :: ShogiGame -> (Int, Int) -> Maybe ShogiPlayer
playerAt g pos = listToMaybe [owner p | p <- board g, (position p) == pos]

-- ## Controlador de la partida ####################################################################

type ShogiAgent = ShogiGame -> IO (Maybe ShogiAction)

{- `runMatch` corre la partida completa a partir del estado de juego dado, usando los dos agentes
dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
runMatch :: (ShogiAgent, ShogiAgent) -> ShogiGame -> IO (Int, Int)
runMatch ags@(agSente, agGote) g = do
   putStrLn (showBoard g)
   case (activePlayer g) of
      Nothing -> return (fromJust (score g Sente), fromJust (score g Gote))
      Just p -> do
         move <- (if p == Sente then agSente else agGote) g
         runMatch ags (nextState g p (fromJust move))

{- `runOnConsole` ejecuta toda la partida a partir del estado inicial usando dos agentes de consola.
-}
runOnConsole :: IO (Int, Int)
runOnConsole = do
   runMatch (consoleAgent Sente, consoleAgent Gote) beginning

{- El agente de consola `consoleAgent` muestra el estado de juego y los movimientos disponibles por
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

{-
-}
randomAgent :: ShogiPlayer -> ShogiAgent
randomAgent player state = do
    let moves = (actions state player)
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       i <- randomRIO (0, (length moves) - 1)
       return (Just (moves !! i))
