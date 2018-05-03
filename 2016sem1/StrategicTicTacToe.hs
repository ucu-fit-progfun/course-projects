module Main2016 where
import Prelude
import Data.Maybe

-- Game logic stub ---------------------------------------------------------------------------------

data Player = PlayerX | PlayerO deriving (Eq, Show, Enum)

data GameState = GameState Int Player deriving Show

data GameAction = GameAction Int deriving (Show, Read)

startState :: GameState
startState = GameState 0 PlayerX

activePlayer :: GameState -> Maybe Player
activePlayer g@(GameState x p) 
  | isFinished g = Nothing
  | otherwise = Just p

actions :: GameState -> Player -> [GameAction]
actions _ _ = map GameAction [-1, 0, 1]

nextState :: GameState -> Player -> GameAction -> GameState
nextState (GameState x ap) p _ | ap /= p = error ("Player "++ (show p) ++" is not active!")
nextState (GameState x _) PlayerX (GameAction y) = GameState (x + y) PlayerO
nextState (GameState x _) PlayerO (GameAction y) = GameState (x - y) PlayerX

isFinished :: GameState -> Bool
isFinished (GameState x _) = (abs x) >= 2

score :: GameState -> Player -> Maybe Int
score (GameState x _) p
  | x >= 2    = Just (if p == PlayerX then 1 else (-1))
  | x <= (-2) = Just (if p == PlayerX then (-1) else 1)
  | otherwise = Nothing
  
-- Console UI --------------------------------------------------------------------------------------

type Agent = GameState -> IO GameAction

consoleAgent :: Player -> GameState -> IO GameAction
consoleAgent p g = do {
   putStr ("\tMueve "++ (show p) ++" "++ (show [n | GameAction n <- actions g p]) ++": ");
   n <- getLine;
   return (GameAction (read n))
 }

runMatch :: (Agent, Agent) -> GameState -> IO (Int, Int)
runMatch (agX, agO) g = do {
   putStrLn (show g);
   case (activePlayer g) of
      Nothing -> return (fromJust (score g PlayerX), fromJust (score g PlayerO));
	  (Just p) -> do {
	     a <- (if p == PlayerX then agX else agO) g;
		 runMatch (agX, agO) (nextState g p a)
	  }
 }
 
runOnConsole :: IO (Int, Int)
runOnConsole = runMatch (consoleAgent PlayerX, consoleAgent PlayerO) startState