{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module BoardGame where

import Data.Maybe (fromMaybe, fromJust)
import System.Random (randomRIO)
import System.IO (Handle, stdout, stdin, hGetLine, hPutStrLn)

type BoardGameResult = Int

class (Show s, Eq s, Show p, Eq p, Enum p, Bounded p, Show a, Read a, Eq a) => BoardGame s p a | s -> p, s -> a where
   -- Minimal definitions.
   beginning :: s
   actions :: s -> p -> [a]
   nextState :: s -> [(p, a)] -> s
   score :: s -> Maybe [(p, BoardGameResult)]
   -- Other functions.
   activePlayers :: s -> [p]
   activePlayers s = [p | p <- players s, not $ null $ actions s p]
   players :: s -> [p]
   players _ = [minBound .. maxBound]
--end class BoardGame

nextPlayer :: (Eq p, Enum p, Bounded p) => p -> p
nextPlayer p = if p == maxBound then minBound else (succ p)

activePlayer :: (BoardGame s p a) => s -> p
activePlayer s
   | (length aps) == 1 = head aps
   | null aps          = error ("BoardGame.activePlayer: there is no active player in "++ (show s) ++"!")
   | otherwise         = error ("BoardGame.activePlayer: there is more than one active player in "++ (show s) ++"!")
   where aps = activePlayers s

isFinished :: (BoardGame s p a) => s -> Bool
isFinished s = null (activePlayers s)

isActive :: (BoardGame s p a) => s -> p -> Bool
isActive s p = elem p (activePlayers s)

-- Matches & Agents --------------------------------------------------------------------------------

class (BoardGame s p a) => BoardGameAgent g s p a where
   play :: g -> s -> p -> IO a

activeAgents :: (BoardGameAgent g s p a) => [(p, g)] -> s -> [(p, g)]
activeAgents ags s = [(p, g) | (p, g) <- ags, elem p _activePlayers]
   where _activePlayers = activePlayers s

activePlayersPlays :: (BoardGameAgent g s p a) => [(p, g)] -> s -> IO [(p, a)]
activePlayersPlays ags s = do
    let _activeAgents = activeAgents ags s
    actions <- mapM (\(p, g) -> play g s p) _activeAgents
    return $ zipWith (\(p, g) a -> (p, a)) _activeAgents actions 

data BoardGameMatch s p a = MatchStart s (BoardGameMatch s p a)
   | MatchActions [(p, a)] s (BoardGameMatch s p a)
   | MatchFinished [(p, BoardGameResult)] deriving (Eq)

instance (BoardGame s p a) => (Show (BoardGameMatch s p a)) where
   show (MatchStart s m) = "Match starts:\n"++ (show s) ++"\n"++ (show m)
   show (MatchActions as s m) = "Players play: "++ (show as) ++".\n"++ (show s) ++"\n"++ (show m)
   show (MatchFinished rs) = "Match ends with: "++ (show rs) ++"."

match :: (BoardGameAgent g s p a) => [(p, g)] -> s -> IO (BoardGameMatch s p a) 
match ags s = if (isFinished s) then 
      return $ MatchFinished $ fromJust $ score s
   else do
      _actions <- activePlayersPlays ags s
      let _nextState = nextState s _actions
      _continuation <- match ags _nextState
      return (MatchActions _actions _nextState _continuation)

{- ### RandomAgent #################################################################################

-}
data RandomAgent = RandomAgent deriving (Show)

instance (BoardGame s p a) => BoardGameAgent RandomAgent s p a where
   play RandomAgent s p = do
      let _actions = (actions s p)
      if null _actions then
         error ("BoardGame.play: player ("++ (show p) ++") has no actions at game state ("++ (show s) ++")!")
      else do
         i <- randomRIO (0, (length _actions) - 1)
         return (_actions !! i)

randomMatch :: (BoardGame s p a) => s -> IO (BoardGameMatch s p a)
randomMatch s = match [(p, RandomAgent) | p <- players s] s

{- ### FileAgent ################################################################################

-}
data FileAgent = FileAgent Handle Handle deriving (Eq, Show)

consoleAgent = FileAgent stdin stdout

instance (BoardGame s p a) => BoardGameAgent FileAgent s p a where
   play g@(FileAgent hin hout) s p = do
      let _actions = (actions s p)
      if null _actions then do
         error ("BoardGame.play: player ("++ (show p) ++") has no actions at game state ("++ (show s) ++")!")
      else do
         hPutStrLn hout ("Select one move:" ++ concat [" "++ show a | a <- _actions])
         line <- hGetLine hin
         let a = read line
         if elem a _actions then return a else do 
            hPutStrLn hout "Invalid move!"
            play g s p

-- Silly testbed definition ------------------------------------------------------------------------

data Choose2Win_Player = Player1 | Player2 deriving (Eq, Show, Enum, Bounded)
data Choose2Win_Action = Win | Lose | Pass deriving (Eq, Show, Read, Enum)
data Choose2Win = Playing Choose2Win_Player | Finished Choose2Win_Player deriving (Eq, Show)

instance BoardGame Choose2Win Choose2Win_Player Choose2Win_Action where
   beginning = Playing Player1
   actions (Playing ap) p | ap == p = [Win .. Pass]
   actions _ _ = []
   nextState s@(Finished _) _ = error ("nextState: game "++ (show s) ++" is finished!") 
   nextState (Playing ap) [(p, Win)] | ap == p = Finished p
   nextState (Playing ap) [(p, Lose)] | ap == p = Finished (nextPlayer p)
   nextState (Playing ap) [(p, Pass)] | ap == p = Playing (nextPlayer p)
   nextState s@(Playing ap) as = 
      error ("nextState: invalid actions "++ (show as) ++" in game "++ (show s) ++"!") 
   score (Playing _) = Nothing
   score (Finished w) = Just [(p, if p == w then 1 else (-1)) | p <- [minBound .. maxBound]]
