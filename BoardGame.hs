{-|
Module      : BoardGame
Description : TODO Short description
Copyright   : (c) Leonardo Val, 2018
License     : MIT
Maintainer  : leonardo.val@creatartis.com

TODO Description.
-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}
module BoardGame where

import Data.Maybe (fromMaybe, fromJust)
import System.Random (randomRIO)
import System.IO (Handle, stdout, stdin, hGetLine, hPutStrLn)

-- * Board game classes and functions

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

-- | The 'BoardGameResult' is the type of the game's results. These are not necessarely the game 
-- scores. Game results are used to distinguish victories, ties and defeats. Defeats must be 
-- negative, victories possitive and ties zero.
type BoardGameResult = Int

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

-- * Player agents and match coordination

-- | 'BoardGameAgent' is the class that all player agents must instance.
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

-- | A 'RandomAgent' is a player that picks its actions completely at random.
data RandomAgent = RandomAgent deriving (Show)

-- ^ When choosing an action, the Haskell's standard pseudo random generator is used. 
instance (BoardGame s p a) => BoardGameAgent RandomAgent s p a where
   play RandomAgent s p = do
      let _actions = (actions s p)
      if null _actions then
         error ("BoardGame.play: player ("++ (show p) ++") has no actions at game state ("++ (show s) ++")!")
      else do
         i <- randomRIO (0, (length _actions) - 1)
         return (_actions !! i)

-- | A 'randomMatch' runs a match of a given game between 'RandomAgent's. 
randomMatch :: (BoardGame s p a) => s -> IO (BoardGameMatch s p a)
randomMatch s = match [(p, RandomAgent) | p <- players s] s

-- | A 'FileAgent' is an agent defined by two file handles.
data FileAgent = FileAgent Handle Handle deriving (Eq, Show)

-- ^ Whenever a 'FileAgent' has to 'play', the current game state is written to the second handle,
-- and a line of text is read from the first handle. The line is expected to have the string 
-- representation of a valid game action. 
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

-- | A 'consoleAgent' is a 'FileAgent' that uses the standard input to read actions and the standard 
-- output to print game states.
consoleAgent = FileAgent stdin stdout