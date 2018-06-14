{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module BoardGame where

import Data.Maybe
import System.Random

class (Show s, Eq s, Show p, Eq p, Enum p, Bounded p, Show a, Read a, Eq a) => BoardGame s p a | s -> p, s -> a where
   -- Minimal definitions.
   beginning :: s
   actions :: s -> p -> [a]
   nextState :: s -> [(p, a)] -> s
   score :: s -> Maybe [(p, Int)]
   -- Other functions.
   activePlayers :: s -> [p]
   activePlayers s = [p | p <- players s, not $ null $ actions s p]
   players :: s -> [p]
   players _ = [minBound .. maxBound]
   isFinished :: s -> Bool
   isFinished s = null (activePlayers s) 
   isActive :: s -> p -> Bool
   isActive s p = elem p (activePlayers s)
--end class BoardGame

nextPlayer :: (Eq p, Enum p, Bounded p) => p -> p
nextPlayer p = if p == maxBound then minBound else (succ p)

activePlayer :: (BoardGame s p a) => s -> p
activePlayer s
   | (length aps) == 1 = head aps
   | null aps          = error ("BoardGame.activePlayer: there is no active player in "++ (show s) ++"!")
   | otherwise         = error ("BoardGame.activePlayer: there is more than one active player in "++ (show s) ++"!")
   where aps = activePlayers s

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


   