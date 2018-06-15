{-|
Module      : Choose2Win
Description : TODO Short description
Copyright   : (c) Leonardo Val, 2018
License     : MIT
Maintainer  : leonardo.val@creatartis.com

TODO Description.
-}
module Choose2Win where

import BoardGame

-- * Silly testbed definition

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