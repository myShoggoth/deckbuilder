module DeckBuilding.Types
    ( module DeckBuilding.Types
    ) where

import Control.Monad.State

-- | The result of a game. Either Left "Player Name" who is the winner, or
--  Right Int which is the number of players that tied for the lead.
type Result = Either String Int

class Game g where
  finished :: State g Bool
  runTurn :: Int -> State g Bool
  result :: State g Result
  numPlayers :: State g Int
