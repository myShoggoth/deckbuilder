{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module DeckBuilding.Types
    ( module DeckBuilding.Types
    ) where

import           Control.Monad.RWS
import           Data.DList

-- | The result of a game. Either Left "Player Name" who is the winner, or
--  Right Int which is the number of players that tied for the lead.
type Result = Either String Int

class (Monoid c, Monoid l) => Game c l g where
  finished :: RWS c l g Bool
  runTurn :: Int -> RWS c l g Bool
  result :: RWS c l g Result
  numPlayers :: RWS c l g Int
