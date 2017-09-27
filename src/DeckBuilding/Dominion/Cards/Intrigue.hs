module DeckBuilding.Dominion.Cards.Base
    ( courtyardCard

    ) where

import DeckBuilding.Dominion.Types

import Control.Monad.State

courtyardCardAction :: Card -> Player -> Game State Player
courtyardCardAction c p = do
  p' <- basicCardAction 3 (-1) 0 0 0
  (p' ^. strategy . handToDeckStrategy) 1 p'

courtyardCard   = Card "Courtyard"    2 courtyardCardAction Action
