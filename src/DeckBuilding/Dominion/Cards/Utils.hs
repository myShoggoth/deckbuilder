module DeckBuilding.Dominion.Cards.Utils
    ( valueCard
    , basicCardAction
    ) where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Utils

import Control.Monad.State
import Control.Lens
import Data.List

-- | For value cards, pass money and victory point values.
valueCard :: Int -> Int -> Card -> Player -> State Game Player
valueCard m v c p = return $ over hand (delete c) $ over played (c:) $ over money (+m) $ over victory (+v) p

-- | For basic card values: draw cards, +actions, +buys, +money, +victory
basicCardAction :: Int -> Int -> Int -> Int -> Int -> Card -> Player -> State Game Player
basicCardAction draw a b m v c p = do
  let p' = over hand (delete c) $ over played (c:) $ over actions (+a) $ over buys (+b) $ over money (+m) $ over victory (+v) p
  deal draw p'
