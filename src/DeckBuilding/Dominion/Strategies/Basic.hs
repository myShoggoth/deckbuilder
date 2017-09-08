module DeckBuilding.Dominion.Strategies.Basic
    ( doDiscard
    , doTrash
    , doRetrieveDiscard
    ) where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Utils
import Data.List (delete, intersect)

-- Strategies

-- Strategy helpers

doDiscard :: (Int, Int) -> [Card] -> Player -> GameState -> GameState
doDiscard (min, max) cards p gs = changeTurn player gs
  where pref = take max $ intersect (_hand p) cards
        toDiscard
          | length pref > min = pref
          | otherwise         = take min $ pref ++ (_hand p)
        player  = Player (_playerName p) (_deck p) (_discard p ++ toDiscard) newHand (_played p) (_actions p) (_buys p) (_money p) (_victory p)
        newHand = foldr (\c acc -> delete c acc) (_hand p) toDiscard

doTrash :: (Int, Int) -> [Card] -> Player -> GameState -> GameState
doTrash (min, max) cards p gs = changeTurn player gs
  where pref = take max $ intersect (_hand p) cards
        toDiscard
          | length pref > min = pref
          | otherwise         = take min $ pref ++ (_hand p)
        player  = Player (_playerName p) (_deck p) (_discard p) newHand (_played p) (_actions p) (_buys p) (_money p) (_victory p)
        newHand = foldr (\c acc -> delete c acc) (_hand p) toDiscard

doRetrieveDiscard :: (Int, Int) -> [Card] -> Player -> GameState -> GameState
doRetrieveDiscard (min, max) cards p gs = changeTurn player gs
  where pref = take max $ intersect (_discard p) cards
        toRetrieve
          | length pref > min = pref
          | otherwise         = take min $ pref ++ (_discard p)
        player = Player (_playerName p) (toRetrieve ++ _deck p) newDiscard (_hand p) (_played p) (_actions p) (_buys p) (_money p) (_victory p)
        newDiscard = foldr (\c acc -> delete c acc) (_discard p) toRetrieve
