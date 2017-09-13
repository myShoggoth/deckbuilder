module DeckBuilding.Dominion.Strategies.Basic
    ( doDiscard
    , doTrash
    , doRetrieveDiscard
    ) where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Utils
import Data.List (delete, intersect)
import Control.Lens

-- Strategies

-- Strategy helpers

doDiscard :: (Int, Int) -> [Card] -> Player -> GameState -> GameState
doDiscard (min, max) cards p = changeTurn (over discard (++ toDiscard) (set hand newHand p))
  where pref = take max $ intersect (p ^. hand) cards
        toDiscard
          | length pref > min = pref
          | otherwise         = take min $ pref ++ (p ^. hand)
        newHand = foldr (\c acc -> delete c acc) (p ^. hand) toDiscard

doTrash :: (Int, Int) -> [Card] -> Player -> GameState -> GameState
doTrash (min, max) cards p = changeTurn (set hand newHand p)
  where pref = take max $ intersect (p ^. hand) cards
        toDiscard
          | length pref > min = pref
          | otherwise         = take min $ pref ++ (p ^. hand)
        newHand = foldr (\c acc -> delete c acc) (p ^. hand) toDiscard

doRetrieveDiscard :: (Int, Int) -> [Card] -> Player -> GameState -> GameState
doRetrieveDiscard (min, max) cards p = changeTurn (over deck (toRetrieve ++) (set discard newDiscard p))
  where pref = take max $ intersect (p ^. discard ) cards
        toRetrieve
          | length pref > min = pref
          | otherwise         = take min $ pref ++ (p ^. discard)
        newDiscard = foldr (\c acc -> delete c acc) (p  ^. discard) toRetrieve
