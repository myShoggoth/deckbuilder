{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedLabels          #-}

module DeckBuilding.Dominion.Cards.Utils
    ( gainCard
    , simpleVictory
    , hasActionCards
    , valueCardAction
    , basicCardAction
    , trashCards
    , discardCards
    ) where

import Control.Lens ( (^.), (%=), (+=), Ixed(ix), (.=) )
import Data.Generics.Product ( HasField(field) )
import Data.Generics.Labels ()
import Data.List ( find )
import qualified Data.Map as Map
import DeckBuilding.Types ( PlayerNumber(unPlayerNumber) )
import DeckBuilding.Dominion.Types
    ( Card, CardType(Action), DominionState, DominionMove(PlayValue, PlayBasic),
      DominionPlayer, DominionAIGame )
import DeckBuilding.Dominion.Utils
    ( decreaseCards, findPlayer, removeFromCards )

-- | A simple points-only Victory card
-- | Victory Points
-- | Card
-- | Player Number
simpleVictory :: Int -> Card -> PlayerNumber -> DominionState Int
simpleVictory v _ p = do
  (field @"players" . ix (unPlayerNumber p) . #victory) += v
  thePlayer <- findPlayer p
  return $ thePlayer ^. field @"victory"

valueCardAction :: Int -> Card -> PlayerNumber -> DominionState (Maybe DominionMove)
valueCardAction m c p = pure $ Just $ PlayValue p c m

basicCardAction :: Int -> Int -> Int -> Int -> Card -> PlayerNumber -> DominionState (Maybe DominionMove)
basicCardAction d a b m c p = pure $ Just $ PlayBasic p c d a b m

-- | Given a list of cards in descending priorty order to gain and a max price,
--  gain the first card in the list that's available that is under the max
--  price.
--  TODO: same structure as buying cards (Card,Card->Player->State Game Bool)
gainCard :: [Card] -> Int -> Maybe Card
gainCard cards highestPrice = find (\c -> (c ^. #cost) < highestPrice) cards

hasActionCards :: Int -> [Card] -> Bool
hasActionCards num cs = num <= length (filter (\c -> (c ^. #cardType) == Action) cs)

trashCards :: PlayerNumber -> DominionPlayer -> [Card] -> DominionState ()
trashCards p thePlayer toTrash = do
  field @"trash" %= (toTrash ++)
  (field @"players" . ix (unPlayerNumber p) . #hand) .= removeFromCards (thePlayer ^. #hand) toTrash

discardCards :: PlayerNumber -> DominionPlayer -> [Card] -> DominionState ()
discardCards p thePlayer toDiscard = do
  (field @"players" . ix (unPlayerNumber p) . #discard) %= (toDiscard ++)
  (field @"players" . ix (unPlayerNumber p) . #hand) .= removeFromCards (thePlayer ^. #hand) toDiscard
