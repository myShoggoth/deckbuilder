{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE GADTs                     #-}

module DeckBuilding.Dominion.Cards.Utils
    ( gainCard
    , simpleVictory
    , hasActionCards
    , valueCardAction
    , basicCardAction
    , trashCards
    , discardCards
    , handToDeck
    , discardToDeck
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
    ( deal, findPlayer, removeFromCards )

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
valueCardAction m c p = do
  (field @"players" . ix (unPlayerNumber p) . #money) += m
  pure $ Just $ PlayValue p c m

basicCardAction :: Int -> Int -> Int -> Int -> Card -> PlayerNumber -> DominionState (Maybe DominionMove)
basicCardAction d a b m c p = do
  (field @"players" . ix (unPlayerNumber p) . #actions) += a
  (field @"players" . ix (unPlayerNumber p) . #buys) += b
  (field @"players" . ix (unPlayerNumber p) . #money) += m
  deal d p
  pure $ Just $ PlayBasic p c d a b m

-- | Given a list of cards in descending priorty order to gain and a max price,
--  gain the first card in the list that's available that is under the max
--  price.
--  TODO: same structure as buying cards (Card,Card->Player->State Game Bool)
gainCard :: [Card] -> Int -> Maybe Card
gainCard cards highestPrice = find (\c -> (c ^. #cost) < highestPrice) cards

hasActionCards :: Int -> [Card] -> Bool
hasActionCards num cs = num <= length (filter (\c -> (c ^. #cardType) == Action) cs)

trashCards :: PlayerNumber -> [Card] -> DominionState ()
trashCards p toTrash = do
  thePlayer <- findPlayer p
  field @"trash" %= (toTrash ++)
  field @"players" . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) toTrash

discardCards :: PlayerNumber -> [Card] -> DominionState ()
discardCards p toDiscard = do
  thePlayer <- findPlayer p
  field @"players" . ix (unPlayerNumber p) . #discard %= (toDiscard ++)
  field @"players" . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) toDiscard

handToDeck :: PlayerNumber -> [Card] -> DominionState ()
handToDeck p cards = do
  thePlayer <- findPlayer p
  field @"players" . ix (unPlayerNumber p) . #deck %= (cards ++)
  field @"players" . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) cards

discardToDeck :: PlayerNumber -> [Card] -> DominionState ()
discardToDeck p cards = do
  thePlayer <- findPlayer p
  field @"players" . ix (unPlayerNumber p) . #discard .= removeFromCards (thePlayer ^. #discard) cards
  field @"players" . ix (unPlayerNumber p) . #deck %= (cards ++)