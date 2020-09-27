{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedLabels          #-}

module DeckBuilding.Dominion.Cards.Utils
    ( valueCard
    , basicCardAction
    , gainCard
    , simpleVictory
    , hasActionCards
    ) where

import Control.Lens ( (^.), (%=), (+=), Ixed(ix) )
import Data.Generics.Product ( HasField(field) )
import Data.Generics.Labels ()
import Data.List ( find, delete )
import qualified Data.Map as Map
import DeckBuilding.Types ( PlayerNumber(unPlayerNumber) )
import DeckBuilding.Dominion.Types
    ( Card, CardType(Action), DominionState )
import DeckBuilding.Dominion.Utils
    ( deal, decreaseCards, findPlayer )

-- | A simple points-only Victory card
-- | Victory Points
-- | Card
-- | Player Number
simpleVictory :: Int -> Card -> PlayerNumber -> DominionState Int
simpleVictory v _ p = do
  (field @"players" . ix (unPlayerNumber p) . #victory) += v
  thePlayer <- findPlayer p
  return $ thePlayer ^. field @"victory"

-- | For value cards, pass the money value.
-- | Money Value
-- | Card
-- | Player Number
valueCard :: Int -> Card -> PlayerNumber -> DominionState PlayerNumber
valueCard m c p = do
  (field @"players" . ix (unPlayerNumber p) . #hand) %= delete c
  (field @"players" . ix (unPlayerNumber p) . #played) %= (c:)
  (field @"players" . ix (unPlayerNumber p) . #money) += m
  return p

-- | For basic card values: draw cards, +actions, +buys, +money
basicCardAction :: Int -> Int -> Int -> Int -> Card -> PlayerNumber -> DominionState PlayerNumber
basicCardAction draw a b m c p = do
  (field @"players" . ix (unPlayerNumber p) . #actions) += a
  (field @"players" . ix (unPlayerNumber p) . #buys) += b
  _ <- deal draw p
  valueCard m c p


-- | Given a list of cards in descending priorty order to gain and a max price,
--  gain the first card in the list that's available that is under the max
--  price.
--  TODO: same structure as buying cards (Card,Card->Player->State Game Bool)
gainCard :: [Card] -> Int -> PlayerNumber -> DominionState (Maybe Card)
gainCard cards highestPrice p = obtain highestCostCard
  where obtain :: Maybe Card -> DominionState (Maybe Card)
        obtain Nothing  = return Nothing
        obtain (Just c) = do
          field @"decks" %= Map.mapWithKey (decreaseCards c)
          (field @"players" . ix (unPlayerNumber p) . #deck) %= (c:)
          return $ Just c
        highestCostCard = find (\c -> (c ^. #cost) < highestPrice) cards

hasActionCards :: Int -> [Card] -> Bool
hasActionCards num cs = num <= length (filter (\c -> (c ^. #cardType) == Action) cs)
