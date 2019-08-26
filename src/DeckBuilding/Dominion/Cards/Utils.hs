{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module DeckBuilding.Dominion.Cards.Utils
    ( valueCard
    , basicCardAction
    , gainCard
    , simpleVictory
    ) where

import           Control.Lens
import           Data.Generics.Product
import           Data.List
import qualified Data.Map                    as Map
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils

simpleVictory :: Int -> Card -> Int -> DominionState Int
simpleVictory v c p = do
  (field @"players" . ix p . field @"victory") += v
  player <- findPlayer p
  return $ player ^. field @"victory"

-- | For value cards, pass the money value.
valueCard :: Int -> Card -> Int -> DominionState Int
valueCard m c p = do
  (field @"players" . ix p . field @"hand") %= (delete c)
  (field @"players" . ix p . field @"played") %= (c:)
  (field @"players" . ix p . field @"money") += m
  return p

-- | For basic card values: draw cards, +actions, +buys, +money
basicCardAction :: Int -> Int -> Int -> Int -> Card -> Int -> DominionState Int
basicCardAction draw a b m c p = do
  (field @"players" . ix p . field @"actions") += a
  (field @"players" . ix p . field @"buys") += b
  _ <- deal draw p
  valueCard m c p


-- | Given a list of cards in descending priorty order to gain and a max price,
--  gain the first card in the list that's available that is under the max
--  price.
--  TODO: same structure as buying cards (Card,Card->Player->State Game Bool)
gainCard :: [Card] -> Int -> Int -> DominionState (Maybe Card)
gainCard cards highestPrice p = obtain highestCostCard
  where obtain :: Maybe Card -> DominionState (Maybe Card)
        obtain Nothing  = return Nothing
        obtain (Just c) = do
          (field @"decks") %= (Map.mapWithKey (decreaseCards c))
          (field @"players" . ix p . field @"deck") %= (c:)
          return $ Just c
        highestCostCard = find (\c -> (c ^. field @"cost") < highestPrice) cards
