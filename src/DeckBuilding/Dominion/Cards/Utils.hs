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
    ) where

import           Control.Lens
import           Data.Generics.Product
import           Data.List
import qualified Data.Map                    as Map
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils

-- | For value cards, pass money and victory point values.
valueCard :: Int -> Int -> Card -> Int -> DominionState Int
valueCard m v c p = do
  (field @"players" . ix p . field @"hand") %= (delete c)
  (field @"players" . ix p . field @"played") %= (c:)
  (field @"players" . ix p . field @"money") += m
  (field @"players" . ix p . field @"victory") += v
  return p

-- | For basic card values: draw cards, +actions, +buys, +money, +victory
basicCardAction :: Int -> Int -> Int -> Int -> Int -> Card -> Int -> DominionState Int
basicCardAction draw a b m v c p = do
  (field @"players" . ix p . field @"actions") += a
  (field @"players" . ix p . field @"buys") += b
  _ <- deal draw p
  valueCard m v c p


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
