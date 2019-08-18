{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}

module DeckBuilding.Dominion.Cards.Intrigue
    ( courtyardCard
    , lurkerCard
    , shantyTownCard
    , conspiratorCard
    , ironworksCard
    , dukeCard
    , haremCard
    ) where

import           Control.Lens
import           Data.Generics.Product
import           Data.List                         (delete)
import qualified Data.Map                          as Map
import           DeckBuilding.Dominion.Cards.Base
import           DeckBuilding.Dominion.Cards.Utils
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils

courtyardCardAction :: Card -> Int -> DominionState Int
courtyardCardAction c p = do
  player <- findPlayer p
  _ <- (player ^. field @"strategy" . field @"handToDeckStrategy") 1 p
  basicCardAction 3 (-1) 0 0 0 c p

courtyardCard :: Card
courtyardCard   = Card "Courtyard"    2 courtyardCardAction Action

lurk :: Either Card Card -> Int -> DominionState Int
lurk (Left c) p                       = do
  icip <- isCardInPlay c
  if icip
    then do
      (field @"trash") %= (c:)
      (field @"decks") %= (Map.mapWithKey (decreaseCards c))
      return p
    else return p
lurk (Right c@(Card _ _ _ Action)) p  = do
  trsh <- use $ field @"trash"
  if c `elem` trsh
    then do
      (field @"trash") %= (delete c)
      (field @"players" . ix p . field @"discard") %= (c:)
      return p
    else return p
lurk (Right _) p                      = return p

lurkerCardAction :: Card -> Int -> DominionState Int
lurkerCardAction c p = do
  player <- findPlayer p
  ec <- (player ^. field @"strategy" . field @"lurkerStrategy") c p
  _ <- lurk ec p
  basicCardAction 0 0 0 0 0 c p

lurkerCard :: Card
lurkerCard      = Card "Lurker"   2 lurkerCardAction Action

hasActionCards :: Int -> [Card] -> Bool
hasActionCards num cs = num <= length (filter (\c -> (c ^. field @"cardType") == Action) cs)

shantyTownCardAction :: Card -> Int -> DominionState Int
shantyTownCardAction c p = do
  player <- findPlayer p
  if hasActionCards 1 (player ^. field @"hand")
    then basicCardAction 0 1 0 0 0 c p
    else basicCardAction 2 1 0 0 0 c p

shantyTownCard :: Card
shantyTownCard  = Card "Shanty Town"  3 shantyTownCardAction Action

conspiratorCardAction :: Card -> Int -> DominionState Int
conspiratorCardAction c p = do
  player <- findPlayer p
  if hasActionCards 2 (player ^. field @"played")
    then basicCardAction 1 0 0 2 0 c p
    else basicCardAction 0 (-1) 0 2 0 c p

conspiratorCard :: Card
conspiratorCard = Card "Conspirator"  4 conspiratorCardAction Action

ironworksCardAction :: Card -> Int -> DominionState Int
ironworksCardAction c p = do
  player <- findPlayer p
  mc <- (player ^. field @"strategy" . field @"gainCardStrategy") 4 p
  case mc of
    Nothing   -> return p
    Just card
          | (card ^. field @"cardType") == Action -> basicCardAction 0 0 0 0 0 c p
          | card `elem` treasureCards             -> basicCardAction 0 (-1) 0 1 0 c p
          | card `elem` victoryCards              -> basicCardAction 1 (-1) 0 0 0 c p
          | otherwise                             -> basicCardAction 0 (-1) 0 0 0 c p

ironworksCard :: Card
ironworksCard   = Card "Ironworks"    4 ironworksCardAction Action

dukeCardAction :: Card -> Int -> DominionState Int
dukeCardAction c p = do
  player <- findPlayer p
  let points = length $ filter (== duchyCard) ( (player ^. field @"hand") ++ (player ^. field @"discard") ++ (player ^. field @"played") ++ (player ^. field @"deck") )
  valueCard 0 points c p

dukeCard :: Card
dukeCard        = Card "Duke"         5 dukeCardAction Action

haremCard :: Card
haremCard       = Card "Harem"        6 (valueCard 2 2) Value
