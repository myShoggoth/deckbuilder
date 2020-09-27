{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}

module DeckBuilding.Dominion.Cards.Intrigue
    ( courtyardCard
    , lurkerCard
    , shantyTownCard
    , conspiratorCard
    , ironworksCard
    , dukeCard
    , haremCard
    ) where

import Control.Lens ( (^.), use, (%=), Ixed(ix) )
import Data.Generics.Product ( HasField(field) )
import Data.List (delete)
import qualified Data.Map as Map
import DeckBuilding.Dominion.Cards.Base
    ( treasureCards, duchyCard, victoryCards )
import DeckBuilding.Dominion.Cards.Utils
    ( simpleVictory, valueCard, basicCardAction, hasActionCards )
import DeckBuilding.Types ( PlayerNumber(unPlayerNumber) )
import DeckBuilding.Dominion.Types
    ( Card(Card), CardType(Value, Action), DominionState )
import DeckBuilding.Dominion.Utils
    ( decreaseCards, isCardInPlay, findPlayer )

-- | +3 Cards
--
-- Put a card from your hand onto your deck.
courtyardCard :: Card
courtyardCard   = Card "Courtyard"    2 courtyardCardAction Action (simpleVictory 0)
  where
    courtyardCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    courtyardCardAction c p = do
      thePlayer <- findPlayer p
      _ <- (thePlayer ^. #strategy . #handToDeckStrategy) 1 p
      basicCardAction 3 (-1) 0 0 c p

-- | +1 Action
--
-- Choose one: Trash an Action card from the Supply; or gain an Action card from the trash.
lurkerCard :: Card
lurkerCard      = Card "Lurker"   2 lurkerCardAction Action (simpleVictory 0)
  where
    lurkerCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    lurkerCardAction c p = do
      thePlayer <- findPlayer p
      ec <- (thePlayer ^. #strategy . #lurkerStrategy) c p
      _ <- lurk ec p
      basicCardAction 0 0 0 0 c p
    lurk :: Either Card Card -> PlayerNumber -> DominionState PlayerNumber
    lurk (Left c) p                       = do
      icip <- isCardInPlay c
      if icip
        then do
          field @"trash" %= (c:)
          field @"decks" %= Map.mapWithKey (decreaseCards c)
          return p
        else return p
    lurk (Right c@(Card _ _ _ Action _)) p  = do
      trsh <- use #trash
      if c `elem` trsh
        then do
          field @"trash" %= delete c
          (field @"players" . ix (unPlayerNumber p) . #discard) %= (c:)
          return p
        else return p
    lurk (Right _) p                      = return p

-- | +2 Actions
--
-- Reveal your hand. If you have no Action cards in hand, +2 Cards.
shantyTownCard :: Card
shantyTownCard  = Card "Shanty Town"  3 shantyTownCardAction Action (simpleVictory 0)
  where
    shantyTownCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    shantyTownCardAction c p = do
      thePlayer <- findPlayer p
      if hasActionCards 1 (thePlayer ^. #hand)
        then basicCardAction 0 1 0 0 c p
        else basicCardAction 2 1 0 0 c p

-- | +$2
--
-- If you’ve played 3 or more Actions this turn (counting this), +1 Card and +1 Action.
conspiratorCard :: Card
conspiratorCard = Card "Conspirator"  4 conspiratorCardAction Action (simpleVictory 0)
  where
    conspiratorCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    conspiratorCardAction c p = do
      thePlayer <- findPlayer p
      if hasActionCards 2 (thePlayer ^. #played)
        then basicCardAction 1 0 0 2 c p
        else basicCardAction 0 (-1) 0 2 c p

-- | Gain a card costing up to $4. If the gained card is an…
--
-- Action card, +1 Action
--
-- Treasure card, +$1
--
-- Victory card, +1 Card
ironworksCard :: Card
ironworksCard   = Card "Ironworks"    4 ironworksCardAction Action (simpleVictory 0)
  where
    ironworksCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    ironworksCardAction c p = do
      thePlayer <- findPlayer p
      mc <- (thePlayer ^. #strategy . #gainCardStrategy) 4 p
      case mc of
        Nothing   -> return p
        Just card
              | (card ^. #cardType) == Action -> basicCardAction 0 0 0 0 c p
              | card `elem` treasureCards             -> basicCardAction 0 (-1) 0 1 c p
              | card `elem` victoryCards              -> basicCardAction 1 (-1) 0 0 c p
              | otherwise                             -> basicCardAction 0 (-1) 0 0 c p

-- | Worth 1VP per Duchy you have.
dukeCard :: Card
dukeCard        = Card "Duke"         5 (valueCard 0) Action dukeCardAction
  where
    dukeCardAction :: Card -> PlayerNumber -> DominionState Int
    dukeCardAction _ p = do
      thePlayer <- findPlayer p
      let points = length $ filter (== duchyCard) ( (thePlayer ^. #hand) ++ (thePlayer ^. #discard) ++ (thePlayer ^. #played) ++ (thePlayer ^. #deck) )
      return points

-- | $2
--
-- 2VP
haremCard :: Card
haremCard       = Card "Harem"        6 (valueCard 2) Value (simpleVictory 2)
