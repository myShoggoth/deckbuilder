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
    ( simpleVictory, basicCardAction, hasActionCards, handToDeck, valueCardAction )
import DeckBuilding.Types ( PlayerNumber(unPlayerNumber) )
import DeckBuilding.Dominion.Types
    ( Card(Card), CardType(Value, Action), DominionState, DominionAction (Courtyard, Lurker, ShantyTown, Conspirator, Ironworks, Duke, Harem), DominionDraw(DominionDraw) )
import DeckBuilding.Dominion.Utils
    ( decreaseCards, isCardInPlay, findPlayer, mkDominionAIGame )

-- | +3 Cards
--
-- Put a card from your hand onto your deck.
courtyardCard :: Card
courtyardCard   = Card "Courtyard"    2 courtyardCardAction Action (simpleVictory 0)
  where
    courtyardCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    courtyardCardAction p = do
      theDeal <- basicCardAction 3 (-1) 0 0 p
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let cs = (thePlayer ^. #strategy . #handToDeckStrategy) aig 1
      handToDeck p cs
      pure $ Just $ Courtyard theDeal cs

-- | +1 Action
--
-- Choose one: Trash an Action card from the Supply; or gain an Action card from the trash.
lurkerCard :: Card
lurkerCard      = Card "Lurker"   2 lurkerCardAction Action (simpleVictory 0)
  where
    lurkerCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    lurkerCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let ec = (thePlayer ^. #strategy . #lurkerStrategy) aig
      ml <- lurk ec p
      case ml of
        Nothing -> pure Nothing
        Just l -> pure $ Just $ Lurker l
    lurk :: Either Card Card -> PlayerNumber -> DominionState (Maybe (Either Card Card))
    lurk e@(Left c) _ = do
      icip <- isCardInPlay c
      if icip
        then do
          field @"trash" %= (c:)
          field @"decks" %= Map.mapWithKey (decreaseCards c)
          return $ Just e
        else return Nothing
    lurk e@(Right c@(Card _ _ _ Action _)) p = do
      trsh <- use #trash
      if c `elem` trsh
        then do
          field @"trash" %= delete c
          (field @"players" . ix (unPlayerNumber p) . #discard) %= (c:)
          return $ Just e
        else return Nothing
    lurk (Right _) _ = return Nothing

-- | +2 Actions
--
-- Reveal your hand. If you have no Action cards in hand, +2 Cards.
shantyTownCard :: Card
shantyTownCard  = Card "Shanty Town"  3 shantyTownCardAction Action (simpleVictory 0)
  where
    shantyTownCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    shantyTownCardAction p = do
      thePlayer <- findPlayer p
      let h = thePlayer ^. #hand
      theDeal <- if hasActionCards 1 h
        then basicCardAction 0 1 0 0 p
        else basicCardAction 2 1 0 0 p
      pure $ Just $ ShantyTown theDeal h

-- | +$2
--
-- If you’ve played 3 or more Actions this turn (counting this), +1 Card and +1 Action.
conspiratorCard :: Card
conspiratorCard = Card "Conspirator"  4 conspiratorCardAction Action (simpleVictory 0)
  where
    conspiratorCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    conspiratorCardAction p = do
      thePlayer <- findPlayer p
      theDeal <- if hasActionCards 2 (thePlayer ^. #played)
        then basicCardAction 1 0 0 2 p
        else basicCardAction 0 (-1) 0 2 p
      pure $ Just $ Conspirator theDeal

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
    ironworksCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    ironworksCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let mc = (thePlayer ^. #strategy . #gainCardStrategy) aig 4
      case mc of
        Nothing   -> return Nothing
        Just card
              | (card ^. #cardType) == Action -> pure $ Just $ Ironworks card (DominionDraw [])
              | card `elem` treasureCards             -> do
                _ <- basicCardAction 0 (-1) 0 1 p
                pure $ Just $ Ironworks card (DominionDraw [])
              | card `elem` victoryCards              -> do
                theDeal <- basicCardAction 1 (-1) 0 0 p
                pure $ Just $ Ironworks card theDeal
        Just _ -> error "Ironworks: this should never happen."

-- | Worth 1VP per Duchy you have.
dukeCard :: Card
dukeCard        = Card "Duke"         5 (valueCardAction 0 Duke) Action dukeCardAction
  where
    dukeCardAction :: PlayerNumber -> DominionState Int
    dukeCardAction p = do
      thePlayer <- findPlayer p
      let points = length $ filter (== duchyCard) ( (thePlayer ^. #hand) ++ (thePlayer ^. #discard) ++ (thePlayer ^. #played) ++ (thePlayer ^. #deck) )
      return points

-- | $2
--
-- 2VP
haremCard :: Card
haremCard       = Card "Harem"        6 (valueCardAction 2 Harem) Value (simpleVictory 2)
