{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE GADTs                     #-}

module DeckBuilding.Dominion.Cards.Utils
    ( simpleVictory
    , hasActionCards
    , valueCardAction
    , basicCardAction
    , trashCards
    , discardCards
    , handToDeck
    , discardToDeck
    , gainCardsToDeck
    , gainCardsToHand
    ) where

import Control.Lens ( (^.), (%=), (+=), Ixed(ix), (.=), use )
import Data.Generics.Product ( HasField(field) )
import Data.Generics.Labels ()
import Data.List ( find )
import DeckBuilding.Types ( PlayerNumber(unPlayerNumber) )
import DeckBuilding.Dominion.Types
    ( Card, CardType(Action), DominionState, DominionAction,
      DominionDraw(DominionDraw) )
import DeckBuilding.Dominion.Utils
    ( deal, findPlayer, removeFromCards, decreaseCards, isCardInPlay )
import qualified Data.Map as Map

-- | A simple points-only Victory card
-- | Victory Points
-- | Player Number
simpleVictory :: Int -> PlayerNumber -> DominionState Int
simpleVictory v p = do
  (#players . ix (unPlayerNumber p) . #victory) += v
  thePlayer <- findPlayer p
  return $ thePlayer ^. #victory

valueCardAction :: Int -> DominionAction -> PlayerNumber -> DominionState (Maybe DominionAction)
valueCardAction m a p = do
  (#players . ix (unPlayerNumber p) . #money) += m
  pure $ Just a

basicCardAction :: Int -- ^ Number of cards to draw
  -> Int -- ^ Number of actions (minus the one being used now)
  -> Int -- ^ Number of additional buys
  -> Int -- ^ Amount of additional money
  -> PlayerNumber -- ^ Player number of the current player
  -> DominionState DominionDraw
basicCardAction d a b m p = do
  (#players . ix (unPlayerNumber p) . #actions) += a
  (#players . ix (unPlayerNumber p) . #buys) += b
  (#players . ix (unPlayerNumber p) . #money) += m
  theDraw <- deal d p
  pure $ DominionDraw theDraw

hasActionCards :: Int -> [Card] -> Bool
hasActionCards num cs = num <= length (filter (\c -> (c ^. #cardType) == Action) cs)

trashCards :: PlayerNumber -> [Card] -> DominionState ()
trashCards p toTrash = do
  thePlayer <- findPlayer p
  #trash %= (toTrash ++)
  #players . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) toTrash

discardCards :: PlayerNumber -> [Card] -> DominionState ()
discardCards p toDiscard = do
  thePlayer <- findPlayer p
  #players . ix (unPlayerNumber p) . #discard %= (toDiscard ++)
  #players . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) toDiscard

handToDeck :: PlayerNumber -> [Card] -> DominionState ()
handToDeck p cards = do
  thePlayer <- findPlayer p
  #players . ix (unPlayerNumber p) . #deck %= (cards ++)
  #players . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) cards

discardToDeck :: PlayerNumber -> [Card] -> DominionState ()
discardToDeck p cards = do
  thePlayer <- findPlayer p
  #players . ix (unPlayerNumber p) . #discard .= removeFromCards (thePlayer ^. #discard) cards
  #players . ix (unPlayerNumber p) . #deck %= (cards ++)

gainCardsToDeck :: PlayerNumber -> [Card] -> DominionState [Card]
gainCardsToDeck _ [] = return []
gainCardsToDeck p (x:xs) = do
  hasCard <- isCardInPlay x
  if hasCard
    then do
      #decks %= Map.mapWithKey (decreaseCards x)
      #players . ix (unPlayerNumber p) . #deck %= (x:)
      theRest <- gainCardsToDeck p xs
      return $ x : theRest
    else gainCardsToDeck p xs

gainCardsToHand :: PlayerNumber -> [Card] -> DominionState [Card]
gainCardsToHand _ [] = return []
gainCardsToHand p (x:xs) = do
  hasCard <- isCardInPlay x
  if hasCard
    then do
      #decks %= Map.mapWithKey (decreaseCards x)
      #players . ix (unPlayerNumber p) . #hand %= (x:)
      theRest <- gainCardsToHand p xs
      return $ x : theRest
    else gainCardsToHand p xs
