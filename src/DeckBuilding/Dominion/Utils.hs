{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedLabels          #-}

module DeckBuilding.Dominion.Utils
    ( deal
    , numEmptyDecks
    , firstCardInPlay
    , decreaseCards
    , isCardInPlay
    , findPlayer
    , removeFromCards
    , discardCard
    , executeMoves
    ) where

import Control.Lens ( preuse, (^.), use, (%=), (.=), Ixed(ix), (-=) )
import Control.Monad ( filterM, void, when )
import Control.Monad.RWS
    ( MonadWriter(tell), MonadState(get) )
import qualified Data.DList as DL
import Data.Generics.Product ( HasField(field) )
import Data.Generics.Labels ()
import Data.List (delete, find)
import qualified Data.Map as Map
import DeckBuilding.Types ( PlayerNumber(unPlayerNumber) )
import DeckBuilding.Dominion.Types
    ( DominionPlayer, Card, DominionState, DominionMove(Deal, Buy), DominionAIGame )
import System.Random (split)
import System.Random.Shuffle ( shuffle' )

-- | Deal n cards, reshuffling the player's deck if needed.
deal :: Int -> PlayerNumber -> DominionState [Card]
deal 0   _    = return []
deal num pnum = do
  p <- findPlayer pnum
  r <- use $ field @"random"
  let (enoughDeck, newDiscard)
          | length (p ^. field @"deck") >= num   = (p ^. field @"deck", p ^. field @"discard")
          | null (p ^. field @"discard")         = (p ^. field @"deck", [])
          | otherwise                            = ( (p ^. field @"deck") ++ shuffle' (p ^. field @"discard") (length (p ^. field @"discard")) r, [])
  let (newCards, newDeck)  = splitAt num enoughDeck
  field @"random" %= snd . split
  (field @"players" . ix (unPlayerNumber pnum) . field @"deck") .= newDeck
  (field @"players" . ix (unPlayerNumber pnum) . field @"discard") .= newDiscard
  (field @"players" . ix (unPlayerNumber pnum) . field @"hand") %= (++ newCards)
  tell $ DL.singleton $ Deal pnum num newCards
  return newCards

-- | How many of the game's decks have been emptied?
numEmptyDecks :: DominionState Int
numEmptyDecks = do
  decks' <- use $ field @"decks"
  return $ length $ Map.filter (== 0) decks'

-- | If the cards are the same, return number of cards - 1.
decreaseCards :: Card -> Card -> Int -> Int
decreaseCards  _  _ 0 = 0
decreaseCards c1 c2 n = if c1 == c2
    then n - 1
    else n

-- | Is this card part of this game, and if so are there any left?
isCardInPlay :: Card -> DominionState Bool
isCardInPlay c = do
  gs <- get
  return $ c `Map.member` (gs ^. field @"decks") && (gs ^. field @"decks") Map.! c > 0

-- | Find the first card, if any, in the list which is still in play.
firstCardInPlay :: [Card] -> DominionState (Maybe Card)
firstCardInPlay cs = do
  cards <- filterM isCardInPlay cs
  return $ find (const True) $ tail cards

-- | Find player # n, error if not found
findPlayer :: PlayerNumber -> DominionState DominionPlayer
findPlayer p = do
  mp <- preuse(field @"players" . ix (unPlayerNumber p))
  case mp of
    Just player' -> pure player'
    Nothing      -> error $ "Unable to find player #" <> show p

-- | Remove this list of cards from that list of cards.
removeFromCards :: [Card] -> [Card] -> [Card]
removeFromCards = foldr delete

-- | Discard a single card, primarily intended for evaluateHand
-- so we can discard Action cards when the player has no actions
-- left.
discardCard :: Card -> PlayerNumber -> DominionState ()
discardCard card p = do
  thePlayer <- findPlayer p
  let newHand = removeFromCards (thePlayer ^. field @"hand") [card]
  (field @"players" . ix (unPlayerNumber p) . field @"discard") %= (++ [card])
  (field @"players" . ix (unPlayerNumber p) . field @"hand") .= newHand

-- | Run the moves the AI has requested, this is where the bulk of the
-- game state changes actually take place.
executeMoves :: DominionAIGame -> [DominionMove] -> DominionState ()
-- executeMoves g xs | trace ("executeMoves: " <> show g <> ", moves: " <> show xs) False=undefined
executeMoves _ [] = return ()
executeMoves g (x:xs) = void $ case x of
    (Buy p c) -> buyCard c p *> executeMoves g xs
    _ -> error "Unimplemented DominionMove execution"
  where
    -- | Decrease the amount of the cards in the game deck, subtract the money
    --  from the player, and add the card to the player's discard pile.
    buyCard :: Card -> PlayerNumber -> DominionState ()
    buyCard c p = do
      thePlayer <- findPlayer p
      tell $ DL.singleton $ Buy p c
      when (thePlayer ^. #buys <= 0) $
        error $ "Buy move requested by " <> show p <> " without buys.\n" <> show thePlayer
      when ((c ^. #cost) > (thePlayer ^. #money)) $
        error $ "Buy move requested by " <> show p <> " without enough money.\n" <> show thePlayer
      field @"decks" %= Map.mapWithKey (decreaseCards c)
      (field @"players" . ix (unPlayerNumber p) . #discard) %= (c:)
      (field @"players" . ix (unPlayerNumber p) . #buys) -= 1
      (field @"players" . ix (unPlayerNumber p) . #money) -= (c ^. #cost)
