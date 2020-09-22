{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module DeckBuilding.Dominion.Utils
    ( deal
    , numEmptyDecks
    , firstCardInPlay
    , decreaseCards
    , isCardInPlay
    , findPlayer
    , removeFromCards
    , discardCard
    ) where

import Control.Lens ( preuse, (^.), use, (%=), (.=), Ixed(ix) )
import Control.Monad (filterM)
import Control.Monad.RWS
    ( MonadWriter(tell), MonadState(get) )
import qualified Data.DList as DL
import Data.Generics.Product ( HasField(field) )
import Data.List (delete, find)
import qualified Data.Map as Map
import DeckBuilding.Dominion.Types
    ( DominionPlayer, Card, DominionState, DominionMove(Deal) )
import System.Random (split)
import System.Random.Shuffle ( shuffle' )

-- | Deal n cards, reshuffling the player's deck if needed.
deal :: Int -> Int -> DominionState [Card]
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
  (field @"players" . ix pnum . field @"deck") .= newDeck
  (field @"players" . ix pnum . field @"discard") .= newDiscard
  (field @"players" . ix pnum . field @"hand") %= (++ newCards)
  tell $ DL.singleton $ Deal num newCards
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
findPlayer :: Int -> DominionState DominionPlayer
findPlayer p = do
  mp <- preuse(field @"players" . ix p)
  case mp of
    Just player' -> pure player'
    Nothing      -> error $ "Unable to find player #" <> show p

-- | Remove this list of cards from that list of cards.
removeFromCards :: [Card] -> [Card] -> [Card]
removeFromCards = foldr delete

-- | Discard a single card, primarily intended for evaluateHand
-- so we can discard Action cards when the player has no actions
-- left.
discardCard :: Card -> Int -> DominionState ()
discardCard card p = do
  thePlayer <- findPlayer p
  let newHand = removeFromCards (thePlayer ^. field @"hand") [card]
  (field @"players" . ix p . field @"discard") %= (++ [card])
  (field @"players" . ix p . field @"hand") .= newHand
