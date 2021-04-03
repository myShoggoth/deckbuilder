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
    , executeBuys
    , mkDominionAIGame
    , cardPlayed
    ) where

import Control.Lens ( preuse, (^.), use, (%=), (.=), Ixed(ix), (-=), (+=), (<>=))
import Control.Monad ( filterM, void, when )
import Control.Monad.RWS
    ( MonadWriter(tell), MonadState(get) )
import qualified Data.DList as DL
import Data.Generics.Product ( HasField(field) )
import Data.Generics.Labels ()
import Data.List ( delete, find, group, sort )
import qualified Data.Map as Map
import DeckBuilding.Types ( PlayerNumber(unPlayerNumber) )
import DeckBuilding.Dominion.Types
    ( DominionPlayer,
      Card(cardName),
      DominionAIGame(..),
      DominionState,
      DominionMove(Buy, Deal, Cellar, PlayValue, PlayBasic,
        Vassal, MoneyLender, Poacher, ThroneRoom) )
import System.Random (split)
import System.Random.Shuffle ( shuffle' )

import Debug.Trace

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

-- | Move a card from the player's hand to their played pile.
cardPlayed :: Card -> PlayerNumber -> DominionState ()
cardPlayed c p = do
  thePlayer <- findPlayer p
  (field @"players" . ix (unPlayerNumber p) . #played) <>= [c]
  (field @"players" . ix (unPlayerNumber p) . #hand) .= removeFromCards (thePlayer ^. #hand) [c]

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
  cardsInPlay <- filterM isCardInPlay cs
  return $ find (const True) $ tail cardsInPlay

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
executeBuys :: [DominionMove] -> DominionAIGame -> DominionState ()
-- executeBuys xs g | trace ("executeBuys: " <> show g <> ", buys: " <> show xs) False=undefined
executeBuys [] _ = return ()
executeBuys (x:xs) g = do
  case x of
    (Buy p c) -> buyCard p c
    _ -> error "Running non-buy DominionMove in executeBuys."
  mkDominionAIGame (g ^. #playerNum) >>= executeBuys xs
  where
    -- | Decrease the amount of the cards in the game deck, subtract the money
    --  from the player, and add the card to the player's discard pile.
    buyCard :: PlayerNumber -> Card -> DominionState ()
    buyCard p c = do
      thePlayer <- findPlayer p
      tell $ DL.singleton $ Buy p c
      when (thePlayer ^. #buys <= 0) $
        error $ "Buy move requested by " <> show p <> " without buys.\n" <> show thePlayer
      when ((c ^. #cost) > (thePlayer ^. #money)) $
        error $ "Buy move requested by " <> show p <> " without enough money.\n" <> show thePlayer
      decks <- use #decks
      when (decks Map.! c <= 0) $ -- TODO: Couldn't make the lens version see the type instance, why not?
        error $ "Buy move requested by " <> show p <> " with empty deck of " <> show (cardName c) <> ".\n"
      field @"decks" %= Map.mapWithKey (decreaseCards c)
      (field @"players" . ix (unPlayerNumber p) . #discard) %= (c:)
      (field @"players" . ix (unPlayerNumber p) . #buys) -= 1
      (field @"players" . ix (unPlayerNumber p) . #money) -= (c ^. #cost)
    -- | For value cards, pass the money value.
    --
    -- Money Value
    --
    -- Card
    --
    -- Player Number
    valueCard :: Int -> Card -> PlayerNumber -> DominionState ()
    valueCard m c p = do
      (field @"players" . ix (unPlayerNumber p) . #hand) %= delete c
      (field @"players" . ix (unPlayerNumber p) . #played) %= (c:)
      (field @"players" . ix (unPlayerNumber p) . #money) += m
    -- | For basic card values:
    --
    -- Draw cards
    --
    -- +actions
    --
    -- +buys
    --
    -- +money
    basicCard :: Int -> Int -> Int -> Int -> Card -> PlayerNumber -> DominionState ()
    basicCard draw a b m c p = do
      (field @"players" . ix (unPlayerNumber p) . #actions) += a
      (field @"players" . ix (unPlayerNumber p) . #buys) += b
      void $ deal draw p
      valueCard m c p

mkDominionAIGame :: PlayerNumber -> DominionState DominionAIGame
mkDominionAIGame pnum = do
  thePlayer <- findPlayer pnum
  decks' <- use $ #decks
  trash' <- use $ #trash
  pure DominionAIGame
    { playerNum = pnum
    , hand = thePlayer ^. #hand
    , played = thePlayer ^. #played
    , actions = thePlayer ^. #actions
    , buys = thePlayer ^. #buys
    , money = thePlayer ^. #money
    , turns = thePlayer ^. #turns
    , cards = buildCardMap thePlayer
    , trash = trash'
    , decks = decks'
    }
  where
    buildCardMap :: DominionPlayer -> Map.Map Card Int
    buildCardMap p = Map.fromList $ map (\x -> (head x, length x)) $ group $ sort allCards
      where
        allCards = (p ^. #hand) <> (p ^. #played) <> (p ^. #discard) <> (p ^. #deck)

