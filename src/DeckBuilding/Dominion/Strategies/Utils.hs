module DeckBuilding.Dominion.Strategies.Utils
    ( canAfford
    , cardsLeft
    , alwaysBuy
    , buyIf
    , countCards
    , countDeck
    , buyN
    , buyNIf
    , buyNAfterTotalDeckOf
    , buyCard
    , buyIfNumberOfCardIsBelow
    , buyIfLowerThanTerminalActions
    ) where

import           DeckBuilding.Dominion.Cards
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils

import           Control.Lens
import           Control.Monad.RWS
import qualified Data.DList                  as DL
import           Data.List                   (intersect)
import qualified Data.Map                    as Map

-- | Can this player afford this card?
canAfford :: Card -> Player -> Bool
canAfford c p = (c ^. cost) <= (p ^. money)

cardsLeft :: DominionGame -> Card -> Int
cardsLeft gs c = if Map.member c (gs ^. decks)
  then (gs ^. decks) Map.! c
  else 0

-- | Are there any of this card left in the game?
areCardsLeft :: DominionGame -> Card -> Bool
areCardsLeft gs c = Map.member c (gs ^. decks) && ((gs ^. decks) Map.! c > 0)

-- | Buy the card if it satisfies the passed in function, the player can
--  afford it, and there are some left in the supply.
buyIf :: Card -> Int -> (Card -> Player -> DominionState Bool) -> DominionState (Maybe Card)
buyIf c p f = do
  (Just player) <- preuse (players . ix p)
  gs <- get
  iff <- f c player
  if iff && canAfford c player && areCardsLeft gs c
    then do
      buyCard (Just c) p
      return $ Just c
    else return Nothing

-- | Helper function when you always want to buy a card if you can afford it.
alwaysBuy :: Card -> Int -> DominionState (Maybe Card)
alwaysBuy c p = buyIf c p (\_ _ -> return True)

allCards :: Player -> [Card]
allCards p = (p ^. hand) ++ (p ^. deck) ++ (p ^. discard) ++ (p ^. played)

-- | How many of this card does the player have?
countCards :: Card -> Player -> Int
countCards c p = length $ filter (== c) $ allCards p

countDeck :: Player -> Int
countDeck p = length $ allCards p

-- | Helper function for a card where you only want to buy up to N of them.
buyN :: Int -> Card -> Int -> DominionState (Maybe Card)
buyN n c p = buyNIf n c p (\_ _ -> return True)

-- | Buy up to N of the card as long as it satisfies the passed in function.
buyNIf :: Int -> Card -> Int -> (Card -> Player -> DominionState Bool) -> DominionState (Maybe Card)
buyNIf n c p f = do
  (Just player) <- preuse (players . ix p)
  iff <- f c player
  if iff
    then buyIf c p (\c p -> return ((countCards c p) < n))
    else return Nothing

-- | Buy N of the card as long as the player's total deck size is D.
buyNAfterTotalDeckOf :: Int -> Int -> Card -> Int -> DominionState (Maybe Card)
buyNAfterTotalDeckOf n d c p = buyNIf n c p (\c' p' -> return (countDeck p' >= d))

isDeckBelowN :: Card -> Int -> DominionState Bool
isDeckBelowN c n = do
  gs <- get
  if n > cardsLeft gs c
    then return True
    else return False

buyIfNumberOfCardIsBelow :: Card -> Int -> Card -> Int -> DominionState (Maybe Card)
buyIfNumberOfCardIsBelow cd n c p = do
  db <- isDeckBelowN cd n
  if db
    then alwaysBuy c p
    else return Nothing

actionTerminators :: Player -> Int
actionTerminators p =  length $ allCards p `intersect` actionTerminatorCards

buyIfLowerThanTerminalActions :: Card -> Int -> DominionState (Maybe Card)
buyIfLowerThanTerminalActions c p = do
  (Just player) <- preuse (players . ix p)
  if (countCards c player) < (actionTerminators player)
    then alwaysBuy c p
    else return Nothing

-- | Decrease the amount of the cards in the game deck, subtract the money
--  from the player, and add the card to the player's discard pile.
buyCard ::  Maybe Card -> Int -> DominionState Int
buyCard Nothing  p = return p
buyCard (Just c) p = do
  tell $ DL.singleton $ Buy c
  decks %= (Map.mapWithKey (decreaseCards c))
  (players . ix p . discard) %= (c:)
  (players . ix p . buys) -= 1
  (players . ix p . money) -= (c ^. cost)
  return p