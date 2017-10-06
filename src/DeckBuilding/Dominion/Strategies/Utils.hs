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
    ) where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Utils

import qualified Data.Map                          as Map
import Control.Monad.State
import Control.Lens

-- | Can this player afford this card?
canAfford :: Card -> Player -> Bool
canAfford c p = (c ^. cost) <= (p ^. money)

-- | Are there any of this card left in the game?
cardsLeft :: Game -> Card -> Bool
cardsLeft gs c = Map.member c (gs ^. decks) && ((gs ^. decks) Map.! c > 0)

-- | Buy the card if it satisfies the passed in function, the player can
--  afford it, and there are some left in the supply.
buyIf :: Card -> Player -> (Card -> Player -> State Game Bool) -> State Game Bool
buyIf c p f = do
  gs <- get
  iff <- f c p
  if iff && canAfford c p && cardsLeft gs c
    then do
      buyCard (Just c) p
      return True
    else return False

-- | Helper function when you always want to buy a card if you can afford it.
alwaysBuy :: Card -> Player -> State Game Bool
alwaysBuy c p = buyIf c p (\_ _ -> return True)

-- | How many of this card does the player have?
countCards :: Card -> Player -> Int
countCards c p = length $ filter (== c) $ (p ^. hand) ++ (p ^. deck) ++ (p ^. discard) ++ (p ^. played)

countDeck :: Player -> Int
countDeck p = length $ (p ^. hand) ++ (p ^. deck) ++ (p ^. discard) ++ (p ^. played)

-- | Helper function for a card where you only want to buy up to N of them.
buyN :: Int -> Card -> Player -> State Game Bool
buyN n c p = buyNIf n c p (\_ _ -> return True)

-- | Buy up to N of the card as long as it satisfies the passed in function.
buyNIf :: Int -> Card -> Player -> (Card -> Player -> State Game Bool) -> State Game Bool
buyNIf n c p f = do
  iff <- f c p
  if iff
    then buyIf c p (\c p -> return (countCards c p < n))
    else return False

-- | Buy N of the card as long as the player's total deck size is D.
buyNAfterTotalDeckOf :: Int -> Int -> Card -> Player -> State Game Bool
buyNAfterTotalDeckOf n d c p = buyNIf n c p (\c' p' -> return (countDeck p' >= d))

-- | Decrease the amount of the cards in the game deck, subtract the money
--  from the player, and add the card to the player's discard pile.
buyCard ::  Maybe Card -> Player -> State Game Player
buyCard Nothing  p = return p
buyCard (Just c) p = do
  gs <- get
  put $ over decks (Map.mapWithKey (decreaseCards c)) gs
  return $ over discard (c:) $ over buys (+ (-1)) $ over money (\m -> m - (c ^. cost)) p
