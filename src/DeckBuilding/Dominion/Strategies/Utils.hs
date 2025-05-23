{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}

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
    , buyIfNumberOfCardIsBelow
    , buyIfLowerThanTerminalActions
    , cardWeightCompare
    , sortByWeight
    , gainWhichCard
    ) where

import Control.Lens ( folded, sumOf, (^.) )
import Data.List (sortBy, find)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import DeckBuilding.Dominion.Cards ( actionTerminatorCards )
import DeckBuilding.Dominion.Types
    ( Card,
      DominionAIGame,
      DominionBuy(DominionBuy) )

-- | Can this player afford this card?
canAfford :: DominionAIGame -> Card -> Bool
canAfford g c = (c ^. #cost) <= (g ^. #money)

cardsLeft :: DominionAIGame -> Card -> Int
cardsLeft g c = (g ^. #decks) Map.! c

-- | Are there any of this card left in the game?
areCardsLeft :: DominionAIGame -> Card -> Bool
areCardsLeft g c = Map.member c (g ^. #decks) && ((g ^. #decks) Map.! c > 0)

-- | Buy the card if it satisfies the passed in function, the player can
--  afford it, and there are some left in the supply.
buyIf :: DominionAIGame -> Card -> (DominionAIGame -> Card -> Bool) -> Maybe DominionBuy
buyIf g c f =
  if f g c  && canAfford g c && areCardsLeft g c
    then Just $ DominionBuy (c ^. #cost) c
    else Nothing

-- | Helper function when you always want to buy a card if you can afford it.
alwaysBuy :: DominionAIGame -> Card -> Maybe DominionBuy
alwaysBuy g c = buyIf g c (\_ _ -> True)

-- | How many of this card does the player have?
countCards :: DominionAIGame -> Card -> Int
countCards g c = fromMaybe 0 $ Map.lookup c $ g ^. #cards

countDeck :: DominionAIGame -> Int
countDeck = sumOf (#cards . folded)

-- | Helper function for a card where you only want to buy up to N of them.
buyN :: Int -> DominionAIGame -> Card -> Maybe DominionBuy
buyN n g c = buyNIf g n c (\_ _ -> True)

-- | Buy up to N of the card as long as it satisfies the passed in function.
buyNIf :: DominionAIGame -> Int -> Card -> (DominionAIGame -> Card -> Bool) -> Maybe DominionBuy
buyNIf g n c f =
  if f g c
    then buyIf g c (\g' c' -> countCards g' c' < n)
    else Nothing

-- | Buy N of the card as long as the player's total deck size is D.
buyNAfterTotalDeckOf :: DominionAIGame -> Int -> Int -> Card -> Maybe DominionBuy
buyNAfterTotalDeckOf g n d c = buyNIf g n c (\g' _ -> countDeck g' >= d)

isDeckBelowN :: DominionAIGame -> Card -> Int -> Bool
isDeckBelowN g c n = n > cardsLeft g c

buyIfNumberOfCardIsBelow :: Card -> Int -> DominionAIGame -> Card -> Maybe DominionBuy
buyIfNumberOfCardIsBelow cd n g c =
  if isDeckBelowN g cd n
    then alwaysBuy g c
    else Nothing

actionTerminators :: DominionAIGame -> Int
actionTerminators g = sum $ map (\at -> fromMaybe 0 (Map.lookup at (g ^. #cards))) actionTerminatorCards

buyIfLowerThanTerminalActions :: DominionAIGame -> Card -> Maybe DominionBuy
buyIfLowerThanTerminalActions g c =
  if countCards g c < actionTerminators g
    then alwaysBuy g c
    else Nothing

sortByWeight :: (Card -> Int) -> [Card] -> [Card]
sortByWeight weights = sortBy (cardWeightCompare weights)

cardWeightCompare :: (Card -> Int) -> Card -> Card -> Ordering
cardWeightCompare cardWeight c1 c2 = cardWeight c2 `compare` cardWeight c1

-- | Given a list of cards in descending priorty order to gain and a max price,
--  gain the first card in the list that's available that is under the max
--  price.
--  TODO: same structure as buying cards (Card,Card->Player->State Game Bool)
gainWhichCard :: [Card] -> Int -> Maybe Card
gainWhichCard cards highestPrice = find (\c -> (c ^. #cost) <= highestPrice) cards
