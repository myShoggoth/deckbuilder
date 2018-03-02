module DeckBuilding.Dominion.Cards.Utils
    ( valueCard
    , basicCardAction
    , gainCard
    ) where

import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils

import           Control.Lens
import           Control.Monad.State
import           Data.List
import qualified Data.Map                    as Map

-- | For value cards, pass money and victory point values.
valueCard :: Int -> Int -> Card -> Int -> State DominionGame Int
valueCard m v c p = do
  (players . ix p . hand) %= (delete c)
  (players . ix p . played) %= (c:)
  (players . ix p . money) += m
  (players . ix p . victory) += v
  return p

-- | For basic card values: draw cards, +actions, +buys, +money, +victory
basicCardAction :: Int -> Int -> Int -> Int -> Int -> Card -> Int -> State DominionGame Int
basicCardAction draw a b m v c p = do
  (players . ix p . actions) += a
  (players . ix p . buys) += b
  deal draw p
  valueCard m v c p


-- | Given a list of cards in descending priorty order to gain and a max price,
--  gain the first card in the list that's available that is under the max
--  price.
--  TODO: same structure as buying cards (Card,Card->Player->State Game Bool)
gainCard :: [Card] -> Int -> Int -> State DominionGame (Maybe Card)
gainCard cards highestPrice p = do
    decks <- use decks
    let nonEmptyDecks = filter (\c -> Map.member c decks && decks Map.! c > 0) cards
    let highestCostCard = find (\c -> (c ^. cost) < highestPrice) cards
    obtain highestCostCard
  where obtain :: Maybe Card -> State DominionGame (Maybe Card)
        obtain Nothing  = return Nothing
        obtain (Just c) = do
          decks %= (Map.mapWithKey (decreaseCards c))
          (players . ix p . deck) %= (c:)
          return $ Just c
