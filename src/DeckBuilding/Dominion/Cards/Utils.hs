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
valueCard :: Int -> Int -> Card -> Player -> State Game Player
valueCard m v c p = return $ over hand (delete c) $ over played (c:) $ over money (+m) $ over victory (+v) p

-- | For basic card values: draw cards, +actions, +buys, +money, +victory
basicCardAction :: Int -> Int -> Int -> Int -> Int -> Card -> Player -> State Game Player
basicCardAction draw a b m v c p = do
  let p' = over hand (delete c) $ over played (c:) $ over actions (+a) $ over buys (+b) $ over money (+m) $ over victory (+v) p
  deal draw p'

-- | Given a list of cards in descending priorty order to gain and a max price,
--  gain the first card in the list that's available that is under the max
--  price.
--  TODO: same structure as buying cards (Card,Card->Player->State Game Bool)
gainCard :: [Card] -> Int -> Player -> State Game Player
gainCard cards highestPrice p = do
  gs <- get
  let nonEmptyDecks = filter (\c -> Map.member c (gs ^. decks) && (gs ^. decks) Map.! c > 0) cards
  let highestCostCard = find (\c -> (c ^. cost) < highestPrice) cards
  obtain highestCostCard
  where obtain :: Maybe Card -> State Game Player
        obtain Nothing  = return p
        obtain (Just c) = do
          gs <- get
          put $ over decks (Map.mapWithKey (decreaseCards c)) gs
          return $ over discard (c:) p
