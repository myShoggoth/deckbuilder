module DeckBuilding.Dominion.Strategies.Basic
    ( bigMoneyStrategy
    ) where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Utils
import DeckBuilding.Dominion.Cards

import Data.List (delete, intersect, find)
import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map
import Data.Foldable (foldrM)

-- Strategies

-- Big money

bigMoneyStrategy = Strategy "Big Money" bigMoneyBuy bigMoneyDiscard bigMoneyTrash bigMoneyRetrieve bigMoneyOrderHand bigMoneyGain

bigMoneyBuy :: Player -> State Game Player
bigMoneyBuy p = doBuys p bigMoneyCards
  where bigMoneyCards = [provinceCard, goldCard, silverCard]

bigMoneyDiscard :: (Int, Int) -> Player -> State Game Player
bigMoneyDiscard rng = doDiscard rng discardCards
  where discardCards = victoryCards ++ [copperCard]

bigMoneyTrash :: (Int, Int) -> Player -> State Game Player
bigMoneyTrash rng = doTrash rng trashCards
  where trashCards = [curseCard, estateCard, copperCard]

bigMoneyRetrieve :: (Int, Int) -> Player -> State Game Player
bigMoneyRetrieve rng = doRetrieveDiscard rng retrieveCards
  where retrieveCards = [goldCard, marketCard, festivalCard, villageCard, laboratoryCard, smithyCard, moatCard, silverCard]

bigMoneyGain :: Int -> Player -> State Game Player
bigMoneyGain cost p = gainCard gainCards cost p
  where gainCards = [provinceCard, goldCard, silverCard, duchyCard]

bigMoneyOrderHand :: Player -> State Game Player
bigMoneyOrderHand p = return p

-- Strategy helpers

doDiscard :: (Int, Int) -> [Card] -> Player -> State Game Player
doDiscard (min, max) cards p = updatePlayer (over discard (++ toDiscard) (set hand newHand p))
  where pref = take max $ intersect (p ^. hand) cards
        toDiscard
          | length pref > min = pref
          | otherwise         = take min $ pref ++ (p ^. hand)
        newHand = foldr (\c acc -> delete c acc) (p ^. hand) toDiscard

doTrash :: (Int, Int) -> [Card] -> Player -> State Game Player
doTrash (min, max) cards p = updatePlayer (set hand newHand p)
  where pref = take max $ intersect (p ^. hand) cards
        toDiscard
          | length pref > min = pref
          | otherwise         = take min $ pref ++ (p ^. hand)
        newHand = foldr (\c acc -> delete c acc) (p ^. hand) toDiscard

doRetrieveDiscard :: (Int, Int) -> [Card] -> Player -> State Game Player
doRetrieveDiscard (min, max) cards p = updatePlayer (over deck (toRetrieve ++) (set discard newDiscard p))
  where pref = take max $ intersect (p ^. discard ) cards
        toRetrieve
          | length pref > min = pref
          | otherwise         = take min $ pref ++ (p ^. discard)
        newDiscard = foldr (\c acc -> delete c acc) (p  ^. discard) toRetrieve

gainCard :: [Card] -> Int -> Player -> State Game Player
gainCard cards highestPrice p = do
  gs <- get
  let nonEmptyDecks = filter (\c -> (Map.member c (gs ^. decks)) && (gs ^. decks) Map.! c > 0) cards
  let highestCostCard = find (\c -> (c ^. cost) <= highestPrice) cards
  let p' = obtain p highestCostCard
  updatePlayer p'
  return p'
  where obtain pl Nothing  = pl
        obtain pl (Just c) = over discard (c:) pl

doBuy :: Int -> Int -> [Card] -> [Maybe Card]
doBuy 0 _ _ = []
doBuy n 0 _ = []
doBuy n m cs = findHighCostCard : doBuy (n - 1) (m - (mcost findHighCostCard)) cs
  where findHighCostCard = find (\c -> (c ^. cost) <= m) cs
        mcost (Just c)   = (c ^. cost)
        mcost Nothing    = 0

decreaseCards :: Card -> Card -> Int -> Int
decreaseCards  _  _ 0 = 0
decreaseCards c1 c2 n = if (c1 == c2)
                          then n - 1
                          else n

buyCard ::  Maybe Card -> Player -> State Game Player
buyCard Nothing  p = return p
buyCard (Just c) p = do
  gs <- get
  put $ over decks (Map.mapWithKey (decreaseCards c)) gs
  let p' = over discard (c:) $ over buys (+ (-1)) $ over money (\m -> m - (c ^. cost)) $ p
  updatePlayer $ p'
  return p'

doBuys :: Player -> [Card] -> State Game Player
doBuys p cards = do
  gs <- get
  let nonEmptyDecks = filter (\c -> (Map.member c (gs ^. decks)) && (gs ^. decks) Map.! c > 0) cards
  foldrM (\mc player -> buyCard mc player) p (doBuy (p ^. buys) (p ^. money ) nonEmptyDecks)
