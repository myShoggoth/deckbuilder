module DeckBuilding.Dominion.Strategies.Basic
    ( bigMoneyStrategy
    , bigSmithyStrategy
    ) where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Utils
import DeckBuilding.Dominion.Cards

import Data.List (delete, intersect, find, (\\))
import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map
import Data.Foldable (foldrM)

-- Strategies

-- Big money

bigMoneyStrategy = Strategy "Big Money" bigMoneyBuy bigMoneyDiscard bigMoneyTrash bigMoneyRetrieve bigMoneyOrderHand bigMoneyGain bigMoneyThroneRoom bigMoneyLibrary bigMoneySentry

canAfford :: Card -> Player -> Bool
canAfford c p = (c ^. cost) <= (p ^. money)

cardsLeft :: Game -> Card -> Bool
cardsLeft gs c = Map.member c (gs ^. decks) && ((gs ^. decks) Map.! c > 0)

alwaysBuy :: Card -> Player -> State Game Bool
alwaysBuy c p = do
  gs <- get
  if canAfford c p && cardsLeft gs c
    then do
      buyCard (Just c) p
      return True
    else return False

countCards :: Card -> Player -> Int
countCards c p = length $ filter (== c) $ (p ^. hand) ++ (p ^. deck) ++ (p ^. discard) ++ (p ^. played)

buyN :: Int -> Card -> Player -> State Game Bool
buyN n c p = if countCards c p < n
    then alwaysBuy c p
    else return False

bigMoneyBuy :: Player -> State Game Player
bigMoneyBuy p = doBuys p (p ^. buys) bigMoneyCards
  where bigMoneyCards = [(colonyCard, alwaysBuy), (platinumCard, alwaysBuy), (provinceCard, alwaysBuy), (goldCard, alwaysBuy), (silverCard, alwaysBuy)]

bigMoneyDiscard :: (Int, Int) -> Player -> State Game Player
bigMoneyDiscard rng = doDiscard rng discardCards
  where discardCards = victoryCards ++ [copperCard]

bigMoneyTrash :: (Int, Int) -> Player -> State Game Player
bigMoneyTrash rng = doTrash rng trashCards
  where trashCards = [curseCard, estateCard, copperCard]

bigMoneyRetrieve :: (Int, Int) -> Player -> State Game Player
bigMoneyRetrieve rng = doRetrieveDiscard rng retrieveCards
  where retrieveCards = [platinumCard, goldCard, marketCard, festivalCard, villageCard, laboratoryCard, smithyCard, moatCard, silverCard]

bigMoneyGain :: Int -> Player -> State Game Player
bigMoneyGain = gainCard gainCards
  where gainCards = [colonyCard, platinumCard, provinceCard, goldCard, duchyCard, silverCard]

bigMoneyOrderHand :: Player -> State Game Player
bigMoneyOrderHand = return

bigMoneyThroneRoom :: Player -> State Game (Maybe Card)
bigMoneyThroneRoom p = return Nothing

bigMoneyLibrary :: Card -> State Game Bool
bigMoneyLibrary _ = return True

bigMoneySentry :: [Card] -> Player -> State Game ([Card], [Card], [Card])
bigMoneySentry cs p = do
  let trash = cs `intersect` trashCards
  let keep = trash \\ cs
  return (trash, [], keep)
  where trashCards = [curseCard, estateCard, copperCard]

-- Big smithy

bigSmithyStrategy = Strategy "Big Smithy" bigSmithyBuy bigMoneyDiscard bigMoneyTrash bigMoneyRetrieve bigMoneyOrderHand bigSmithyGain bigSmithyThroneRoom bigMoneyLibrary bigMoneySentry

bigSmithyBuy :: Player -> State Game Player
bigSmithyBuy p = doBuys p (p ^. buys) bigMoneyCards
  where bigMoneyCards = [(colonyCard, alwaysBuy), (platinumCard, alwaysBuy), (provinceCard, alwaysBuy), (smithyCard, buyN 2), (goldCard, alwaysBuy), (silverCard, alwaysBuy)]

bigSmithyGain :: Int -> Player -> State Game Player
bigSmithyGain = gainCard gainCards
  where gainCards = [colonyCard, platinumCard, provinceCard, goldCard, smithyCard, silverCard, duchyCard]

bigSmithyThroneRoom :: Player -> State Game (Maybe Card)
bigSmithyThroneRoom = findFirstCard throneRoomCards
  where throneRoomCards = [smithyCard]

-- Strategy helpers

prefCards :: Int -> [Card] -> [Card] -> [Card]
prefCards max cs h= take max $ intersect h cs

prefPlusCards :: (Int, Int) -> [Card] -> [Card] -> [Card]
prefPlusCards (min, max) cs h
    | length pref > min = pref
    | otherwise         = take min $ pref ++ cs
  where pref = prefCards max cs h

removeFromCards :: [Card] -> [Card] -> [Card]
removeFromCards = foldr delete

doDiscard :: (Int, Int) -> [Card] -> Player -> State Game Player
doDiscard minmax cards p = updatePlayer (over discard (++ toDiscard) (set hand newHand p))
  where toDiscard = prefPlusCards minmax cards (p ^. hand)
        newHand = removeFromCards (p ^. hand) toDiscard

doTrash :: (Int, Int) -> [Card] -> Player -> State Game Player
doTrash minmax cards p = updatePlayer (set hand newHand p)
  where toTrash = prefPlusCards minmax cards (p ^. hand)
        newHand = removeFromCards (p ^. hand) toTrash

doRetrieveDiscard :: (Int, Int) -> [Card] -> Player -> State Game Player
doRetrieveDiscard (min, max) cards p = updatePlayer (over deck (toRetrieve ++) (set discard newDiscard p))
  where pref = take max $ intersect (p ^. discard ) cards
        toRetrieve
          | length pref > min = pref
          | otherwise         = take min $ pref ++ (p ^. discard)
        newDiscard = foldr delete (p  ^. discard) toRetrieve

findFirstCard :: [Card] -> Player -> State Game (Maybe Card)
findFirstCard cards p = return $ getFirst pref
  where pref = (p ^. hand) `intersect` cards
        getFirst []     = Nothing
        getFirst (x:xs) = Just x

gainCard :: [Card] -> Int -> Player -> State Game Player
gainCard cards highestPrice p = do
  gs <- get
  let nonEmptyDecks = filter (\c -> Map.member c (gs ^. decks) && (gs ^. decks) Map.! c > 0) cards
  let highestCostCard = find (\c -> (c ^. cost) <= highestPrice) cards
  p' <- obtain highestCostCard
  updatePlayer p'
  return p'
  where obtain :: Maybe Card -> State Game Player
        obtain Nothing  = return p
        obtain (Just c) = do
          gs <- get
          put $ over decks (Map.mapWithKey (decreaseCards c)) gs
          let p'' = over discard (c:) p
          return p''

doBuy :: Int -> Int -> [Card] -> [Maybe Card]
doBuy 0 _ _ = []
doBuy n 0 _ = []
doBuy n m cs = findHighCostCard : doBuy (n - 1) (m - mcost findHighCostCard) cs
  where findHighCostCard = find (\c -> (c ^. cost) <= m) cs
        mcost (Just c)   = c ^. cost
        mcost Nothing    = 0

buyCard ::  Maybe Card -> Player -> State Game Player
buyCard Nothing  p = return p
buyCard (Just c) p = do
  gs <- get
  put $ over decks (Map.mapWithKey (decreaseCards c)) gs
  let p' = over discard (c:) $ over buys (+ (-1)) $ over money (\m -> m - (c ^. cost)) p
  updatePlayer p'
  return p'

doBuys' :: Player -> [(Card, Card -> Player -> State Game Bool)] -> State Game Bool
doBuys' p [] = return False
doBuys' p ( (c, a):xs) = do
  bought <- a c p
  if bought
    then return True
    else doBuys' p xs

doBuys :: Player -> Int -> [(Card, Card -> Player -> State Game Bool)] -> State Game Player
doBuys p 0 _      = return p
doBuys p b cards = do
  bought <- doBuys' p cards
  gs <- get
  let Just p' = find (== p) (gs ^. players)
  if bought
    then doBuys p' (b - 1) cards
    else return p'
