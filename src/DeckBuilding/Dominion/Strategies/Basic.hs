module DeckBuilding.Dominion.Strategies.Basic
    ( bigMoneyStrategy
    , bigSmithyStrategy
    , bigMoneyBuy
    , bigMoneyDiscard
    , bigMoneyTrash
    , bigMoneyRetrieve
    , bigMoneyOrderHand
    , bigMoneyGain
    , bigMoneyThroneRoom
    , bigMoneyLibrary
    , bigMoneySentry
    , bigMoneyHandToDeck
    , bigMoneyLurker
    , bigSmithyBuy
    , bigSmithyGain
    , bigSmithyThroneRoom
    , villageSmithyEngine4
    ) where

import           DeckBuilding.Dominion.Cards
import           DeckBuilding.Dominion.Cards.Utils
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils
import           DeckBuilding.Dominion.Strategies.Utils

import           Control.Lens
import           Control.Monad.State
import           Data.Foldable                     (foldrM)
import           Data.List                         (delete, find, intersect,
                                                    (\\))
import qualified Data.Map                          as Map

-- import Debug.Trace

-- Strategies

-- Big money

-- | The most basic Dominion strategy: buy the best money you can afford
--  and provinces.
bigMoneyStrategy = Strategy "Big Money"
                            bigMoneyBuy
                            bigMoneyDiscard
                            bigMoneyTrash
                            bigMoneyRetrieve
                            bigMoneyOrderHand
                            bigMoneyGain
                            bigMoneyThroneRoom
                            bigMoneyLibrary
                            bigMoneySentry
                            bigMoneyHandToDeck
                            bigMoneyLurker

-- | The most basic Dominion strategy: buy money and then buy provinces.
bigMoneyBuy :: Int -> State DominionGame [Card]
bigMoneyBuy p = do
    (Just player) <- preuse (players . ix p)
    doBuys p (player ^. buys) bigMoneyCards
  where bigMoneyCards = [ (provinceCard, alwaysBuy)
                        , (duchyCard, buyIfNumberOfCardIsBelow provinceCard 4)
                        , (goldCard, alwaysBuy)
                        , (duchyCard, buyIfNumberOfCardIsBelow provinceCard 5)
                        , (estateCard, buyIfNumberOfCardIsBelow provinceCard 2)
                        , (silverCard, alwaysBuy)
                        , (estateCard, buyIfNumberOfCardIsBelow provinceCard 3)
                        ]

-- | If you can discard a card, get rid of victory cards and coppers.
bigMoneyDiscard :: (Int, Int) -> Int -> State DominionGame [Card]
bigMoneyDiscard rng = doDiscard rng discardCards
  where discardCards = victoryCards ++ [copperCard]

-- | If you can trash a card, get rid of curses, estates, and coppers.
--  Note: this logic is dumb and could cause your strategy to not have any
--  money. Write something better, maybe using countCards?
bigMoneyTrash :: (Int, Int) -> Int -> State DominionGame [Card]
bigMoneyTrash rng = doTrash rng trashCards

-- | Such trash.
trashCards = [curseCard, estateCard, copperCard]

-- | If you can retrieve a card from your discard into your hand, get something
--  worth it.
bigMoneyRetrieve :: (Int, Int) -> Int -> State DominionGame [Card]
bigMoneyRetrieve rng = doRetrieveDiscard rng retrieveCards
  where retrieveCards = [ goldCard
                        , marketCard
                        , festivalCard
                        , villageCard
                        , laboratoryCard
                        , smithyCard
                        , moatCard
                        , silverCard
                        ]

-- | When you're given the opportunity to gain a card, the is the list in
--  descending cost order. Would be good to make this better ala buy.
bigMoneyGain :: Int -> Int -> State DominionGame (Maybe Card)
bigMoneyGain = gainCard gainCards
  where gainCards = [ provinceCard
                    , goldCard
                    , duchyCard
                    , silverCard
                    ]

-- | We never have anything, so why bother?
bigMoneyOrderHand :: Int -> State DominionGame [Card]
bigMoneyOrderHand _ = return []

-- | We don't buy throne rooms in big money.
bigMoneyThroneRoom :: Int -> State DominionGame (Maybe Card)
bigMoneyThroneRoom p = return Nothing

-- | We don't buy libraries in big money.
bigMoneyLibrary :: Card -> State DominionGame Bool
bigMoneyLibrary _ = return True

-- | Simple stupid version of this logic, trash any trash cards, discard
--  remaining victory cards, keep the rest in whatever order.
bigMoneySentry :: [Card] -> Int -> State DominionGame ([Card], [Card], [Card])
bigMoneySentry cs p = do
  let trash = cs `intersect` trashCards
  let disc = (trash \\ cs) `intersect` victoryCards
  let keep = (trash ++ disc) \\ cs
  return (trash, disc, keep)

-- | Meh?
bigMoneyHandToDeck :: Int -> Int -> State DominionGame [Card]
bigMoneyHandToDeck n p = do
    (Just player) <- preuse (players . ix p)
    let cards = take n $ (player ^. hand) `intersect` handToDeckCards
    (players . ix p . deck) %= (cards++)
    (players . ix p . hand) .= ((player ^. hand) \\ cards)
    return cards
  where handToDeckCards = [ estateCard
                          , copperCard
                          , smithyCard
                          ]

findInPlayAction :: Map.Map Card Int -> Card
findInPlayAction decks = fst $ Map.elemAt 0 $ Map.filterWithKey (\k v -> (k ^. cardType == Action) && v > 0) decks

-- | Just need something
bigMoneyLurker :: Card -> Int -> State DominionGame (Either Card Card)
bigMoneyLurker c p = do
  decks <- use decks
  return $ Left $ findInPlayAction decks

-- Big smithy

-- | Big money plus buy up to two Smithy cards. Note this one change beats the
--  crap out of big money.
bigSmithyStrategy = Strategy "Big Smithy"
                             bigSmithyBuy
                             bigMoneyDiscard
                             bigMoneyTrash
                             bigMoneyRetrieve
                             bigMoneyOrderHand
                             bigSmithyGain
                             bigSmithyThroneRoom
                             bigMoneyLibrary
                             bigMoneySentry
                             bigMoneyHandToDeck
                             bigMoneyLurker

-- | Just like big money buy also buy up to two smithy cards.
bigSmithyBuy :: Int -> State DominionGame [Card]
bigSmithyBuy p = do
    (Just player) <- preuse (players . ix p)
    doBuys p (player ^. buys) bigSmithyCards
  where bigSmithyCards = [ (provinceCard, alwaysBuy)
                        , (smithyCard, buyN 2)
                        , (goldCard, alwaysBuy)
                        , (silverCard, alwaysBuy)
                        ]

-- | Just like big money buy we also gain smithy cards.
bigSmithyGain :: Int -> Int -> State DominionGame (Maybe Card)
bigSmithyGain = gainCard gainCards
  where gainCards = [ provinceCard
                    , goldCard
                    , smithyCard
                    , silverCard
                    , duchyCard
                    ]

-- | If we somehow had a throne room, definitely double the smithy.
bigSmithyThroneRoom :: Int -> State DominionGame (Maybe Card)
bigSmithyThroneRoom = findFirstCard throneRoomCards
  where throneRoomCards = [smithyCard]


-- Village/Smithy engine #4 from https://dominionstrategy.com/2012/07/30/building-the-first-game-engine/

villageSmithyEngine4 = Strategy "Village/Smithy Engine 4"
                                villageSmithyEngine4Buy
                                bigMoneyDiscard
                                bigMoneyTrash
                                bigMoneyRetrieve
                                bigMoneyOrderHand
                                bigSmithyGain
                                bigSmithyThroneRoom
                                bigMoneyLibrary
                                bigMoneySentry
                                bigMoneyHandToDeck
                                bigMoneyLurker

-- | The buy strategy
villageSmithyEngine4Buy :: Int -> State DominionGame [Card]
villageSmithyEngine4Buy p = do
    (Just player) <- preuse (players . ix p)
    doBuys p (player ^. buys) bigVillageSmithyEngine4Cards
  where bigVillageSmithyEngine4Cards =  [
                                          (provinceCard, alwaysBuy)
                                        , (duchyCard, buyIfNumberOfCardIsBelow provinceCard 3)
                                        , (estateCard, buyIfNumberOfCardIsBelow provinceCard 2)
                                        , (goldCard, buyN 2)
                                        , (marketCard, buyN 5)
                                        , (remodelCard, buyN 1)
                                        , (militiaCard, buyN 1)
                                        , (villageCard, buyIfLowerThanTerminalActions)
                                        , (smithyCard, alwaysBuy)
                                        , (villageCard, alwaysBuy)
                                        , (cellarCard, buyN 2)
                                        ]


-- Strategy helpers

-- | Take the list of preferred cards and figure out which ones are in the hand.
--  Take up to the max.
prefCards :: Int -> [Card] -> [Card] -> [Card]
prefCards max cs h= take max $ intersect h cs

-- | Given a (min, max), take up to max of the preferred cards and fill out
--  with whatever is left. Order those cards appropriately.
prefPlusCards :: (Int, Int) -> [Card] -> [Card] -> [Card]
prefPlusCards (min, max) cs h
    | length pref > min = pref
    | otherwise         = take min $ pref ++ cs
  where pref = prefCards max cs h

-- | Remove this list of cards from that list of cards.
removeFromCards :: [Card] -> [Card] -> [Card]
removeFromCards = foldr delete

-- | Core for a simple discarding logic. (min, max) and the list of
--  preferred cards to discard.
doDiscard :: (Int, Int) -> [Card] -> Int -> State DominionGame [Card]
doDiscard minmax cards p = do
  (Just player) <- preuse (players . ix p)
  let toDiscard = prefPlusCards minmax cards (player ^. hand)
  let newHand = removeFromCards (player ^. hand) toDiscard
  (players . ix p . discard) %= (++ toDiscard)
  (players . ix p . hand) .= newHand
  return toDiscard

-- | Core for a simple trashing logic. (min, max) and the list of
--  preferred cards to trash.
doTrash :: (Int, Int) -> [Card] -> Int -> State DominionGame [Card]
doTrash minmax cards p = do
  (Just player) <- preuse (players . ix p)
  let toTrash = prefPlusCards minmax cards (player ^. hand)
  let newHand = removeFromCards (player ^. hand) toTrash
  trash %= (toTrash ++)
  (players . ix p . hand) .= newHand
  return toTrash

-- | Core for a simple card retrieving from the discard pile logic. (min, max)
--  and the list of preferred cards to retrieve.
doRetrieveDiscard :: (Int, Int) -> [Card] -> Int -> State DominionGame [Card]
doRetrieveDiscard (min, max) cards p = do
  (Just player) <- preuse (players . ix p)
  let pref = take max $ intersect (player ^. discard) cards
  let toRetrieve
        | length pref > min = pref
        | otherwise         = take min $ pref ++ (player ^. discard)
  let newDiscard = foldr delete (player ^. discard) toRetrieve
  (players . ix p . deck) %= (toRetrieve++)
  (players . ix p . discard) .= newDiscard
  return toRetrieve

-- | Find the first card in the list that the player has in its hand, if any.
findFirstCard :: [Card] -> Int -> State DominionGame (Maybe Card)
findFirstCard cards p = do
  (Just player) <- preuse (players . ix p)
  return $ case (player ^. hand) `intersect` cards of
    [] -> Nothing
    (x:xs) -> Just x

-- | Given a list of cards and buy functions, call the buy functions until one
--  is bought and return True. If none are bought, return False.
doBuys' :: Int -> [(Card, Card -> Int -> State DominionGame (Maybe Card))] -> State DominionGame [Card]
doBuys' p [] = return []
doBuys' p ( (c, a):xs) = do
  bought <- a c p
  case bought of
    Nothing  -> doBuys' p xs
    (Just c) -> return [c]


-- | Given a player, a number of buys, and a list of preferred cards to buy
--  and a buy function, buy as many as possible given the number of buys and
--  the amount of money the player has.
doBuys :: Int -> Int -> [(Card, Card -> Int -> State DominionGame (Maybe Card))] -> State DominionGame [Card]
-- doBuys p b cs | trace ("doBuys: " ++ show (p ^. playerName) ++ " (" ++ show b ++ ")") False = undefined
doBuys p 0 _      = return []
doBuys p b cards  = do
  bought <- doBuys' p cards
  case bought of
    []  -> return []
    cs  -> do
      more <- doBuys p (b - 1) cards
      return $ cs ++ more
