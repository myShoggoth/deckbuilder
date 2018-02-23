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
bigMoneyBuy :: Player -> State Game Player
bigMoneyBuy p = doBuys p (p ^. buys) bigMoneyCards
  where bigMoneyCards = [ (provinceCard, alwaysBuy)
                        , (duchyCard, buyIfNumberOfCardIsBelow provinceCard 4)
                        , (goldCard, alwaysBuy)
                        , (duchyCard, buyIfNumberOfCardIsBelow provinceCard 5)
                        , (estateCard, buyIfNumberOfCardIsBelow provinceCard 2)
                        , (silverCard, alwaysBuy)
                        , (estateCard, buyIfNumberOfCardIsBelow provinceCard 3)
                        ]

-- | If you can discard a card, get rid of victory cards and coppers.
bigMoneyDiscard :: (Int, Int) -> Player -> State Game Player
bigMoneyDiscard rng = doDiscard rng discardCards
  where discardCards = victoryCards ++ [copperCard]

-- | If you can trash a card, get rid of curses, estates, and coppers.
--  Note: this logic is dumb and could cause your strategy to not have any
--  money. Write something better, maybe using countCards?
bigMoneyTrash :: (Int, Int) -> Player -> State Game Player
bigMoneyTrash rng = doTrash rng trashCards

-- | Such trash.
trashCards = [curseCard, estateCard, copperCard]

-- | If you can retrieve a card from your discard into your hand, get something
--  worth it.
bigMoneyRetrieve :: (Int, Int) -> Player -> State Game Player
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
bigMoneyGain :: Int -> Player -> State Game Player
bigMoneyGain = gainCard gainCards
  where gainCards = [ provinceCard
                    , goldCard
                    , duchyCard
                    , silverCard
                    ]

-- | We never have anything, so why bother?
bigMoneyOrderHand :: Player -> State Game Player
bigMoneyOrderHand = return

-- | We don't buy throne rooms in big money.
bigMoneyThroneRoom :: Player -> State Game (Maybe Card)
bigMoneyThroneRoom p = return Nothing

-- | We don't buy libraries in big money.
bigMoneyLibrary :: Card -> State Game Bool
bigMoneyLibrary _ = return True

-- | Simple stupid version of this logic, trash any trash cards, discard
--  remaining victory cards, keep the rest in whatever order.
bigMoneySentry :: [Card] -> Player -> State Game ([Card], [Card], [Card])
bigMoneySentry cs p = do
  let trash = cs `intersect` trashCards
  let disc = (trash \\ cs) `intersect` victoryCards
  let keep = (trash ++ disc) \\ cs
  return (trash, disc, keep)

-- | Meh?
bigMoneyHandToDeck :: Int -> Player -> State Game Player
bigMoneyHandToDeck n p = do
  let cards = take n $ (p ^. hand) `intersect` handToDeckCards
  return $ over deck (cards ++) $ set hand ((p ^. hand) \\ cards) p
  where handToDeckCards = [ estateCard
                          , copperCard
                          , smithyCard
                          ]

findInPlayAction :: Map.Map Card Int -> Card
findInPlayAction decks = fst $ Map.elemAt 0 $ Map.filterWithKey (\k v -> (k ^. cardType == Action) && v > 0) decks

-- | Just need something
bigMoneyLurker :: Card -> Player -> State Game (Either Card Card)
bigMoneyLurker c p = do
  gs <- get
  return $ Left $ findInPlayAction (gs ^. decks)

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
bigSmithyBuy :: Player -> State Game Player
bigSmithyBuy p = doBuys p (p ^. buys) bigMoneyCards
  where bigMoneyCards = [ (provinceCard, alwaysBuy)
                        , (smithyCard, buyN 2)
                        , (goldCard, alwaysBuy)
                        , (silverCard, alwaysBuy)
                        ]

-- | Just like big money buy we also gain smithy cards.
bigSmithyGain :: Int -> Player -> State Game Player
bigSmithyGain = gainCard gainCards
  where gainCards = [ provinceCard
                    , goldCard
                    , smithyCard
                    , silverCard
                    , duchyCard
                    ]

-- | If we somehow had a throne room, definitely double the smithy.
bigSmithyThroneRoom :: Player -> State Game (Maybe Card)
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
villageSmithyEngine4Buy :: Player -> State Game Player
villageSmithyEngine4Buy p = doBuys p (p ^. buys) bigVillageSmithyEngine4Cards
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
doDiscard :: (Int, Int) -> [Card] -> Player -> State Game Player
doDiscard minmax cards p = updatePlayer $ over discard (++ toDiscard) $ set hand newHand p
  where toDiscard = prefPlusCards minmax cards (p ^. hand)
        newHand = removeFromCards (p ^. hand) toDiscard

-- | Core for a simple trashing logic. (min, max) and the list of
--  preferred cards to trash.
doTrash :: (Int, Int) -> [Card] -> Player -> State Game Player
doTrash minmax cards p = do
  gs <- get
  put $ over trash (toTrash ++) gs
  return $ set hand newHand p
  where toTrash = prefPlusCards minmax cards (p ^. hand)
        newHand = removeFromCards (p ^. hand) toTrash

-- | Core for a simple card retrieving from the discard pile logic. (min, max)
--  and the list of preferred cards to retrieve.
doRetrieveDiscard :: (Int, Int) -> [Card] -> Player -> State Game Player
doRetrieveDiscard (min, max) cards p = return $ over deck (toRetrieve ++) $ set discard newDiscard p
  where pref = take max $ intersect (p ^. discard ) cards
        toRetrieve
          | length pref > min = pref
          | otherwise         = take min $ pref ++ (p ^. discard)
        newDiscard = foldr delete (p  ^. discard) toRetrieve

-- | Find the first card in the list that the player has in its hand, if any.
findFirstCard :: [Card] -> Player -> State Game (Maybe Card)
findFirstCard cards p = return $ getFirst pref
  where pref = (p ^. hand) `intersect` cards
        getFirst []     = Nothing
        getFirst (x:xs) = Just x

-- | Given a list of cards and buy functions, call the buy functions until one
--  is bought and return True. If none are bought, return False.
doBuys' :: Player -> [(Card, Card -> Player -> State Game Bool)] -> State Game Bool
doBuys' p [] = return False
doBuys' p ( (c, a):xs) = do
  bought <- a c p
  if bought
    then return True
    else doBuys' p xs

-- | Given a player, a number of buys, and a list of preferred cards to buy
--  and a buy function, buy as many as possible given the number of buys and
--  the amount of money the player has.
doBuys :: Player -> Int -> [(Card, Card -> Player -> State Game Bool)] -> State Game Player
-- doBuys p b cs | trace ("doBuys: " ++ show (p ^. playerName) ++ " (" ++ show b ++ ")") False = undefined
doBuys p 0 _      = return p
doBuys p b cards = do
  bought <- doBuys' p cards
  gs <- get
  let Just p' = find (== p) (gs ^. players)
  if bought
    then doBuys p' (b - 1) cards
    else return p'
