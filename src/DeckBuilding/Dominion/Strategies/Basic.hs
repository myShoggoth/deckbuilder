{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}

module DeckBuilding.Dominion.Strategies.Basic
    ( bigMoneyStrategy
    , bigSmithyStrategy
    , bigMoneyBuy
    , bigMoneyDiscard
    , bigMoneyTrash
    , bigMoneyRetrieve
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
    , nextCardByWeight
    , bigMoneyCardWeight
    ) where

import           Control.Lens
import           Data.Generics.Product
import           Data.List                              (delete, intersect,
                                                         (\\))
import qualified Data.Map                               as Map
import           Safe (headMay)
import           DeckBuilding.Dominion.Cards
import           DeckBuilding.Dominion.Cards.Utils
import           DeckBuilding.Dominion.Strategies.Utils
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils

-- Strategies

-- Big money

-- | The most basic Dominion strategy: buy the best money you can afford
--  and provinces.
bigMoneyStrategy :: Strategy
bigMoneyStrategy = Strategy "Big Money"
                            bigMoneyBuy
                            bigMoneyDiscard
                            bigMoneyTrash
                            bigMoneyRetrieve
                            (nextCardByWeight bigMoneyCardWeight)
                            bigMoneyGain
                            bigMoneyThroneRoom
                            bigMoneyLibrary
                            bigMoneySentry
                            bigMoneyHandToDeck
                            bigMoneyLurker

-- | The most basic Dominion strategy: buy money and then buy provinces.
bigMoneyBuy :: Int -> DominionState Int
bigMoneyBuy p = do
    thePlayer <- findPlayer p
    doBuys p (thePlayer ^. field @"buys") bigMoneyCards
  where bigMoneyCards = [ (provinceCard, alwaysBuy)
                        , (duchyCard, buyIfNumberOfCardIsBelow provinceCard 4)
                        , (goldCard, alwaysBuy)
                        , (duchyCard, buyIfNumberOfCardIsBelow provinceCard 5)
                        , (estateCard, buyIfNumberOfCardIsBelow provinceCard 2)
                        , (silverCard, alwaysBuy)
                        , (estateCard, buyIfNumberOfCardIsBelow provinceCard 3)
                        ]

-- | If you can discard a card, get rid of victory cards and coppers.
bigMoneyDiscard :: (Int, Int) -> Int -> DominionState [Card]
bigMoneyDiscard rng = doDiscard rng discardCards
  where discardCards = victoryCards ++ [copperCard]

-- | If you can trash a card, get rid of curses, estates, and coppers.
--  Note: this logic is dumb and could cause your strategy to not have any
--  money. Write something better, maybe using countCards?
bigMoneyTrash :: (Int, Int) -> Int -> DominionState [Card]
bigMoneyTrash rng = doTrash rng trashCards

-- | Such trash.
trashCards :: [Card]
trashCards = [curseCard, estateCard, copperCard]

-- | If you can retrieve a card from your discard into your hand, get something
--  worth it.
bigMoneyRetrieve :: (Int, Int) -> Int -> DominionState [Card]
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
bigMoneyGain :: Int -> Int -> DominionState (Maybe Card)
bigMoneyGain = gainCard gainCards
  where gainCards = [ provinceCard
                    , goldCard
                    , duchyCard
                    , silverCard
                    ]

bigMoneyCardWeight :: Card -> Int
bigMoneyCardWeight _ = 1

-- | We don't buy throne rooms in big money.
bigMoneyThroneRoom :: Int -> DominionState (Maybe Card)
bigMoneyThroneRoom _ = return Nothing

-- | We don't buy libraries in big money.
bigMoneyLibrary :: Card -> DominionState Bool
bigMoneyLibrary _ = return True

-- | Simple stupid version of this logic, trash any trash cards, discard
--  remaining victory cards, keep the rest in whatever order.
bigMoneySentry :: [Card] -> Int -> DominionState ([Card], [Card], [Card])
bigMoneySentry cs _ = do
  let trash' = cs `intersect` trashCards
  let disc = (trash' \\ cs) `intersect` victoryCards
  let keep = (trash' ++ disc) \\ cs
  return (trash', disc, keep)

-- | Meh?
bigMoneyHandToDeck :: Int -> Int -> DominionState [Card]
bigMoneyHandToDeck n p = do
    thePlayer <- findPlayer p
    let cards = take n $ (thePlayer ^. field @"hand") `intersect` handToDeckCards
    (field @"players" . ix p . field @"deck") %= (cards++)
    (field @"players" . ix p . field @"hand") .= ((thePlayer ^. field @"hand") \\ cards)
    return cards
  where handToDeckCards = [ estateCard
                          , copperCard
                          , smithyCard
                          ]

findInPlayAction :: Map.Map Card Int -> Card
findInPlayAction decks' = fst $ Map.elemAt 0 $ Map.filterWithKey (\k v -> (k ^. field @"cardType" == Action) && v > 0) decks'

-- | Just need something
bigMoneyLurker :: Card -> Int -> DominionState (Either Card Card)
bigMoneyLurker _ _ = do
  decks' <- use $ field @"decks"
  return $ Left $ findInPlayAction decks'

-- Big smithy

-- | Big money plus buy up to two Smithy cards. Note this one change beats the
--  crap out of big money.
bigSmithyStrategy :: Strategy
bigSmithyStrategy = Strategy "Big Smithy"
                             bigSmithyBuy
                             bigMoneyDiscard
                             bigMoneyTrash
                             bigMoneyRetrieve
                             (nextCardByWeight bigSmithyCardWeight)
                             bigSmithyGain
                             bigSmithyThroneRoom
                             bigMoneyLibrary
                             bigMoneySentry
                             bigMoneyHandToDeck
                             bigMoneyLurker

-- | Just like big money buy also buy up to two smithy cards.
bigSmithyBuy :: Int -> DominionState Int
bigSmithyBuy p = do
    thePlayer <- findPlayer p
    doBuys p (thePlayer ^. field @"buys") bigSmithyCards
  where bigSmithyCards = [ (provinceCard, alwaysBuy)
                        , (duchyCard, buyIfNumberOfCardIsBelow provinceCard 4)
                        , (smithyCard, buyN 2)
                        , (goldCard, alwaysBuy)
                        , (duchyCard, buyIfNumberOfCardIsBelow provinceCard 5)
                        , (estateCard, buyIfNumberOfCardIsBelow provinceCard 2)
                        , (silverCard, alwaysBuy)
                        , (estateCard, buyIfNumberOfCardIsBelow provinceCard 3)
                        ]

nextCardByWeight :: (Card -> Int) -> Int -> DominionState (Maybe Card)
nextCardByWeight weights p = do
  thePlayer <- findPlayer p
  return $ headMay $ sortByWeight weights $ thePlayer ^. field @"hand"

bigSmithyCardWeight :: Card -> Int
bigSmithyCardWeight (Card "Throne Room" _ _ _ _) = 11 -- This is for the Throne Room test
bigSmithyCardWeight (Card "Smithy" _ _ _ _)      = 10
bigSmithyCardWeight _                            = 1

-- | Just like big money buy we also gain smithy cards.
bigSmithyGain :: Int -> Int -> DominionState (Maybe Card)
bigSmithyGain = gainCard gainCards
  where gainCards = [ provinceCard
                    , goldCard
                    , smithyCard
                    , silverCard
                    , duchyCard
                    ]

-- | If we somehow had a throne room, definitely double the smithy.
bigSmithyThroneRoom :: Int -> DominionState (Maybe Card)
bigSmithyThroneRoom = findFirstCard throneRoomCards
  where throneRoomCards = [smithyCard]


-- Village/Smithy engine #4 from https://dominionstrategy.com/2012/07/30/building-the-first-game-engine/

villageSmithyEngine4 :: Strategy
villageSmithyEngine4 = Strategy "Village/Smithy Engine 4"
                                villageSmithyEngine4Buy
                                bigMoneyDiscard
                                bigMoneyTrash
                                bigMoneyRetrieve
                                (nextCardByWeight villageSmithyEngine4CardWeight)
                                bigSmithyGain
                                bigSmithyThroneRoom
                                bigMoneyLibrary
                                bigMoneySentry
                                bigMoneyHandToDeck
                                bigMoneyLurker

-- | The buy strategy
villageSmithyEngine4Buy :: Int -> DominionState Int
villageSmithyEngine4Buy p = do
    thePlayer <- findPlayer p
    doBuys p (thePlayer ^. field @"buys") bigVillageSmithyEngine4Cards
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
                                        , (silverCard, alwaysBuy)
                                        , (cellarCard, buyN 2)
                                        ]


villageSmithyEngine4CardWeight :: Card -> Int
villageSmithyEngine4CardWeight (Card "Village" _ _ _ _) = 10
villageSmithyEngine4CardWeight (Card "Market" _ _ _ _)  = 9
villageSmithyEngine4CardWeight (Card "Militia" _ _ _ _) = 8
villageSmithyEngine4CardWeight (Card "Remodel" _ _ _ _) = 7
villageSmithyEngine4CardWeight (Card "Smithy" _ _ _ _)  = 6
villageSmithyEngine4CardWeight (Card "Cellar" _ _ _ _)  = 5
villageSmithyEngine4CardWeight _                        = 1

-- Strategy helpers

-- | Take the list of preferred cards and figure out which ones are in the hand.
--  Take up to the max.
prefCards :: Int -> [Card] -> [Card] -> [Card]
prefCards max' cs h = take max' $ intersect h cs

-- | Given a (min, max), take up to max of the preferred cards and fill out
--  with whatever is left. Order those cards appropriately.
prefPlusCards :: (Int, Int) -> [Card] -> [Card] -> [Card]
prefPlusCards (min', max') cs h
    | length pref > min' = pref
    | otherwise         = take min' $ pref ++ cs
  where pref = prefCards max' cs h

-- | Core for a simple discarding logic. (min, max) and the list of
--  preferred cards to discard.
doDiscard :: (Int, Int) -> [Card] -> Int -> DominionState [Card]
doDiscard minmax cards p = do
  thePlayer <- findPlayer p
  let toDiscard = prefPlusCards minmax cards (thePlayer ^. field @"hand")
  let newHand = removeFromCards (thePlayer ^. field @"hand") toDiscard
  (field @"players" . ix p . field @"discard") %= (++ toDiscard)
  (field @"players" . ix p . field @"hand") .= newHand
  return toDiscard

-- | Core for a simple trashing logic. (min, max) and the list of
--  preferred cards to trash.
doTrash :: (Int, Int) -> [Card] -> Int -> DominionState [Card]
doTrash minmax cards p = do
  thePlayer <- findPlayer p
  let toTrash = prefPlusCards minmax cards (thePlayer ^. field @"hand")
  let newHand = removeFromCards (thePlayer ^. field @"hand") toTrash
  field @"trash" %= (toTrash ++)
  (field @"players" . ix p . field @"hand") .= newHand
  return toTrash

-- | Core for a simple card retrieving from the discard pile logic. (min, max)
--  and the list of preferred cards to retrieve.
doRetrieveDiscard :: (Int, Int) -> [Card] -> Int -> DominionState [Card]
doRetrieveDiscard (min', max') cards p = do
  thePlayer <- findPlayer p
  let pref = take max' $ intersect (thePlayer ^. field @"discard") cards
  let toRetrieve
        | length pref > min' = pref
        | otherwise         = take min' $ pref ++ (thePlayer ^. field @"discard")
  let newDiscard = foldr delete (thePlayer ^. field @"discard") toRetrieve
  (field @"players" . ix p . field @"deck") %= (toRetrieve++)
  (field @"players" . ix p . field @"discard") .= newDiscard
  return toRetrieve

-- | Find the first card in the list that the player has in its hand, if any.
findFirstCard :: [Card] -> Int -> DominionState (Maybe Card)
findFirstCard cards p = do
  thePlayer <- findPlayer p
  return $ case (thePlayer ^. field @"hand") `intersect` cards of
    []    -> Nothing
    (x:_) -> Just x

-- | Given a list of cards and buy functions, call the buy functions until one
--  is bought and return True. If none are bought, return False.
doBuys' :: Int -> [(Card, Card -> Int -> DominionState (Maybe Card))] -> DominionState [Card]
doBuys' _ [] = return []
doBuys' p ( (c, a):xs) = do
  bought <- a c p
  case bought of
    Nothing   -> doBuys' p xs
    (Just c') -> return [c']


-- | Given a player, a number of buys, and a list of preferred cards to buy
--  and a buy function, buy as many as possible given the number of buys and
--  the amount of money the player has.
doBuys :: Int -> Int -> [(Card, Card -> Int -> DominionState (Maybe Card))] -> DominionState Int
-- doBuys p b cs | trace ("doBuys: " ++ show (p ^. playerName) ++ " (" ++ show b ++ ")") False = undefined
doBuys p 0 _      = return p
doBuys p b cards  = do
  bought <- doBuys' p cards
  case bought of
    []  -> return p
    _  -> do
      _ <- doBuys p (b - 1) cards
      return p
