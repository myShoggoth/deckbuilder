{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}

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

import Control.Lens ( (^.), use, (%=), (.=), Ixed(ix) )
import Data.Generics.Product ( HasField(field) )
import Data.List (delete, intersect, (\\))
import qualified Data.Map                               as Map
import Safe (headMay)
import DeckBuilding.Dominion.Cards
    ( goldCard,
      silverCard,
      copperCard,
      provinceCard,
      duchyCard,
      estateCard,
      curseCard,
      victoryCards,
      marketCard,
      moatCard,
      smithyCard,
      villageCard,
      festivalCard,
      laboratoryCard,
      cellarCard,
      militiaCard,
      remodelCard )
import DeckBuilding.Dominion.Cards.Utils ( gainCard )
import DeckBuilding.Dominion.Strategies.Utils
    ( alwaysBuy,
      buyIfLowerThanTerminalActions,
      buyIfNumberOfCardIsBelow,
      buyN,
      sortByWeight )
import DeckBuilding.Types ( PlayerNumber(unPlayerNumber) )
import DeckBuilding.Dominion.Types
    ( Card(Card)
    , CardType(Action)
    , DominionState
    , Strategy(Strategy)
    , DominionAIGame(..)
    , DominionMove)
import DeckBuilding.Dominion.Utils ( findPlayer, removeFromCards )

import Debug.Trace
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
bigMoneyBuy :: DominionAIGame -> [DominionMove]
bigMoneyBuy g = doBuys g bigMoneyCards
  where bigMoneyCards = [ (provinceCard, alwaysBuy)
                        , (duchyCard, buyIfNumberOfCardIsBelow provinceCard 4)
                        , (goldCard, alwaysBuy)
                        , (duchyCard, buyIfNumberOfCardIsBelow provinceCard 5)
                        , (estateCard, buyIfNumberOfCardIsBelow provinceCard 2)
                        , (silverCard, alwaysBuy)
                        , (estateCard, buyIfNumberOfCardIsBelow provinceCard 3)
                        ]

-- | If you can discard a card, get rid of victory cards and coppers.
bigMoneyDiscard :: DominionAIGame -> (Int, Int) -> [Card]
bigMoneyDiscard g rng = doDiscard g rng discardCards
  where discardCards = victoryCards ++ [copperCard]

-- | If you can trash a card, get rid of curses, estates, and coppers.
--  Note: this logic is dumb and could cause your strategy to not have any
--  money. Write something better, maybe using countCards?
bigMoneyTrash :: DominionAIGame -> (Int, Int) -> [Card] -> [Card]
bigMoneyTrash g rng discards = doTrash g rng trashCards $ (g ^. #hand) ++ discards

-- | Such trash.
trashCards :: [Card]
trashCards = [curseCard, estateCard, copperCard]

-- | If you can retrieve a card from your discard into your hand, get something
--  worth it.
bigMoneyRetrieve :: DominionAIGame -> (Int, Int) -> [Card] -> [Card]
bigMoneyRetrieve aig rng discardPile = doRetrieveDiscard aig rng retrieveCards discardPile
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
bigMoneyGain :: DominionAIGame -> Int -> Maybe Card
bigMoneyGain _ n = gainCard gainCards n
  where gainCards = [ provinceCard
                    , goldCard
                    , duchyCard
                    , silverCard
                    ]

bigMoneyCardWeight :: Card -> Int
bigMoneyCardWeight _ = 1

-- | We don't buy throne rooms in big money.
bigMoneyThroneRoom :: DominionAIGame -> Maybe Card
bigMoneyThroneRoom _ = Nothing

-- | We don't buy libraries in big money.
bigMoneyLibrary :: DominionAIGame -> Card -> Bool
bigMoneyLibrary _ _ = True

-- | Simple stupid version of this logic, trash any trash cards, discard
--  remaining victory cards, keep the rest in whatever order.
bigMoneySentry :: DominionAIGame -> [Card] -> ([Card], [Card], [Card])
bigMoneySentry _ cs =
  let trash' = cs `intersect` trashCards
      disc = (trash' \\ cs) `intersect` victoryCards
      keep = (trash' ++ disc) \\ cs
    in (trash', disc, keep)

-- | Meh?
bigMoneyHandToDeck :: DominionAIGame -> Int -> [Card]
bigMoneyHandToDeck g n = take n $ (g ^. #hand) `intersect` handToDeckCards
  where handToDeckCards = [ estateCard
                          , copperCard
                          , smithyCard
                          ]

findInPlayAction :: Map.Map Card Int -> Card
findInPlayAction decks' = fst $ Map.elemAt 0 $ Map.filterWithKey (\k v -> (k ^. #cardType == Action) && v > 0) decks'

-- | Just need something
bigMoneyLurker :: DominionAIGame -> Card -> Either Card Card
bigMoneyLurker _ c = Left c

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
bigSmithyBuy :: DominionAIGame -> [DominionMove]
bigSmithyBuy g = doBuys g bigSmithyCards
  where bigSmithyCards = [ (provinceCard, alwaysBuy)
                        , (duchyCard, buyIfNumberOfCardIsBelow provinceCard 4)
                        , (smithyCard, buyN 2)
                        , (goldCard, alwaysBuy)
                        , (duchyCard, buyIfNumberOfCardIsBelow provinceCard 5)
                        , (estateCard, buyIfNumberOfCardIsBelow provinceCard 2)
                        , (silverCard, alwaysBuy)
                        , (estateCard, buyIfNumberOfCardIsBelow provinceCard 3)
                        ]

nextCardByWeight :: (Card -> Int) -> PlayerNumber -> DominionState (Maybe Card)
nextCardByWeight weights p = do
  thePlayer <- findPlayer p
  return $ headMay $ sortByWeight weights $ thePlayer ^. #hand

bigSmithyCardWeight :: Card -> Int
bigSmithyCardWeight (Card "Throne Room" _ _ _ _) = 11 -- This is for the Throne Room test
bigSmithyCardWeight (Card "Smithy" _ _ _ _)      = 10
bigSmithyCardWeight _                            = 1

-- | Just like big money buy we also gain smithy cards.
bigSmithyGain :: DominionAIGame -> Int -> Maybe Card
bigSmithyGain g n = gainCard gainCards n
  where gainCards = [ provinceCard
                    , goldCard
                    , smithyCard
                    , silverCard
                    , duchyCard
                    ]

-- | If we somehow had a throne room, definitely double the smithy.
bigSmithyThroneRoom :: DominionAIGame -> Maybe Card
bigSmithyThroneRoom g = findFirstCard g throneRoomCards
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
villageSmithyEngine4Buy :: DominionAIGame -> [DominionMove]
villageSmithyEngine4Buy g = doBuys g bigVillageSmithyEngine4Cards
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
doDiscard :: DominionAIGame -> (Int, Int) -> [Card] -> [Card]
doDiscard g minmax cs = prefPlusCards minmax cs (g ^. #hand)

-- | Core for a simple trashing logic. (min, max) and the list of
--  preferred cards to trash.
doTrash :: DominionAIGame -> (Int, Int) -> [Card] -> [Card] -> [Card]
doTrash g minmax cs possibles = prefPlusCards minmax cs possibles

-- | Core for a simple card retrieving from the discard pile logic. (min, max)
--  and the list of preferred cards to retrieve.
doRetrieveDiscard :: DominionAIGame -> (Int, Int) -> [Card] -> [Card] -> [Card]
doRetrieveDiscard pnum (min', max') cs discardPile = do
  let pref = take max' $ intersect discardPile cs
      toRetrieve
        | length pref > min' = pref
        | otherwise         = take min' $ pref ++ discardPile
    in toRetrieve

-- | Find the first card in the list that the player has in its hand, if any.
findFirstCard :: DominionAIGame -> [Card] -> Maybe Card
findFirstCard g cs = case (g ^. #hand) `intersect` cs of
    []    -> Nothing
    (x:_) -> Just x

-- | Given a player, a number of buys, and a list of preferred cards to buy
--  and a buy function, buy as many as possible given the number of buys and
--  the amount of money the player has.
doBuys :: DominionAIGame -> [(Card, DominionAIGame -> Card -> Maybe DominionMove)] -> [DominionMove]
-- doBuys g _ | trace ("doBuys: " ++ show g) False = undefined
doBuys _ [] = []
doBuys g ((x, f):xs) =
  if (g ^. #buys) > 0
    then case f g x of
            Nothing -> doBuys g xs
            Just dm -> let g' = DominionAIGame
                                { playerNum = g ^. #playerNum
                                , hand = g ^. #hand
                                , played = g ^. #played
                                , actions = g ^. #actions
                                , buys = (g ^. #buys) - 1
                                , money = (g ^. #money) - (x ^. #cost)
                                , turns = (g ^. #turns)
                                , cards = (g ^. #cards) -- TODO include the new card in this
                                , trash = (g ^. #trash)
                                , decks = (g ^. #decks) -- TODO should remove bought card
                                }
                      in dm : doBuys g' xs
    else []