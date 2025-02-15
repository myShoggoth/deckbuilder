{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE BlockArguments            #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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
    , bigMoneyIsland
    , bigMoneyAmbassador
    , bigMoneyEmbargo
    , bigMoneyHaven
    , bigMoneyNativeVillage
    , bigMoneyPearlDiver
    , bigMoneyLookout
    , bigMoneyNavigator
    , bigMoneyPirateShip
    , bigMoneyPirateShipDecision
    , bigMoneySalvage
    ) where

import Control.Lens ( (^.) )
import Data.List ( intersect, (\\), elemIndex, sortBy, findIndices, elemIndices, sort )
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
      remodelCard,
      throneRoomCard,
      treasureCards )
import DeckBuilding.Dominion.Strategies.Utils
    ( alwaysBuy,
      buyIfLowerThanTerminalActions,
      buyIfNumberOfCardIsBelow,
      buyN,
      sortByWeight, gainWhichCard )
import DeckBuilding.Dominion.Types
    ( Card(Card)
    , DominionState
    , Strategy(Strategy)
    , DominionAIGame(..)
    , DominionBuy, DominionPlayer (nativeVillage), CardType (Value), DominionAction (Estate) )
import DeckBuilding.Dominion.Utils ( findPlayer )
import DeckBuilding.Types (PlayerNumber)
import qualified Data.Map as Map
import DeckBuilding.Dominion.Cards.Base (estateCard)

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
                            bigMoneyIsland
                            bigMoneyAmbassador
                            bigMoneyEmbargo
                            bigMoneyHaven
                            bigMoneyNativeVillage
                            bigMoneyPearlDiver
                            bigMoneyLookout
                            bigMoneyNavigator
                            bigMoneyPirateShip
                            bigMoneyPirateShipDecision
                            bigMoneySalvage
                            bigMoneyTreasury
                            bigMoneyPawn
                            bigMoneyMasqueradePass
                            bigMoneySteward
                            bigMoneySwindler
                            bigMoneyWishingWell

-- | The most basic Dominion strategy: buy money and then buy provinces.
bigMoneyBuy :: DominionAIGame -> [DominionBuy]
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
bigMoneyRetrieve aig rng = doRetrieveDiscard aig rng retrieveCards
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
bigMoneyGain _ = gainWhichCard gainCards
  where gainCards = [ provinceCard
                    , goldCard
                    , duchyCard
                    , silverCard
                    ]

bigMoneyCardWeight :: Card -> Int
bigMoneyCardWeight (Card _ _ _ Value _) = 2
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

-- | Just need something
bigMoneyLurker :: DominionAIGame -> Either Card Card
bigMoneyLurker _ = Left cellarCard

-- | Pick the biggest VP by preferences, otherwise whatevs
bigMoneyIsland :: DominionAIGame -> Maybe Card
bigMoneyIsland g =
  case take 1 $ (g ^. #hand) `intersect` islandCards of
    [] -> headMay $ g ^. #hand
    [c] -> Just c
  where
    islandCards = reverse victoryCards -- Ideally we would not island curses

-- | If there are trash cards in the hand, give up the trashiest card
-- first, two if we got 'em.
bigMoneyAmbassador :: DominionAIGame -> [Card]
bigMoneyAmbassador g =
  let matches = take 1 $ (g ^. #hand) `intersect` trashCards
  in case matches of
        [] -> []
        (x:_) -> if length (elemIndices x matches) > 1
                    then [x, x]
                    else [x]

-- | Just pile the embargo tokens on the first card
bigMoneyEmbargo :: DominionAIGame -> Card
bigMoneyEmbargo g = fst . head $ Map.toList (g ^. #embargoes)

-- | I dunno, I guess have an ordered list and do that?
-- This is not great, since it is possible to not find
-- one of these cards in the hand, which would bottom out.
bigMoneyHaven :: DominionAIGame -> Card
bigMoneyHaven g = head $ take 1 $ (g ^. #hand) `intersect` havenCards
  where havenCards = [ goldCard
                     , silverCard
                     , smithyCard
                     , throneRoomCard
                     , copperCard
                     , estateCard
                     ]

-- | Dumb implementation: if the mat is empty, add to it,
-- otherwise pull its contents
bigMoneyNativeVillage :: DominionAIGame -> Bool
bigMoneyNativeVillage g = null $ g ^. #nativeVillages

-- | Just have a simple list of good cards
bigMoneyPearlDiver :: DominionAIGame -> Card -> Bool
bigMoneyPearlDiver g c = c `elem` pearls
  where
    pearls = [goldCard, silverCard, copperCard]

-- | No logic, just give them back in order
bigMoneyLookout :: DominionAIGame -> [Card] -> (Card, Card, Card)
bigMoneyLookout _ [x, y, z] = (x, y, z)
bigMoneyLookout _ _ = error "bigMoneyLookout called with anything other than three cards?!"

-- | No logic, just give  it back in the same order
bigMoneyNavigator :: DominionAIGame -> [Card] -> [Card]
bigMoneyNavigator _ xs = xs

-- | If I can afford a province, do it
bigMoneyPirateShip :: DominionAIGame -> Bool
bigMoneyPirateShip g = g ^. #pirateShip + g ^. #money < 8

-- | If they show a treasure card, have them trash it. No nuance.
bigMoneyPirateShipDecision :: DominionAIGame -> [Card] -> Maybe Card
bigMoneyPirateShipDecision _ [] = Nothing
bigMoneyPirateShipDecision _ [x] =
    if x `elem` treasureCards
      then Just x
      else Nothing
bigMoneyPirateShipDecision g xs@[x, y] = headMay $ treasureCards `intersect` xs
bigMoneyPirateShipDecision _ _ = error "More than two cards for pirate ship decision!"

-- | Pick Estate so we can test it
bigMoneySalvage :: DominionAIGame -> Maybe Card
bigMoneySalvage g = headMay $ [estateCard] `intersect` (g ^. #hand)

-- | Uh, sure, yes, let's do.
bigMoneyTreasury :: DominionAIGame -> Bool
bigMoneyTreasury _ = True

-- | Super basic pawn strategy - +1 card, +1 action
bigMoneyPawn :: DominionAIGame -> (Int, Int, Int, Int)
bigMoneyPawn _ = (1, 1, 0, 0)

-- | Take either a prefered card to pass or the first of the cheapest cards in
-- the hand if none are present.
bigMoneyMasqueradePass :: DominionAIGame -> Maybe Card
bigMoneyMasqueradePass g = case headMay $ passCards `intersect` (g ^. #hand) of
  Nothing -> headMay $ flip sortBy (g ^. #hand) \a b ->
    if a ^. #cost == b ^. #cost
      then EQ
      else if a ^. #cost < b ^. #cost
        then LT
        else GT
  Just ca -> Just ca

  where
    passCards = [curseCard, estateCard, copperCard]

-- | Just going to be real dumb here and always pick the two cards
bigMoneySteward :: DominionAIGame -> (Int, Int, [Card])
bigMoneySteward _ = (2, 0, [])

-- | Find the first supply card of the right cost, if any.
bigMoneySwindler :: DominionAIGame -> Int -> Maybe Card
bigMoneySwindler aig cost = case headMay $ Map.toList correctCostCards of
      Nothing -> Nothing
      Just (c, _) -> Just c
  where
    correctCostCards = Map.filterWithKey (\c n -> c ^. #cost == cost && n > 0) (aig ^. #decks)


-- | Guess which card is on the top of the deck. Simple algorithm, pick
-- one of the cards we have the most of.
bigMoneyWishingWell :: DominionAIGame -> Card
bigMoneyWishingWell aig  = head $ Map.keys $ Map.filter (mostCards ==) $ aig ^. #cards
  where
    mostCards = maximum $ Map.elems (aig ^. #cards)

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
                             bigMoneyIsland
                             bigMoneyAmbassador
                             bigMoneyEmbargo
                             bigMoneyHaven
                             bigMoneyNativeVillage
                             bigMoneyPearlDiver
                             bigMoneyLookout
                             bigMoneyNavigator
                             bigMoneyPirateShip
                             bigMoneyPirateShipDecision
                             bigMoneySalvage
                             bigMoneyTreasury
                             bigMoneyPawn
                             bigMoneyMasqueradePass
                             bigMoneySteward
                             bigMoneySwindler
                             bigMoneyWishingWell


-- | Just like big money buy also buy up to two smithy cards.
bigSmithyBuy :: DominionAIGame -> [DominionBuy]
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
bigSmithyCardWeight (Card _ _ _ Value _)         = 12
bigSmithyCardWeight (Card "Throne Room" _ _ _ _) = 11 -- This is for the Throne Room test
bigSmithyCardWeight (Card "Smithy" _ _ _ _)      = 10
bigSmithyCardWeight _                            = 1

-- | Just like big money buy we also gain smithy cards.
bigSmithyGain :: DominionAIGame -> Int -> Maybe Card
bigSmithyGain _ = gainWhichCard gainCards
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
                                bigMoneyIsland
                                bigMoneyAmbassador
                                bigMoneyEmbargo
                                bigMoneyHaven
                                bigMoneyNativeVillage
                                bigMoneyPearlDiver
                                bigMoneyLookout
                                bigMoneyNavigator
                                bigMoneyPirateShip
                                bigMoneyPirateShipDecision
                                bigMoneySalvage
                                bigMoneyTreasury
                                bigMoneyPawn
                                bigMoneyMasqueradePass
                                bigMoneySteward
                                bigMoneySwindler
                                bigMoneyWishingWell

-- | The buy strategy
villageSmithyEngine4Buy :: DominionAIGame -> [DominionBuy]
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
villageSmithyEngine4CardWeight (Card _ _ _ Value _)     = 11
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
    | length pref >= min' = pref
    | otherwise         = take min' $ pref ++ cs
  where pref = prefCards max' cs h'
        h' = sortBy (definedOrderSort cs) h

-- | Ordering based on the ordering of the same values in an input list.
definedOrderSort :: Eq a => [a] -> a -> a -> Ordering
definedOrderSort order x y | x == y = EQ
                           | x `elemIndex` order < y `elemIndex` order = LT
                           | otherwise = GT

-- | Core for a simple discarding logic. (min, max) and the list of
--  preferred cards to discard.
doDiscard :: DominionAIGame -> (Int, Int) -> [Card] -> [Card]
doDiscard g minmax cs = prefPlusCards minmax cs (g ^. #hand)

-- | Core for a simple trashing logic. (min, max) and the list of
--  preferred cards to trash.
doTrash :: DominionAIGame -> (Int, Int) -> [Card] -> [Card] -> [Card]
doTrash _ = prefPlusCards

-- | Core for a simple card retrieving from the discard pile logic. (min, max)
--  and the list of preferred cards to retrieve.
doRetrieveDiscard :: DominionAIGame -> (Int, Int) -> [Card] -> [Card] -> [Card]
doRetrieveDiscard _ (min', max') cs discardPile = do
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
doBuys :: DominionAIGame -> [(Card, DominionAIGame -> Card -> Maybe DominionBuy)] -> [DominionBuy]
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
                                , turns = g ^. #turns
                                , cards = g ^. #cards -- TODO include the new card in this
                                , trash = g ^. #trash
                                , decks = g ^. #decks -- TODO should remove bought card
                                , embargoes = g ^. #embargoes
                                , nativeVillages = g ^. #nativeVillages
                                , pirateShip = g ^. #pirateShip
                                }
                      in dm : doBuys g' xs
    else []