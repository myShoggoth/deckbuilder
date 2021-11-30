{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DataKinds                 #-}
module Dominion.Cards.SeasideSpec
    ( spec
    ) where

import Test.Hspec ( shouldBe, it, describe, Spec )
import Control.Lens ( (^?), (^.), (%=), set, Ixed(ix) )
import DeckBuilding.Dominion.Cards
    ( ambassadorCard,
      caravanCard,
      cutpurseCard,
      embargoCard,
      fishingVillageCard,
      havenCard,
      islandCard,
      lighthouseCard,
      lookoutCard,
      nativeVillageCard,
      pearlDiverCard,
      warehouseCard,
      firstGameKingdomCards,
      pirateShipCard,
      salvagerCard,
      seaHagCard,
      curseCard,
      treasureMapCard
    )

import Control.Monad.State ( execState, evalState )
import System.Random ( mkStdGen )
import DeckBuilding.Dominion.Utils ( deal )
import DeckBuilding.Types ( PlayerNumber(PlayerNumber), Game (tallyPoints) )
import DeckBuilding.Dominion.Types
    ( Card,
      DominionPlayer(DominionPlayer, nativeVillage),
      DominionConfig(DominionConfig),
      Strategy(Strategy),
      DominionState,
      CardType(Action),
      DominionGame(DominionGame), DominionBoard(DominionBoard), DominionAIGame (pirateShip), DominionPlayerTurn (actions) )
import DeckBuilding.Dominion.Strategies.Basic
    ( bigMoneyCardWeight,
      bigMoneyDiscard,
      bigMoneyHandToDeck,
      bigMoneyLibrary,
      bigMoneyLurker,
      bigMoneyRetrieve,
      bigMoneySentry,
      bigMoneyStrategy,
      bigMoneyThroneRoom,
      bigMoneyTrash,
      bigSmithyBuy,
      bigSmithyStrategy,
      bigMoneyIsland,
      nextCardByWeight )
import DeckBuilding.Dominion
    ( basicDecks, configToGame, makeDecks )
import qualified Data.Map as Map
import Safe (headMay, lastMay)
import DeckBuilding.Dominion.Cards.Seaside (treasureMapCard)
import Data.Generics.Product ( HasField(field) )

spec :: Spec
spec = do
    let g = mkStdGen 45752345316
    let c = DominionConfig
                [ ("Player 1", bigMoneyStrategy)
                , ("Player 2", bigSmithyStrategy)
                ]
                firstGameKingdomCards
    let p0 = PlayerNumber 0
        p1 = PlayerNumber 1
    let dg = configToGame c g
    let afterDeal           = execState (deal 5 p0) dg
    let (Just p1AfterDeal)  = afterDeal ^? #players . ix 0

    describe "Ambassador action" $ do
        let afterCard = execState ((ambassadorCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        let (Just p2AfterCard) = afterCard ^? #players . ix 1
        it "gives up a trash card (estate)" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 4
        it "gives the other player a copy" $ do
            length (p2AfterCard ^. #discard) `shouldBe` 11

    describe "Caravan action" $ do
        let afterCard = execState ((caravanCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        let afterCard' = execState (head (p1AfterCard ^. #duration) p0) afterCard
        let (Just p1AfterCard') = afterCard' ^? #players . ix 0
        it "adds a duration entry" $ do
            length (p1AfterCard ^. #duration) `shouldBe` 1
        it "draws a card, does not use an action" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 6
            p1AfterCard ^. #actions `shouldBe` 1
        it "duration action draws another card" $ do
            length (p1AfterCard' ^. #hand) `shouldBe` 7

    describe "Cutpurse action" $ do
        let afterCard = execState (deal 5 p1) afterDeal
        let afterCard2 = execState ((cutpurseCard ^. #action) p0) afterCard
        let (Just p1AfterCard) = afterCard2 ^? #players . ix 0
        let (Just p2AfterCard) = afterCard2 ^? #players . ix 1
        it "gives 2 money" $ do
            p1AfterCard ^. #money `shouldBe` 2
        it "discards a copper from the opponent's hand" $ do
            length (p2AfterCard ^. #hand) `shouldBe` 4

    describe "Embargo action" $  do
        let afterCard = execState ((embargoCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        let (Just p2AfterCard) = afterCard ^? #players . ix 1
        it "adds an embargo token to the first Supply deck" $ do
            (snd . head $ Map.toList $ afterCard ^. #embargoes) `shouldBe` 1
        it "gives two money" $ do
            (p1AfterCard ^. #money) `shouldBe` 2

    describe "Fishing Village action" $ do
        let afterCard = execState ((fishingVillageCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        let afterCard' = execState (head (p1AfterCard ^. #duration) p0) afterCard
        let (Just p1AfterCard') = afterCard' ^? #players . ix 0
        it "adds a duration entry" $ do
            length (p1AfterCard ^. #duration) `shouldBe` 1
        it "adds two actions and one money" $ do
            p1AfterCard ^. #money `shouldBe` 1
            p1AfterCard ^. #actions `shouldBe` 2
        it "adds one action and one money the next turn" $ do
            p1AfterCard' ^. #money `shouldBe` 2
            p1AfterCard' ^. #actions `shouldBe` 3

    describe "Haven action" $ do
        let afterCard = execState ((havenCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        it "adds a duration entry" $ do
            length (p1AfterCard ^. #duration) `shouldBe` 1
        it "draws a card, but removes the Haven and the selected card to haven" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 4
        it "adds and uses an action" $ do
            p1AfterCard ^. #actions `shouldBe` 1

    describe "Island action" $ do
        let afterCard = execState ((islandCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        it "puts two cards on the Island mat, one is the Island" $ do
            length (p1AfterCard ^. #island) `shouldBe` 2
            islandCard `elem` (p1AfterCard ^. #island) `shouldBe` True
        it "counts cards on the Island in the final score" $ do
            let afterCardDone = evalState tallyPoints afterCard
            Map.fromList afterCardDone Map.! "Player 1" `shouldBe` 5

    describe "Lighthouse action" $ do
        let afterCard = execState ((lighthouseCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        let afterCard' = execState (head (p1AfterCard ^. #duration) p0) afterCard
        let (Just p1AfterCard') = afterCard' ^? #players . ix 0
        it "increments the lighthouse counter" $ do
            p1AfterCard ^. #lighthouse `shouldBe` 1
        it "decrements the lighthouse counter in the duration" $ do
            p1AfterCard' ^. #lighthouse `shouldBe` 0

    describe "Lookout action" $ do
        let afterCard = execState ((lookoutCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        it "trashes one, discards one, puts one back on the deck" $ do
            length (p1AfterCard ^. #discard) `shouldBe` 1
            length (afterCard ^. #trash) `shouldBe` 1
            length (p1AfterCard ^. #deck) `shouldBe` 3

    describe "Native Village action" $ do
        let afterCard = execState ((nativeVillageCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        let afterCard' = execState ((nativeVillageCard ^. #action) p0) afterCard
        let (Just p1AfterCard') = afterCard' ^? #players . ix 0
        it "adds the top of the deck to the Native Village at" $ do
            length (p1AfterCard ^. #nativeVillage) `shouldBe` 1
            length (p1AfterCard ^. #deck) `shouldBe` 4
        it "puts the contents of the Native Village into the hand" $ do
            length (p1AfterCard' ^. #nativeVillage) `shouldBe` 0
            length (p1AfterCard' ^. #hand) `shouldBe` 6

    describe "Navigator action" $  do
        let afterCard = execState ((embargoCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        it "gives two money" $ do
            (p1AfterCard ^. #money) `shouldBe` 2

    describe "Pearl Diver action" $ do
        let afterCard = execState ((pearlDiverCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        it "moves the bottom card of the deck to the top" $ do
            lastMay (p1AfterDeal ^. #deck) `shouldBe` headMay (p1AfterCard ^. #deck)

    describe "Pirate Ship action" $ do
        let afterCard = execState ((pirateShipCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        it "gets a pirate ship mat coin token" $ do
            p1AfterCard ^. #pirateShip `shouldBe` 1

    describe "Salvager action" $ do
        let afterCard = execState ((salvagerCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        it "gets coins for trashing an estate" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 4
            p1AfterCard ^. #money `shouldBe` 2
            length (afterCard ^. #trash) `shouldBe` 1

    describe "Sea Hag action" $ do
        let afterCard = execState ((seaHagCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        let (Just p2') = afterCard ^? #players . ix 1
        it "causes other players to get curses" $ do
            p1AfterCard ^. #discard `shouldBe` []
            head (p2' ^. #discard) `shouldBe` curseCard
        it "causes other players to discard the top card of their deck" $ do
            length (p2' ^. #discard) `shouldBe` 2
            length (p2' ^. #deck) `shouldBe` 9

    describe "Treasure Map action" $ do
        let afterCard = execState ((treasureMapCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        it "does nothing if there aren't two treasure maps" $ do
            length (p1AfterCard ^. #deck) `shouldBe` 5
        let afterTMsAdded = execState ((field @"players" . ix 0 . #hand) %= ([treasureMapCard, treasureMapCard] ++)) afterCard
        let afterTM = execState ((treasureMapCard ^. #action) p0) afterTMsAdded
        let (Just p1AfterTM) = afterTM ^? #players . ix 0
        it "trashes two treasure maps" $ do
            afterTM ^. #trash `shouldBe` [treasureMapCard, treasureMapCard]
        it "adds four gold to the deck" $ do
            length (p1AfterTM ^. #deck) `shouldBe` 9

    describe "Warehouse action" $ do
        let afterCard = execState ((warehouseCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        it "draws and then discards three" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 5
            length (p1AfterCard ^. #discard) `shouldBe` 3
