{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
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
      explorerCard,
      fishingVillageCard,
      ghostShipCard,
      havenCard,
      islandCard,
      lighthouseCard,
      lookoutCard,
      merchantShipCard,
      nativeVillageCard,
      pearlDiverCard,
      warehouseCard,
      firstGameKingdomCards,
      pirateShipCard,
      salvagerCard,
      seaHagCard,
      curseCard,
      treasureMapCard,
      silverCard,
      provinceCard,
      goldCard, treasureCards
    )

import Control.Monad.State ( execState, evalState )
import System.Random ( mkStdGen )
import DeckBuilding.Dominion.Utils ( deal, findPlayer )
import DeckBuilding.Types ( PlayerNumber(PlayerNumber, unPlayerNumber), Game (tallyPoints) )
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
import Data.Generics.Product ( HasField(field) )
import Dominion.Utils ( defaultConfig, initialState, p0, p1, setDeck, setHand )
import DeckBuilding.Dominion.Cards.Seaside (ambassadorCard)
import DeckBuilding.Dominion.Cards.Base (provinceCard)

spec :: Spec
spec = do
    describe "Ambassador action" $ do
        let ((p1AfterCard, p2AfterCard), _) = initialState defaultConfig $ do
                ambassadorCard ^. #action $ p0
                p0' <- findPlayer p0
                p1' <- findPlayer p1
                return (p0', p1')
        it "gives up a trash card (estate)" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 4
        it "gives the other player a copy" $ do
            length (p2AfterCard ^. #discard) `shouldBe` 1

    describe "Caravan action" $ do
        let ((p1AfterCard, p1AfterCard'), _) = initialState defaultConfig $ do
                caravanCard ^. #action $ p0
                p0' <- findPlayer p0
                head (p0' ^. #duration) p0
                p0'' <- findPlayer p0
                return (p0', p0'')
        it "adds a duration entry" $ do
            length (p1AfterCard ^. #duration) `shouldBe` 1
        it "draws a card, does not use an action" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 6
            p1AfterCard ^. #actions `shouldBe` 1
        it "duration action draws another card" $ do
            length (p1AfterCard' ^. #hand) `shouldBe` 7

    describe "Cutpurse action" $ do
        let ((p1AfterCard, p2AfterCard), _) = initialState defaultConfig $ do
                cutpurseCard ^. #action $ p0
                p0' <- findPlayer p0
                p1' <- findPlayer p1
                return (p0', p1')
        it "gives 2 money" $ do
            p1AfterCard ^. #money `shouldBe` 2
        it "discards a copper from the opponent's hand" $ do
            length (p2AfterCard ^. #hand) `shouldBe` 4

    describe "Embargo action" $  do
        let ((p1AfterCard, p2AfterCard), afterCard) = initialState defaultConfig $ do
                embargoCard ^. #action $ p0
                p0' <- findPlayer p0
                p1' <- findPlayer p1
                return (p0', p1')
        it "adds an embargo token to the first Supply deck" $ do
            (snd . head $ Map.toList $ afterCard ^. #embargoes) `shouldBe` 1
        it "gives two money" $ do
            (p1AfterCard ^. #money) `shouldBe` 2

    describe "Explorer action" $ do
        it "gains a silver to the hand if there is no province" $ do
            let (p1AfterCard, _) = initialState defaultConfig $ do
                    explorerCard ^. #action $ p0
                    findPlayer p0
            headMay (p1AfterCard ^. #hand) `shouldBe` Just silverCard
            length (p1AfterCard ^. #hand) `shouldBe` 6
        it "gains a gold to the hand if there is a province" $ do
            let (p1AfterCard, _) = initialState defaultConfig $ do
                    #players . ix (unPlayerNumber p0) . #hand %= (provinceCard:)
                    explorerCard ^. #action $ p0
                    findPlayer p0
            headMay (p1AfterCard ^. #hand) `shouldBe` Just goldCard
            length (p1AfterCard ^. #hand) `shouldBe` 7

    describe "Fishing Village action" $ do
        let ((p1AfterCard, p1AfterCard'), _) = initialState defaultConfig $ do
                fishingVillageCard ^. #action $ p0
                p0' <- findPlayer p0
                head (p0' ^. #duration) p0
                p0'' <- findPlayer p0
                return (p0', p0'')
        it "adds a duration entry" $ do
            length (p1AfterCard ^. #duration) `shouldBe` 1
        it "adds two actions and one money" $ do
            p1AfterCard ^. #money `shouldBe` 1
            p1AfterCard ^. #actions `shouldBe` 2
        it "adds one action and one money the next turn" $ do
            p1AfterCard' ^. #money `shouldBe` 2
            p1AfterCard' ^. #actions `shouldBe` 3

    describe "Ghost Ship action" $ do
        let ((p1AfterCard, p2AfterCard), _) = initialState defaultConfig $ do
                ghostShipCard ^. #action $ p0
                p0' <- findPlayer p0
                p1' <- findPlayer p1
                return (p0', p1')
        it "draws two cards" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 7
        it "causes the other player to put cards back on their deck down to 3 in hand" $ do
            length (p2AfterCard ^. #hand) `shouldBe` 3
            length (p2AfterCard ^. #deck) `shouldBe` 7

    describe "Haven action" $ do
        let (p1AfterCard, _) = initialState defaultConfig $ do
                havenCard ^. #action $ p0
                findPlayer p0
        it "adds a duration entry" $ do
            length (p1AfterCard ^. #duration) `shouldBe` 1
        it "draws a card, but removes the Haven and the selected card to haven" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 4
        it "adds and uses an action" $ do
            p1AfterCard ^. #actions `shouldBe` 1

    describe "Island action" $ do
        let (p1AfterCard, afterCard) = initialState defaultConfig $ do
                islandCard ^. #action $ p0
                findPlayer p0
        it "puts two cards on the Island mat, one is the Island" $ do
            length (p1AfterCard ^. #island) `shouldBe` 2
            islandCard `elem` (p1AfterCard ^. #island) `shouldBe` True
        it "counts cards on the Island in the final score" $ do
            let afterCardDone = evalState tallyPoints afterCard
            Map.fromList afterCardDone Map.! "Player 1" `shouldBe` 5

    describe "Lighthouse action" $ do
        let ((p1AfterCard, p1AfterCard'), _) = initialState defaultConfig $ do
                lighthouseCard ^. #action $ p0
                p0' <- findPlayer p0
                head (p0' ^. #duration) p0
                p0'' <- findPlayer p0
                return (p0', p0'')
        it "increments the lighthouse counter" $ do
            p1AfterCard ^. #lighthouse `shouldBe` 1
        it "decrements the lighthouse counter in the duration" $ do
            p1AfterCard' ^. #lighthouse `shouldBe` 0

    describe "Lookout action" $ do
        let (p1AfterCard, afterCard) = initialState defaultConfig $ do
                lookoutCard ^. #action $ p0
                findPlayer p0
        it "trashes one, discards one, puts one back on the deck" $ do
            length (p1AfterCard ^. #discard) `shouldBe` 1
            length (afterCard ^. #trash) `shouldBe` 1
            length (p1AfterCard ^. #deck) `shouldBe` 3

    describe "Merchant Ship action" $ do
        let ((p1AfterCard, p1AfterCard'), _) = initialState defaultConfig $ do
                merchantShipCard ^. #action $ p0
                p0' <- findPlayer p0
                head (p0' ^. #duration) p0
                p0'' <- findPlayer p0
                return (p0', p0'')
        it "adds two money this turn" $ do
            p1AfterCard ^. #money `shouldBe` 2
        it "adds another two the next turn" $ do
            p1AfterCard' ^. #money `shouldBe` 4

    describe "Native Village action" $ do
        let ((p1AfterCard, p1AfterCard'), _) = initialState defaultConfig $ do
                nativeVillageCard ^. #action $ p0
                p0' <- findPlayer p0
                nativeVillageCard ^. #action $ p0
                p0'' <- findPlayer p0
                return (p0', p0'')
        it "adds the top of the deck to the Native Village at" $ do
            length (p1AfterCard ^. #nativeVillage) `shouldBe` 1
            length (p1AfterCard ^. #deck) `shouldBe` 4
        it "puts the contents of the Native Village into the hand" $ do
            length (p1AfterCard' ^. #nativeVillage) `shouldBe` 0
            length (p1AfterCard' ^. #hand) `shouldBe` 6

    describe "Navigator action" $  do
        let (p1AfterCard, _) = initialState defaultConfig $ do
                embargoCard ^. #action $ p0
                findPlayer p0
        it "gives two money" $ do
            (p1AfterCard ^. #money) `shouldBe` 2

    describe "Pearl Diver action" $ do
        let ((p1AfterDeal, p1AfterCard), _) = initialState defaultConfig $ do
                p0' <- findPlayer p0
                pearlDiverCard ^. #action $ p0
                p0'' <- findPlayer p0
                return (p0', p0'')
        it "moves the bottom card of the deck to the top" $ do
            lastMay (p1AfterDeal ^. #deck) `shouldBe` headMay (p1AfterCard ^. #deck)

    describe "Pirate Ship action" $ do
        let (p1AfterCard, _) = initialState defaultConfig $ do
                pirateShipCard ^. #action $ p0
                findPlayer p0
        it "gets a pirate ship mat coin token" $ do
            p1AfterCard ^. #pirateShip `shouldBe` 1

    describe "Salvager action" $ do
        let (p1AfterCard, afterCard) = initialState defaultConfig $ do
                salvagerCard ^. #action $ p0
                findPlayer p0
        it "gets coins for trashing an estate" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 4
            p1AfterCard ^. #money `shouldBe` 2
            length (afterCard ^. #trash) `shouldBe` 1

    describe "Sea Hag action" $ do
        let ((p1AfterCard, p2AfterCard), afterCard) = initialState defaultConfig $ do
                seaHagCard ^. #action $ p0
                p0' <- findPlayer p0
                p1' <- findPlayer p1
                return (p0', p1')
        it "causes other players to get curses" $ do
            p1AfterCard ^. #discard `shouldBe` []
            head (p2AfterCard ^. #discard) `shouldBe` curseCard
        it "causes other players to discard the top card of their deck" $ do
            length (p2AfterCard ^. #discard) `shouldBe` 2
            length (p2AfterCard ^. #deck) `shouldBe` 4

    describe "Treasure Map action" $ do
        it "does nothing if there aren't two treasure maps" $ do
            let (p1AfterCard, _) = initialState defaultConfig $ do
                    treasureMapCard ^. #action $ p0
                    findPlayer p0
            length (p1AfterCard ^. #deck) `shouldBe` 5
        let (p1AfterCard, afterCard) = initialState defaultConfig $ do
                #players . ix (unPlayerNumber p0) . #hand %= ([treasureMapCard, treasureMapCard] ++)
                treasureMapCard ^. #action $ p0
                findPlayer p0
        it "trashes two treasure maps" $ do
            afterCard ^. #trash `shouldBe` [treasureMapCard, treasureMapCard]
        it "adds four gold to the deck" $ do
            length (p1AfterCard ^. #deck) `shouldBe` 9

    describe "Warehouse action" $ do
        let (p1AfterCard, _) = initialState defaultConfig $ do
                warehouseCard ^. #action $ p0
                findPlayer p0
        it "draws and then discards three" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 5
            length (p1AfterCard ^. #discard) `shouldBe` 3
