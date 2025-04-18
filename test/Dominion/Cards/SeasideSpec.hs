{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}

module Dominion.Cards.SeasideSpec
    ( spec
    ) where

import Test.Hspec ( shouldBe, shouldNotBe, it, describe, Spec )
import Control.Lens ( (^?), (^.), (%=), set, Ixed(ix), (.=) )
import DeckBuilding.Dominion.Cards
    ( ambassadorCard,
      astrolabeCard,
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
      tacticianCard,
      treasureMapCard,
      silverCard,
      provinceCard,
      goldCard,
      treasureCards,
      wharfCard,
      treasuryCard,
      outpostCard,
      estateCard,
      copperCard,
      smugglersCard,
      tidePoolsCard,
      seaChartCard,
      blockadeCard,
      monkeyCard,
      corsairCard,
      sailorCard,
      seaWitchCard
    )

import Control.Monad.State ( execState, evalState )
import System.Random ( mkStdGen )
import DeckBuilding.Dominion.Utils ( deal, findPlayer )
import DeckBuilding.Types ( PlayerNumber(PlayerNumber, unPlayerNumber), Game (tallyPoints) )
import DeckBuilding.Dominion.Types
    ( Card (action),
      DominionPlayer(DominionPlayer, nativeVillage),
      DominionConfig(DominionConfig),
      DominionDraw(DominionDraw),
      DominionAction(OutpostDuration, Copper, Estate),
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
    ( basicDecks, configToGame, makeDecks, resetTurn )
import qualified Data.Map as Map
import Safe (headMay, lastMay)
import Data.Generics.Product ( HasField(field) )
import Dominion.Utils ( defaultConfig, initialState, p0, p1, setDeck, setHand )
import DeckBuilding.Dominion.Cards.Utils ( gainCardsToDiscard, gainCardsToDeck )
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

    describe "Astrolabe action" $ do
        let ((p1AfterCard, p1AfterCard'), _) = initialState defaultConfig $ do
                astrolabeCard ^. #action $ p0
                p0' <- findPlayer p0
                head (map snd (p0' ^. #duration)) p0
                p0'' <- findPlayer p0
                return (p0', p0'')
        it "adds a duration entry" $ do
            length (map snd (p1AfterCard ^. #duration)) `shouldBe` 1
        it "adds one buy and one money" $ do
            p1AfterCard ^. #money `shouldBe` 1
            p1AfterCard ^. #buys `shouldBe` 2
        it "adds one action and one money the next turn" $ do
            p1AfterCard' ^. #money `shouldBe` 2
            p1AfterCard' ^. #buys `shouldBe` 3

    describe "Caravan action" $ do
        let ((p1AfterCard, p1AfterCard'), _) = initialState defaultConfig $ do
                caravanCard ^. #action $ p0
                p0' <- findPlayer p0
                head (map snd (p0' ^. #duration)) p0
                p0'' <- findPlayer p0
                return (p0', p0'')
        it "adds a duration entry" $ do
            length (map snd (p1AfterCard ^. #duration)) `shouldBe` 1
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
                head (map snd (p0' ^. #duration)) p0
                p0'' <- findPlayer p0
                return (p0', p0'')
        it "adds a duration entry" $ do
            length (map snd (p1AfterCard ^. #duration)) `shouldBe` 1
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
            length (map snd (p1AfterCard ^. #duration)) `shouldBe` 1
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
                head (map snd (p0' ^. #duration)) p0
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
                head (map snd (p0' ^. #duration)) p0
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

    describe "Outpost action" $ do
        let ((p1AfterCard, das), _) = initialState defaultConfig $ do
                outpostCard ^. #action $ p0
                das <- resetTurn p0
                p0' <- findPlayer p0
                return (p0', das)
        it "returns the actions for the turn" $ do
            das `shouldBe` [OutpostDuration (DominionDraw [copperCard, copperCard, estateCard]) [], Copper, Copper, Estate]

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

    describe "Sea Chart action" $ do
        let (p1AfterCard, afterCard) = initialState defaultConfig $ do
                seaChartCard ^. #action $ p0
                findPlayer p0

        it "provides +1 Card and +1 Action when played" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 6 -- 5 initial + 1 drawn
            p1AfterCard ^. #actions `shouldBe` 1

        it "reveals the top card and adds it to hand if a copy is in play" $ do
            let (p1AfterRevealWithCopy, _) = initialState defaultConfig $ do
                    #players . ix (unPlayerNumber p0) . #played .= [seaChartCard]
                    #players . ix (unPlayerNumber p0) . #deck %= ([goldCard, seaChartCard]++)
                    seaChartCard ^. #action $ p0
                    findPlayer p0
            length (p1AfterRevealWithCopy ^. #hand) `shouldBe` 7 -- 5 initial + 1 drawn + 1 revealed
            headMay (p1AfterRevealWithCopy ^. #deck) `shouldNotBe` Just seaChartCard

        it "reveals the top card and leaves it on the deck if it is not in play" $ do
            let (p1AfterRevealWithoutCopy, _) = initialState defaultConfig $ do
                    #players . ix (unPlayerNumber p0) . #deck %= ([goldCard, seaChartCard]++)
                    seaChartCard ^. #action $ p0
                    findPlayer p0
            length (p1AfterRevealWithoutCopy ^. #hand) `shouldBe` 6 -- 5 initial + 1 drawn, no revealed card added
            headMay (p1AfterRevealWithoutCopy ^. #deck) `shouldBe` Just seaChartCard

    describe "Smugglers action" $ do
        let ((p1AfterCard, p2AfterCard), afterCard) = initialState defaultConfig $ do
                #players . ix (unPlayerNumber p1) . #gained .= [goldCard]
                smugglersCard ^. #action $ p0
                p0' <- findPlayer p0
                p1' <- findPlayer p1
                return (p0', p1')
        it "gains a card that costs 6 or less that the previous player gained in their last turn" $ do
            p1AfterCard ^. #discard `shouldBe` [goldCard]

    describe "Tactician action" $ do
        let (p1AfterCard, _) = initialState defaultConfig $ do
                tacticianCard ^. #action $ p0
                p0' <- findPlayer p0
                head (map snd (p0' ^. #duration)) p0
                findPlayer p0
        it "draws 5 cards, adds one action and one buy when played with cards in hand" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 5
            p1AfterCard ^. #actions `shouldBe` 1
            p1AfterCard ^. #buys `shouldBe` 2

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

    describe "Treasury action" $ do
        let (p1AfterCard, _) = initialState defaultConfig $ do
                treasuryCard ^. #action $ p0
                #players . ix (unPlayerNumber p0) . #played %= (treasuryCard:)
                resetTurn p0
                deal 5 p0
                findPlayer p0
        it "is put on the deck when no victory card is bought" $ do
            treasuryCard `elem` p1AfterCard ^. #hand `shouldBe` True

    describe "Warehouse action" $ do
        let (p1AfterCard, _) = initialState defaultConfig $ do
                warehouseCard ^. #action $ p0
                findPlayer p0
        it "draws and then discards three" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 5
            length (p1AfterCard ^. #discard) `shouldBe` 3

    describe "Wharf action" $ do
        let ((p1AfterCard, p1AfterCard'), _) = initialState defaultConfig $ do
                wharfCard ^. #action $ p0
                p0' <- findPlayer p0
                head (map snd (p0' ^. #duration)) p0
                p0'' <- findPlayer p0
                return (p0', p0'')
        it "draws two cards when played" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 7
            length (p1AfterCard ^. #deck) `shouldBe` 3
        it "draws two more in the next turn" $ do
            length (p1AfterCard' ^. #hand) `shouldBe` 9
            length (p1AfterCard' ^. #deck) `shouldBe` 1

    describe "Tide Pools action" $ do
        let ((p1AfterCard, p1AfterDuration), _) = initialState defaultConfig $ do
                tidePoolsCard ^. #action $ p0
                p0' <- findPlayer p0
                resetTurn p0 -- Reset the turn before invoking the duration action
                deal 5 p0 -- Deal a new hand after resetting the turn
                head (map snd (p0' ^. #duration)) p0
                p0'' <- findPlayer p0
                return (p0', p0'')

        it "provides +3 Cards and +1 Action when played" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 8 -- 5 initial + 3 drawn
            p1AfterCard ^. #actions `shouldBe` 1

        it "discards 2 cards at the start of the next turn" $ do
            length (p1AfterDuration ^. #hand) `shouldBe` 3 -- 5 initial - 2 discarded

    describe "Blockade action" $ do
        let ((p1AfterCard, p1AfterDuration), afterCard) = initialState defaultConfig $ do
                blockadeCard ^. #action $ p0
                p0' <- findPlayer p0
                resetTurn p0 -- Reset the turn before invoking the duration action
                deal 5 p0 -- Deal a new hand after resetting the turn
                head (map snd (p0' ^. #duration)) p0
                p0'' <- findPlayer p0
                return (p0', p0'')

        it "gains a card costing up to $4" $ do
            length (p1AfterCard ^. #discard) `shouldBe` 1
            (headMay $ p1AfterCard ^. #discard) `shouldNotBe` Nothing

        it "provides +$2 at the start of the next turn" $ do
            p1AfterDuration ^. #money `shouldBe` 2

    describe "Monkey action" $ do
        let ((p1AfterCard, p1AfterDuration), _) = initialState defaultConfig $ do
                monkeyCard ^. #action $ p0
                p0' <- findPlayer p0
                resetTurn p0 -- Reset the turn before invoking the duration action
                deal 5 p0 -- Deal a new hand after resetting the turn
                head (map snd (p0' ^. #duration)) p0
                p0'' <- findPlayer p0
                return (p0', p0'')

        it "provides +1 Card at the start of the next turn" $ do
            length (p1AfterDuration ^. #hand) `shouldBe` 6 -- 5 initial + 1 drawn

        it "reactively draws a card when the player to the right gains a card" $ do
            let (p1AfterReactive, _) = initialState defaultConfig $ do
                    monkeyCard ^. #action $ p0
                    resetTurn p0
                    deal 5 p0 -- Deal a new hand after resetting the turn
                    gainCardsToDiscard p1 [silverCard] -- Simulate the player to the right gaining a card
                    findPlayer p0
            length (p1AfterReactive ^. #hand) `shouldBe` 6 -- 5 initial + 1 reactive draw

    describe "Corsair action" $ do
        let ((p1AfterCard, p2AfterCard), afterCard) = initialState defaultConfig $ do
                corsairCard ^. #action $ p0
                p0' <- findPlayer p0
                p1' <- findPlayer p1
                return (p0', p1')
        it "forces other players to trash a Treasure from their hand" $ do
            length (afterCard ^. #trash) `shouldBe` 1
            (headMay $ afterCard ^. #trash) `shouldNotBe` Nothing
        it "gives the player a Silver to their hand" $ do
            headMay (p1AfterCard ^. #hand) `shouldBe` Just silverCard
            length (p1AfterCard ^. #hand) `shouldBe` 6

    describe "Sailor action" $ do
        let ((p1AfterCard, p1AfterGain), _) = initialState defaultConfig $ do
                sailorCard ^. #action $ p0
                p0' <- findPlayer p0
                #players . ix (unPlayerNumber p0) . #gained .= [silverCard]
                sailorCard ^. #action $ p0
                p0'' <- findPlayer p0
                return (p0', p0'')

        it "provides +2 Actions when played" $ do
            p1AfterCard ^. #actions `shouldBe` 2

        it "provides +$2 if a card was gained this turn" $ do
            p1AfterGain ^. #money `shouldBe` 2

    describe "Sea Witch action" $ do
        let ((p1AfterCard, p2AfterCard), afterCard) = initialState defaultConfig $ do
                seaWitchCard ^. #action $ p0
                p0' <- findPlayer p0
                p1' <- findPlayer p1
                return (p0', p1')
        it "provides +2 Cards when played" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 7 -- 5 initial + 2 drawn
        it "causes other players to gain a Curse" $ do
            headMay (p2AfterCard ^. #discard) `shouldBe` Just curseCard
            length (p2AfterCard ^. #discard) `shouldBe` 1
