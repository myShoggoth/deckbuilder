import DeckBuilding.Dominion
import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Cards
import DeckBuilding.Dominion.Utils

import System.Random
import Data.List
import Test.Hspec
import qualified Test.QuickCheck as QC
import Control.Exception (evaluate)
import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map

main :: IO ()
main = do
  g <- newStdGen
  hspec $ do
    let p1                                = newPlayer "Player 1"
    let p2                                = newPlayer "Player 2"
    let (p1AfterDeal, afterDeal)          = runState (deal 5 p1) $ Game [p1, p2] (basicDecks 2) g
    let (p2AfterDeal, afterDeal2)         = runState (deal 5 p2) afterDeal
    let (p1AfterEvaluate, afterEvaluate)  = runState (evaluateHand p1AfterDeal) afterDeal2
    let (p1AfterReset, afterReset)        = runState (resetTurn p1) afterEvaluate

    describe "DeckBuilding.Dominion.Cards.deal" $ do
      it "deals the correct number of cards" $ do
        (length (p1AfterDeal  ^. hand)) `shouldBe` 5
        (length (p1AfterDeal ^. deck)) `shouldBe` 5
        (length (p1AfterDeal ^. discard)) `shouldBe` 0

      it "has a total of seven copper" $ do
        (length (filter (== copperCard) ((p1AfterDeal ^. hand) ++ (p1AfterDeal ^. deck)))) `shouldBe` 7

      it "has a total of three estates" $ do
        (length (filter (== estateCard) ((p1AfterDeal ^. hand) ++ (p1AfterDeal ^. deck)))) `shouldBe` 3

    describe "DeckBuilding.Dominion.evaluateHand" $ do
      it "has no more cards in hand" $ do
        (length (p1AfterEvaluate ^. played)) `shouldBe` 5
        (length (p1AfterEvaluate ^. hand)) `shouldBe` 0
        (length (p1AfterDeal ^. discard)) `shouldBe` 0

      it "calculates the right amount of money" $ do
        (p1AfterEvaluate ^. money) `shouldBe` (length (filter (== copperCard) (p1AfterEvaluate ^. played)))

      it "calcualtes the right amount of victory" $ do
        (p1AfterEvaluate ^. victory) `shouldBe` (length (filter (== estateCard) (p1AfterEvaluate ^. played)))

    describe "DeckBuilding.Dominion.resetTurn" $ do
      it "has an empty played pile" $ do
        (length (p1AfterReset ^. played)) `shouldBe` 0

      it "has zero money" $ do
        (p1AfterReset ^. money) `shouldBe` 0

      it "has zero victory" $ do
        (p1AfterReset ^. victory) `shouldBe` 0

      it "has an empty hand" $ do
        (length (p1AfterReset ^. hand)) `shouldBe` 0

      it "has only one buy" $ do
        (p1AfterReset ^. buys) `shouldBe` 1

      it "has only one action" $ do
        (p1AfterReset ^. actions) `shouldBe` 1

    describe "DeckBuilding.Dominion.Cards.valueCard" $ do
      it "gives money for a copper" $ do
        let (p1AfterCard, afterCard) = runState ((copperCard ^. action) copperCard p1AfterDeal) afterDeal
        (p1AfterCard ^. money) `shouldBe` 1

      it "gives money for a silver" $ do
        let (p1AfterCard, afterCard) = runState ((silverCard ^. action) silverCard p1AfterDeal) afterDeal
        (p1AfterCard ^. money) `shouldBe` 2

      it "gives money for a gold" $ do
        let (p1AfterCard, afterCard) = runState ((goldCard ^. action) goldCard p1AfterDeal) afterDeal
        (p1AfterCard ^. money) `shouldBe` 3

      it "gives victory for an estate" $ do
        let (p1AfterCard, afterCard) = runState ((estateCard ^. action) estateCard p1AfterDeal) afterDeal
        (p1AfterCard ^. victory) `shouldBe` 1

      it "gives victory for a duchy" $ do
        let (p1AfterCard, afterCard) = runState ((duchyCard ^. action) duchyCard p1AfterDeal) afterDeal
        (p1AfterCard ^. victory) `shouldBe` 3

      it "gives victory for a province" $ do
        let (p1AfterCard, afterCard) = runState ((provinceCard ^. action) provinceCard p1AfterDeal) afterDeal
        (p1AfterCard  ^. victory) `shouldBe` 6

      it "takes victory for a curse" $ do
        let (p1AfterCard, afterCard) = runState ((curseCard ^. action) curseCard p1AfterDeal) afterDeal
        (p1AfterCard ^. victory) `shouldBe` (-1)

    describe "DeckBuilding.Dominion.Cards.basicCardAction" $ do
      it "it works with market" $ do
        let (p1AfterCard, afterCard) = runState ((marketCard ^. action) marketCard p1AfterDeal) afterDeal
        (length (p1AfterCard ^. hand)) `shouldBe` 6
        (p1AfterCard ^. actions) `shouldBe` 1
        (p1AfterCard ^. buys) `shouldBe` 2
        (p1AfterCard ^. money) `shouldBe` 1

      it "it works with moat" $ do
        let (p1AfterCard, afterCard) = runState ((moatCard ^. action) moatCard p1AfterDeal) afterDeal
        (length (p1AfterCard ^. hand)) `shouldBe` 7
        (p1AfterCard ^. actions) `shouldBe` 0

      it "it works with smithy" $ do
        let (p1AfterCard, afterCard) = runState ((smithyCard ^. action) smithyCard p1AfterDeal) afterDeal
        (length (p1AfterCard ^. hand)) `shouldBe` 8
        (p1AfterCard ^. actions) `shouldBe` 0

      it "it works with village" $ do
        let (p1AfterCard, afterCard) = runState ((villageCard ^. action) villageCard p1AfterDeal) afterDeal
        (length (p1AfterCard ^. hand)) `shouldBe` 6
        (p1AfterCard ^. actions) `shouldBe` 2

      it "it works with festival" $ do
        let (p1AfterCard, afterCard) = runState ((festivalCard ^. action) festivalCard p1AfterDeal) afterDeal
        (p1AfterCard ^. actions) `shouldBe` 2
        (p1AfterCard ^. buys) `shouldBe` 2
        (p1AfterCard ^. money) `shouldBe` 2

      it "it works with laboratory" $ do
        let (p1AfterCard, afterCard) = runState ((laboratoryCard ^. action) laboratoryCard p1AfterDeal) afterDeal
        (length (p1AfterCard ^. hand)) `shouldBe` 7
        (p1AfterCard ^. actions) `shouldBe` 1

      it "it works with woodcutter" $ do
        let (p1AfterCard, afterCard) = runState ((woodcutterCard ^. action) woodcutterCard p1AfterDeal) afterDeal
        (p1AfterCard ^. actions) `shouldBe` 1
        (p1AfterCard ^. buys) `shouldBe` 2
        (p1AfterCard ^. money) `shouldBe` 2

    describe "DeckBuilding.Dominion.Cards.cellarCardAction" $ do
      it "discards all starting cards" $ do
        let (p1AfterCard, afterCard) = runState ((cellarCard ^. action) cellarCard p1AfterDeal) afterDeal
        (length (p1AfterCard ^. hand)) `shouldBe` 5
        (length (p1AfterCard ^. discard)) `shouldBe` 5
        (length (p1AfterCard ^. played)) `shouldBe` 1

    describe "DeckBuilding.Dominion.Cards.chapelCardAction" $ do
      it "trashes 4 of the starting cards" $ do
        let (p1AfterCard, afterCard) = runState ((chapelCard ^. action) chapelCard p1AfterDeal) afterDeal
        (length (p1AfterCard ^. hand)) `shouldBe` 1
        (length (p1AfterCard ^. discard)) `shouldBe` 0
        (length (p1AfterCard ^. played)) `shouldBe` 1
        (length (p1AfterCard ^. played ++ p1AfterCard ^. discard ++ p1AfterCard ^. hand ++ p1AfterCard ^. deck)) `shouldBe` 7

    describe "DeckBuilding.Dominion.Cards.harbingerCardAction" $ do
      it "takes a silver from the discard pile and puts it on the deck" $ do
        let (p1BeforeCard, beforeCard)  = runState (updatePlayer (over discard (silverCard:) p1AfterDeal)) afterDeal
        let (p1AfterCard, afterCard)    = runState ((harbingerCard ^. action) harbingerCard p1BeforeCard) beforeCard
        (head (p1AfterCard ^. deck)) `shouldBe` silverCard
        (length (p1AfterCard ^. hand)) `shouldBe` 6
        (p1AfterCard ^. actions) `shouldBe` 1

    describe "DeckBuilding.Dominion.Cards.merchantCardAction" $ do
      it "adds one money if there is no silver played" $ do
        let (p1AfterCard, afterCard) = runState ((merchantCard ^. action) merchantCard p1AfterDeal) afterDeal
        (p1AfterCard ^. money) `shouldBe` 1
        (p1AfterCard ^. actions) `shouldBe` 1
      it "adds two money if there has been a silver played" $ do
        let (p1AfterSilver, afterSilver)  = runState ((silverCard ^. action) silverCard p1AfterDeal) afterDeal
        let (p1AfterCard, afterCard)      = runState ((merchantCard ^. action) merchantCard p1AfterSilver) afterSilver
        (p1AfterCard ^. money) `shouldBe` 4 -- 2 for silver, 2 for merchant
        (p1AfterCard ^. actions) `shouldBe` 1
      it "adds two money if a silver is played after" $ do
        let (p1AfterSilver, afterCard)  = runState ((merchantCard ^. action) merchantCard p1AfterDeal) afterDeal
        let (p1AfterCard, afterSilver)  = runState ((silverCard ^. action) silverCard p1AfterSilver) afterCard
        (p1AfterCard ^. money) `shouldBe` 4
        (p1AfterCard ^. actions) `shouldBe` 1

    describe "DeckBuilding.Dominion.Cards.vassalCardAction" $ do
      it "draws a value card" $ do
        let forcedDeal = Player "Forced Deal" (take 5 (repeat copperCard)) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0
        let (p1AfterCard, afterCard) = runState (evaluateHand forcedDeal) $ Game [forcedDeal] (basicDecks 2) g
        (p1AfterCard ^. money) `shouldBe` 4
        (length (p1AfterCard ^. hand)) `shouldBe` 0
        (length (p1AfterCard ^. played)) `shouldBe` 5
        (length (p1AfterCard ^. deck)) `shouldBe` 4
        (length (p1AfterCard ^. discard)) `shouldBe` 1

    describe "DeckBuilding.Dominion.Cards.bureaucratCardAction" $ do
      let (p1AfterCard, afterCard) = runState ((bureaucratCard ^. action) bureaucratCard p1AfterDeal) afterDeal2
      it "puts a silver on the deck" $ do
        head (p1AfterCard ^. deck) `shouldBe` silverCard
      it "makes other players discard a victory card" $ do
        let (Just p2') = find (== p2) (afterCard ^. players)
        (length (p2' ^. hand)) `shouldBe` 4

    describe "DeckBuilding.Dominion.Cards.gardensCardAction" $ do
      it "gives 1 point for the starting deck" $ do
        let (p1AfterCard, afterCard) = runState ((gardensCard ^. action) gardensCard p1AfterDeal) afterDeal
        (p1AfterCard ^. victory) `shouldBe` 1

    describe "DeckBuilding.Dominion.Cards.militiaCardAction" $ do
      let (p1AfterCard, afterCard) = runState ((militiaCard ^. action) militiaCard p1AfterDeal) afterDeal2
      it "gives two money" $ do
        (p1AfterCard ^. money) `shouldBe` 2
      it "makes other players discard down to three cards" $ do
        let (Just p2') = find (== p2) (afterCard ^. players)
        (length (p2' ^. hand)) `shouldBe` 3

    describe "DeckBuilding.Dominion.Cards.moneylenderCardAction" $ do
      let (p1AfterCard, afterCard) = runState ((moneylenderCard ^. action) moneylenderCard p1AfterDeal) afterDeal2
      it "gives 3 money" $ do
        (p1AfterCard ^. money) `shouldBe` 3
      it "trashes a copper" $ do
        length ((p1AfterCard ^. hand) ++ (p1AfterCard ^. discard) ++ (p1AfterCard ^. played) ++ (p1AfterCard ^. deck)) `shouldBe` 10 -- includes the moneylender card itself

    describe "DeckBuilding.Dominion.Cards.poacherCardAction" $ do
      it "gives a card, action, and money" $ do
        let (p1AfterCard, afterCard) = runState ((poacherCard ^. action) poacherCard p1AfterDeal) afterDeal
        length (p1AfterCard ^. hand) `shouldBe` 6
        (p1AfterCard ^. money) `shouldBe` 1
        (p1AfterCard ^. actions) `shouldBe` 1
      it "causes a discard per empty supply pile" $ do
        let preCard = set decks (Map.fromList [ (copperCard, 46), (silverCard, 0), (goldCard, 30), (estateCard, 0), (duchyCard, 8), (provinceCard, 8) ]) afterDeal
        let (p1AfterCard, afterCard) = runState ((poacherCard ^. action) poacherCard p1AfterDeal) preCard
        length (p1AfterCard ^. hand) `shouldBe` 4
        (p1AfterCard ^. money) `shouldBe` 1
        (p1AfterCard ^. actions) `shouldBe` 1
