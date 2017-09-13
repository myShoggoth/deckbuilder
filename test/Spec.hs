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

main :: IO ()
main = do
  g <- newStdGen
  hspec $ do
    let p1                    = newPlayer "Player 1"
    let afterDeal             = deal 5 p1 (GameState [p1] (basicDecks 2) g)
    let Just p1AfterDeal      = find (== p1) (afterDeal ^. players)
    let afterEvaluate         = evaluateHand p1AfterDeal afterDeal
    let Just p1AfterEvaluate  = find (== p1) (afterEvaluate ^. players)
    let afterReset            = resetTurn p1 afterEvaluate
    let Just p1AfterReset     = find (== p1) (afterReset ^. players)

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
        let afterCard = (copperCard ^. action) copperCard p1AfterDeal afterDeal
        let Just p1AfterDeal = find (== p1) (afterCard ^. players)
        (p1AfterDeal ^. money) `shouldBe` 1

      it "gives money for a silver" $ do
        let afterCard = (silverCard ^. action) silverCard p1AfterDeal afterDeal
        let Just p1AfterDeal = find (== p1) (afterCard ^. players)
        (p1AfterDeal ^. money) `shouldBe` 2

      it "gives money for a gold" $ do
        let afterCard = (goldCard ^. action) goldCard p1AfterDeal afterDeal
        let Just p1AfterDeal = find (== p1) (afterCard ^. players)
        (p1AfterDeal ^. money) `shouldBe` 3

      it "gives victory for an estate" $ do
        let afterCard = (estateCard ^. action) estateCard p1AfterDeal afterDeal
        let Just p1AfterDeal = find (== p1) (afterCard ^. players)
        (p1AfterDeal ^. victory) `shouldBe` 1

      it "gives victory for a duchy" $ do
        let afterCard = (duchyCard ^. action) duchyCard p1AfterDeal afterDeal
        let Just p1AfterDeal = find (== p1) (afterCard ^. players)
        (p1AfterDeal ^. victory) `shouldBe` 3

      it "gives victory for a province" $ do
        let afterCard = (provinceCard ^. action) provinceCard p1AfterDeal afterDeal
        let Just p1AfterDeal = find (== p1) (afterCard ^. players)
        (p1AfterDeal  ^. victory) `shouldBe` 6

      it "takes victory for a curse" $ do
        let afterCard = (curseCard ^. action) curseCard p1AfterDeal afterDeal
        let Just p1AfterDeal = find (== p1) (afterCard ^. players)
        (p1AfterDeal ^. victory) `shouldBe` (-1)

    describe "DeckBuilding.Dominion.Cards.basicCardAction" $ do
      it "it works with market" $ do
        let afterCard = (marketCard ^. action) marketCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (afterCard ^. players)
        (length (p1AfterCard ^. hand)) `shouldBe` 6
        (p1AfterCard ^. actions) `shouldBe` 1
        (p1AfterCard ^. buys) `shouldBe` 2
        (p1AfterCard ^. money) `shouldBe` 1

      it "it works with moat" $ do
        let afterCard = (moatCard ^. action) moatCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (afterCard ^. players)
        (length (p1AfterCard ^. hand)) `shouldBe` 7
        (p1AfterCard ^. actions) `shouldBe` 0

      it "it works with smithy" $ do
        let afterCard = (smithyCard ^. action) smithyCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (afterCard ^. players)
        (length (p1AfterCard ^. hand)) `shouldBe` 8
        (p1AfterCard ^. actions) `shouldBe` 0

      it "it works with village" $ do
        let afterCard = (villageCard ^. action) villageCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (afterCard ^. players)
        (length (p1AfterCard ^. hand)) `shouldBe` 6
        (p1AfterCard ^. actions) `shouldBe` 2

      it "it works with festival" $ do
        let afterCard = (festivalCard ^. action) festivalCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (afterCard ^. players)
        (p1AfterCard ^. actions) `shouldBe` 2
        (p1AfterCard ^. buys) `shouldBe` 2
        (p1AfterCard ^. money) `shouldBe` 2

      it "it works with laboratory" $ do
        let afterCard = (laboratoryCard ^. action) laboratoryCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (afterCard ^. players)
        (length (p1AfterCard ^. hand)) `shouldBe` 7
        (p1AfterCard ^. actions) `shouldBe` 1

      it "it works with woodcutter" $ do
        let afterCard = (woodcutterCard ^. action) woodcutterCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (afterCard ^. players)
        (p1AfterCard ^. actions) `shouldBe` 1
        (p1AfterCard ^. buys) `shouldBe` 2
        (p1AfterCard ^. money) `shouldBe` 2

    describe "DeckBuilding.Dominion.Cards.cellarCardAction" $ do
      it "discards all starting cards" $ do
        let afterCard = (cellarCard ^. action) cellarCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (afterCard ^. players)
        (length (p1AfterCard ^. hand)) `shouldBe` 5
        (length (p1AfterCard ^. discard)) `shouldBe` 5
        (length (p1AfterCard ^. played)) `shouldBe` 1

    describe "DeckBuilding.Dominion.Cards.chapelCardAction" $ do
      it "trashes 4 of the starting cards" $ do
        let afterCard = (chapelCard ^. action) chapelCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (afterCard ^. players)
        (length (p1AfterCard ^. hand)) `shouldBe` 1
        (length (p1AfterCard ^. discard)) `shouldBe` 0
        (length (p1AfterCard ^. played)) `shouldBe` 1
        (length (p1AfterCard ^. played ++ p1AfterCard ^. discard ++ p1AfterCard ^. hand ++ p1AfterCard ^. deck)) `shouldBe` 7

    describe "DeckBuilding.Dominion.Cards.harbingerCardAction" $ do
      it "takes a silver from the discard pile and puts it on the deck" $ do
        let beforeCard = changeTurn (over discard (silverCard:) p1AfterDeal) afterDeal
        let Just p1BeforeCard = find (== p1) (beforeCard ^. players)
        let afterCard = (harbingerCard ^. action) harbingerCard p1BeforeCard beforeCard
        let Just p1AfterCard = find (== p1) (afterCard ^. players)
        (head (p1AfterCard ^. deck)) `shouldBe` silverCard
        (length (p1AfterCard ^. hand)) `shouldBe` 6
        (p1AfterCard ^. actions) `shouldBe` 1

    describe "DeckBuilding.Dominion.Cards.merchantCardAction" $ do
      it "adds one money if there is no silver played" $ do
        let afterCard = (merchantCard ^. action) merchantCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (afterCard ^. players)
        (p1AfterCard ^. money) `shouldBe` 1
        (p1AfterCard ^. actions) `shouldBe` 1
      it "adds two money if there has been a silver played" $ do
        let afterSilver = (silverCard ^. action) silverCard p1AfterDeal afterDeal
        let afterCard = (merchantCard ^. action) merchantCard p1AfterDeal afterSilver
        let Just p1AfterCard = find (== p1) (afterCard ^. players)
        (p1AfterCard ^. money) `shouldBe` 4 -- 2 for silver, 2 for merchant
        (p1AfterCard ^. actions) `shouldBe` 1
      it "adds two money if a silver is played after" $ do
        let afterCard = (merchantCard ^. action) merchantCard p1AfterDeal afterDeal
        let afterSilver = (silverCard ^. action) silverCard p1AfterDeal afterCard
        let Just p1AfterCard = find (== p1) (afterSilver ^. players)
        (p1AfterCard ^. money) `shouldBe` 4
        (p1AfterCard ^. actions) `shouldBe` 1

    describe "DeckBuilding.Dominion.Cards.vassalCardAction" $ do
      it "draws a value card" $ do
        let forcedDeal = Player (p1 ^. playerName) [vassalCard, estateCard, estateCard, copperCard, copperCard] [] (take 5 (repeat copperCard)) [] 1 1 0 0
        let afterForcedDeal = (GameState [forcedDeal] (basicDecks 2) g)
        let afterCard = evaluateHand forcedDeal afterForcedDeal
        let Just p1AfterCard = find (== p1) (afterCard ^. players)
        (p1AfterCard ^. money) `shouldBe` 5
        (length (p1AfterCard ^. hand)) `shouldBe` 0
        (length (p1AfterCard ^. played)) `shouldBe` 5
        (length (p1AfterCard ^. deck)) `shouldBe` 4
        (length (p1AfterCard ^. discard)) `shouldBe` 1

-- Dominion Game testing functions

testDominionBuy :: IO (GameState)
testDominionBuy = do
  let a = newPlayer "Player 1"
  g <- newStdGen
  return $ ( (bigMoneyBuy a) . (evaluateHand a) . (deal 5 a) ) (GameState [a] (basicDecks 2) g)

testDominionRound :: IO (GameState)
testDominionRound = do
  g <- newStdGen
  let players = [ newPlayer "Player 1", newPlayer "Player 2"]
  let gs = (GameState players (basicDecks (length players)) g)
  let afterDeal = foldr (deal 5) gs players
  return $ foldr (doTurn False) afterDeal players

testDominionTurn :: IO (GameState)
testDominionTurn = do
  g <- newStdGen
  let players = [ newPlayer "Player 1", newPlayer "Player 2"]
  let gs = (GameState players (basicDecks (length players)) g)
  let afterDeal = foldr (deal 5) gs players
  return $ doTurn False (head players) afterDeal

testDominionDeals :: IO (GameState)
testDominionDeals = do
  g <- newStdGen
  let players = [ newPlayer "Player 1", newPlayer "Player 2"]
  let gs = (GameState players (basicDecks (length players)) g)
  return $ foldr (deal 5) gs players

testDominionGame :: IO (Result)
testDominionGame = runGame [newPlayer "Player 1", newPlayer "Player 2"]

testDominionGames :: IO ([(Result, Int)])
testDominionGames = runGames 1000 [newPlayer "Player 1", newPlayer "Player 2"]
