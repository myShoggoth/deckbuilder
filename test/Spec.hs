import DeckBuilding.Dominion
import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Cards
import DeckBuilding.Dominion.Utils

import System.Random
import Data.List
import Test.Hspec
import qualified Test.QuickCheck as QC
import Control.Exception (evaluate)

main :: IO ()
main = do
  g <- newStdGen
  hspec $ do
    let p1                    = newPlayer "Player 1"
    let afterDeal             = deal 5 p1 (GameState [p1] (basicDecks 2) g)
    let Just p1AfterDeal      = find (== p1) (_players afterDeal)
    let afterEvaluate         = evaluateHand p1AfterDeal afterDeal
    let Just p1AfterEvaluate  = find (== p1) (_players afterEvaluate)
    let afterReset            = resetTurn p1 afterEvaluate
    let Just p1AfterReset     = find (== p1) (_players afterReset)

    describe "DeckBuilding.Dominion.Cards.deal" $ do
      it "deals the correct number of cards" $ do
        (length (_hand p1AfterDeal)) `shouldBe` 5
        (length (_deck p1AfterDeal)) `shouldBe` 5
        (length (_discard p1AfterDeal)) `shouldBe` 0

      it "has a total of seven copper" $ do
        (length (filter (== copperCard) ((_hand p1AfterDeal) ++ (_deck p1AfterDeal)))) `shouldBe` 7

      it "has a total of three estates" $ do
        (length (filter (== estateCard) ((_hand p1AfterDeal) ++ (_deck p1AfterDeal)))) `shouldBe` 3

    describe "DeckBuilding.Dominion.evaluateHand" $ do
      it "has no more cards in hand" $ do
        (length (_played p1AfterEvaluate)) `shouldBe` 5
        (length (_hand p1AfterEvaluate)) `shouldBe` 0
        (length (_discard p1AfterDeal)) `shouldBe` 0

      it "calculates the right amount of money" $ do
        (_money p1AfterEvaluate) `shouldBe` (length (filter (== copperCard) (_played p1AfterEvaluate)))

      it "calcualtes the right amount of victory" $ do
        (_victory p1AfterEvaluate) `shouldBe` (length (filter (== estateCard) (_played p1AfterEvaluate)))

    describe "DeckBuilding.Dominion.resetTurn" $ do
      it "has an empty played pile" $ do
        (length (_played p1AfterReset)) `shouldBe` 0

      it "has zero money" $ do
        (_money p1AfterReset) `shouldBe` 0

      it "has zero victory" $ do
        (_victory p1AfterReset) `shouldBe` 0

      it "has an empty hand" $ do
        (length (_hand p1AfterReset)) `shouldBe` 0

      it "has only one buy" $ do
        (_buys p1AfterReset) `shouldBe` 1

      it "has only one action" $ do
        (_actions p1AfterReset) `shouldBe` 1

    describe "DeckBuilding.Dominion.Cards.valueCard" $ do
      it "gives money for a copper" $ do
        let afterCard = (_action copperCard) copperCard p1AfterDeal afterDeal
        let Just p1AfterDeal = find (== p1) (_players afterCard)
        (_money p1AfterDeal) `shouldBe` 1

      it "gives money for a silver" $ do
        let afterCard = (_action silverCard) silverCard p1AfterDeal afterDeal
        let Just p1AfterDeal = find (== p1) (_players afterCard)
        (_money p1AfterDeal) `shouldBe` 2

      it "gives money for a gold" $ do
        let afterCard = (_action goldCard) goldCard p1AfterDeal afterDeal
        let Just p1AfterDeal = find (== p1) (_players afterCard)
        (_money p1AfterDeal) `shouldBe` 3

      it "gives victory for an estate" $ do
        let afterCard = (_action estateCard) estateCard p1AfterDeal afterDeal
        let Just p1AfterDeal = find (== p1) (_players afterCard)
        (_victory p1AfterDeal) `shouldBe` 1

      it "gives victory for a duchy" $ do
        let afterCard = (_action duchyCard) duchyCard p1AfterDeal afterDeal
        let Just p1AfterDeal = find (== p1) (_players afterCard)
        (_victory p1AfterDeal) `shouldBe` 3

      it "gives victory for a province" $ do
        let afterCard = (_action provinceCard) provinceCard p1AfterDeal afterDeal
        let Just p1AfterDeal = find (== p1) (_players afterCard)
        (_victory p1AfterDeal) `shouldBe` 6

      it "takes victory for a curse" $ do
        let afterCard = (_action curseCard) curseCard p1AfterDeal afterDeal
        let Just p1AfterDeal = find (== p1) (_players afterCard)
        (_victory p1AfterDeal) `shouldBe` (-1)

    describe "DeckBuilding.Dominion.Cards.basicCardAction" $ do
      it "it works with market" $ do
        let afterCard = (_action marketCard) marketCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (_players afterCard)
        (length (_hand p1AfterCard)) `shouldBe` 6
        (_actions p1AfterCard) `shouldBe` 1
        (_buys p1AfterCard) `shouldBe` 2
        (_money p1AfterCard) `shouldBe` 1

      it "it works with moat" $ do
        let afterCard = (_action moatCard) moatCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (_players afterCard)
        (length (_hand p1AfterCard)) `shouldBe` 7
        (_actions p1AfterCard) `shouldBe` 0

      it "it works with smithy" $ do
        let afterCard = (_action smithyCard) smithyCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (_players afterCard)
        (length (_hand p1AfterCard)) `shouldBe` 8
        (_actions p1AfterCard) `shouldBe` 0

      it "it works with village" $ do
        let afterCard = (_action villageCard) villageCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (_players afterCard)
        (length (_hand p1AfterCard)) `shouldBe` 6
        (_actions p1AfterCard) `shouldBe` 2

      it "it works with festival" $ do
        let afterCard = (_action festivalCard) festivalCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (_players afterCard)
        (_actions p1AfterCard) `shouldBe` 2
        (_buys p1AfterCard) `shouldBe` 2
        (_money p1AfterCard) `shouldBe` 2

      it "it works with laboratory" $ do
        let afterCard = (_action laboratoryCard) laboratoryCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (_players afterCard)
        (length (_hand p1AfterCard)) `shouldBe` 7
        (_actions p1AfterCard) `shouldBe` 1

      it "it works with woodcutter" $ do
        let afterCard = (_action woodcutterCard) woodcutterCard p1AfterDeal afterDeal
        let Just p1AfterCard = find (== p1) (_players afterCard)
        (_actions p1AfterCard) `shouldBe` 1
        (_buys p1AfterCard) `shouldBe` 2
        (_money p1AfterCard) `shouldBe` 2

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
