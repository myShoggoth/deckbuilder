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
    let p1 = newPlayer "Player 1"
    let gs'       = deal 5 p1 (GameState [p1] (basicDecks 2) g)
    let Just p1'  = find (== p1) (_players gs')
    let gs'' = evaluateHand p1' gs'
    let Just p1'' = find (== p1) (_players gs'')
    let gs''' = resetTurn p1 gs''
    let Just p1''' = find (== p1) (_players gs''')

    describe "DeckBuilding.Dominion.Cards.deal" $ do
      it "deals the correct number of cards" $ do
        (length (_hand p1')) `shouldBe` 5

      it "has the right number of cards still in deck" $ do
        (length (_deck p1')) `shouldBe` 5

      it "has an empty discard pile" $ do
        (length (_discard p1')) `shouldBe` 0

      it "has a total of seven copper" $ do
        (length (filter (== copperCard) ((_hand p1') ++ (_deck p1')))) `shouldBe` 7

      it "has a total of three estates" $ do
        (length (filter (== estateCard) ((_hand p1') ++ (_deck p1')))) `shouldBe` 3

    describe "DeckBuilding.Dominion.evaluateHand" $ do
      it "has no more cards in hand" $ do
        (length (_played p1'')) `shouldBe` 5

      it "calculates the right amount of money" $ do
        (_money p1'') `shouldBe` (length (filter (== copperCard) (_played p1'')))

      it "calcualtes the right amount of victory" $ do
        (_victory p1'') `shouldBe` (length (filter (== estateCard) (_played p1'')))

    describe "DeckBuilding.Dominion.resetTurn" $ do
      it "has an empty played pile" $ do
        (length (_played p1''')) `shouldBe` 0

      it "has zero money" $ do
        (_money p1''') `shouldBe` 0

      it "has zero victory" $ do
        (_victory p1''') `shouldBe` 0

      it "has an empty hand" $ do
        (length (_hand p1''')) `shouldBe` 0

      it "has only one buy" $ do
        (_buys p1''') `shouldBe` 1

      it "has only one action" $ do
        (_actions p1''') `shouldBe` 1

    describe "DeckBuilding.Dominion.Cards.valueCard" $ do
      it "gives money for a copper" $ do
        let gs'''' = (_action copperCard) copperCard p1' gs'
        let Just p1'''' = find (== p1) (_players gs'''')
        (_money p1'''') `shouldBe` 1

      it "gives victory for an estate" $ do
        let gs'''' = (_action estateCard) estateCard p1' gs'
        let Just p1'''' = find (== p1) (_players gs'''')
        (_victory p1'''') `shouldBe` 1

-- Dominion Game testing functions

testDominionCard :: IO (GameState)
testDominionCard = do
  let a = newPlayer "Player 1"
  g <- newStdGen
  let afterDeal = deal 5 a (GameState [a] (basicDecks 2) g)
  let Just newA = find (== a) (_players afterDeal)
  let firstCard = head (_hand newA)
  return $ (_action firstCard) firstCard newA afterDeal

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
  let gs' = foldr (deal 5) gs players
  return $ foldr (doTurn False) gs' players

testDominionTurn :: IO (GameState)
testDominionTurn = do
  g <- newStdGen
  let players = [ newPlayer "Player 1", newPlayer "Player 2"]
  let gs = (GameState players (basicDecks (length players)) g)
  let gs' = foldr (deal 5) gs players
  return $ doTurn False (head players) gs'

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
