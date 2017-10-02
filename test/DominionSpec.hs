module DominionSpec
    ( spec
    ) where

import           Control.Exception                      (evaluate)
import           Control.Lens
import           Control.Monad.State
import           Data.List
import qualified Data.Map                               as Map
import           DeckBuilding.Dominion
import           DeckBuilding.Dominion.Cards.Base
import           DeckBuilding.Dominion.Strategies.Basic
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils
import           System.Random
import           Test.Hspec
import qualified Test.QuickCheck                        as QC

spec :: Spec
spec = do
  let g = mkStdGen 45752345316
  let p1                                = newPlayer "Player 1" bigMoneyStrategy
  let p2                                = newPlayer "Player 2" bigSmithyStrategy
  let (p1AfterDeal, afterDeal)          = runState (deal 5 p1) $ Game [p1, p2] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
  let (p2AfterDeal, afterDeal2)         = runState (deal 5 p2) afterDeal
  let (p1AfterEvaluate, afterEvaluate)  = runState (evaluateHand p1AfterDeal) afterDeal2
  let (p1AfterReset, afterReset)        = runState (resetTurn p1) afterEvaluate

  describe "Utils.deal" $ do
    it "deals the correct number of cards" $ do
      length (p1AfterDeal  ^. hand) `shouldBe` 5
      length (p1AfterDeal ^. deck) `shouldBe` 5
      length (p1AfterDeal ^. discard) `shouldBe` 0

    it "has a total of seven copper" $
      length (filter (== copperCard) ((p1AfterDeal ^. hand) ++ (p1AfterDeal ^. deck))) `shouldBe` 7

    it "has a total of three estates" $
      length (filter (== estateCard) ((p1AfterDeal ^. hand) ++ (p1AfterDeal ^. deck))) `shouldBe` 3

  describe "evaluateHand" $ do
    it "has no more cards in hand" $ do
      length (p1AfterEvaluate ^. played) `shouldBe` 5
      length (p1AfterEvaluate ^. hand) `shouldBe` 0
      length (p1AfterDeal ^. discard) `shouldBe` 0

    it "calculates the right amount of money" $
      p1AfterEvaluate ^. money `shouldBe` length (filter (== copperCard) (p1AfterEvaluate ^. played))

    it "calcualtes the right amount of victory" $
      p1AfterEvaluate ^. victory `shouldBe` length (filter (== estateCard) (p1AfterEvaluate ^. played))

  describe "resetTurn" $ do
    it "has an empty played pile" $
      length (p1AfterReset ^. played) `shouldBe` 0

    it "has zero money" $
      (p1AfterReset ^. money) `shouldBe` 0

    it "has zero victory" $
      (p1AfterReset ^. victory) `shouldBe` 0

    it "has an empty hand" $
      length (p1AfterReset ^. hand) `shouldBe` 0

    it "has only one buy" $
      (p1AfterReset ^. buys) `shouldBe` 1

    it "has only one action" $
      (p1AfterReset ^. actions) `shouldBe` 1
