module Dominion.CardsSpec
    ( spec
    ) where

import DeckBuilding.Dominion
import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Utils
import DeckBuilding.Dominion.Strategies.Basic
import DeckBuilding.Dominion.Cards

import System.Random
import Data.List
import Test.Hspec
import qualified Test.QuickCheck as QC
import Control.Exception (evaluate)
import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map

spec :: Spec
spec = do
  let g = mkStdGen 45752345316
  let p1                                = newPlayer "Player 1" bigMoneyStrategy
  let p2                                = newPlayer "Player 2" bigSmithyStrategy
  let (p1AfterDeal, afterDeal)          = runState (deal 5 p1) $ Game [p1, p2] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
  let (p2AfterDeal, afterDeal2)         = runState (deal 5 p2) afterDeal
  let (p1AfterEvaluate, afterEvaluate)  = runState (evaluateHand p1AfterDeal) afterDeal2
  let (p1AfterReset, afterReset)        = runState (resetTurn p1) afterEvaluate
  describe "Utils.valueCard" $ do
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

  describe "Utils.basicCardAction" $ do
    it "it works with market" $ do
      let (p1AfterCard, afterCard) = runState ((marketCard ^. action) marketCard p1AfterDeal) afterDeal
      length (p1AfterCard ^. hand) `shouldBe` 6
      (p1AfterCard ^. actions) `shouldBe` 1
      (p1AfterCard ^. buys) `shouldBe` 2
      (p1AfterCard ^. money) `shouldBe` 1

    it "it works with moat" $ do
      let (p1AfterCard, afterCard) = runState ((moatCard ^. action) moatCard p1AfterDeal) afterDeal
      length (p1AfterCard ^. hand) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 0

    it "it works with smithy" $ do
      let (p1AfterCard, afterCard) = runState ((smithyCard ^. action) smithyCard p1AfterDeal) afterDeal
      length (p1AfterCard ^. hand) `shouldBe` 8
      (p1AfterCard ^. actions) `shouldBe` 0

    it "it works with village" $ do
      let (p1AfterCard, afterCard) = runState ((villageCard ^. action) villageCard p1AfterDeal) afterDeal
      length (p1AfterCard ^. hand) `shouldBe` 6
      (p1AfterCard ^. actions) `shouldBe` 2

    it "it works with festival" $ do
      let (p1AfterCard, afterCard) = runState ((festivalCard ^. action) festivalCard p1AfterDeal) afterDeal
      (p1AfterCard ^. actions) `shouldBe` 2
      (p1AfterCard ^. buys) `shouldBe` 2
      (p1AfterCard ^. money) `shouldBe` 2

    it "it works with laboratory" $ do
      let (p1AfterCard, afterCard) = runState ((laboratoryCard ^. action) laboratoryCard p1AfterDeal) afterDeal
      length (p1AfterCard ^. hand) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 1
