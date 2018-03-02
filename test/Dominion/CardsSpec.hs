module Dominion.CardsSpec
    ( spec
    ) where

import           DeckBuilding.Dominion
import           DeckBuilding.Dominion.Cards
import           DeckBuilding.Dominion.Strategies.Basic
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils

import           Control.Exception                      (evaluate)
import           Control.Lens
import           Control.Monad.State
import           Data.List
import qualified Data.Map                               as Map
import           System.Random
import           Test.Hspec
import qualified Test.QuickCheck                        as QC

spec :: Spec
spec = do
  let g = mkStdGen 45752345316
  let p1            = newPlayer "Player 1" bigMoneyStrategy
  let p2            = newPlayer "Player 2" bigSmithyStrategy
  let afterDeal     = execState (deal 5 0) $ DominionGame [p1, p2] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
  let afterDeal2    = execState (deal 5 1) afterDeal
  let afterEvaluate = execState (evaluateHand 0) afterDeal2
  let afterReset    = execState (resetTurn 0) afterEvaluate
  describe "Utils.valueCard" $ do
    it "gives money for a copper" $ do
      let afterCard = execState ((copperCard ^. action) copperCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. money) `shouldBe` 1

    it "gives money for a silver" $ do
      let afterCard = execState ((silverCard ^. action) silverCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. money) `shouldBe` 2

    it "gives money for a gold" $ do
      let afterCard = execState ((goldCard ^. action) goldCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. money) `shouldBe` 3

    it "gives victory for an estate" $ do
      let afterCard = execState ((estateCard ^. action) estateCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. victory) `shouldBe` 1

    it "gives victory for a duchy" $ do
      let afterCard = execState ((duchyCard ^. action) duchyCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. victory) `shouldBe` 3

    it "gives victory for a province" $ do
      let afterCard = execState ((provinceCard ^. action) provinceCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard  ^. victory) `shouldBe` 6

    it "takes victory for a curse" $ do
      let afterCard = execState ((curseCard ^. action) curseCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. victory) `shouldBe` (-1)

  describe "Utils.basicCardAction" $ do
    it "it works with market" $ do
      let afterCard = execState ((marketCard ^. action) marketCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 6
      (p1AfterCard ^. actions) `shouldBe` 1
      (p1AfterCard ^. buys) `shouldBe` 2
      (p1AfterCard ^. money) `shouldBe` 1

    it "it works with moat" $ do
      let afterCard = execState ((moatCard ^. action) moatCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 0

    it "it works with smithy" $ do
      let afterCard = execState ((smithyCard ^. action) smithyCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 8
      (p1AfterCard ^. actions) `shouldBe` 0

    it "it works with village" $ do
      let afterCard = execState ((villageCard ^. action) villageCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 6
      (p1AfterCard ^. actions) `shouldBe` 2

    it "it works with festival" $ do
      let afterCard = execState ((festivalCard ^. action) festivalCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. actions) `shouldBe` 2
      (p1AfterCard ^. buys) `shouldBe` 2
      (p1AfterCard ^. money) `shouldBe` 2

    it "it works with laboratory" $ do
      let afterCard = execState ((laboratoryCard ^. action) laboratoryCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 1
