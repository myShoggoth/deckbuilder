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
import           Control.Monad.RWS
import           Data.List
import qualified Data.Map                               as Map
import           System.Random
import           Test.Hspec
import qualified Test.QuickCheck                        as QC

spec :: Spec
spec = do
  let g = mkStdGen 45752345316
  let c = DominionConfig
              [ ("Player 1", bigMoneyStrategy)
              , ("Player 2", bigSmithyStrategy)
              ]
              firstGameKingdomCards
              1
              [g]
  let dg = configToGame c g
  let afterDeal     = fst $ execRWS (deal 5 0) c dg
  let afterDeal2    = fst $ execRWS (deal 5 1) c afterDeal
  let afterEvaluate = fst $ execRWS (evaluateHand 0) c afterDeal2
  describe "Utils.valueCard" $ do
    it "gives money for a copper" $ do
      let afterCard = fst $ execRWS ((copperCard ^. action) copperCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. money) `shouldBe` 1

    it "gives money for a silver" $ do
      let afterCard = fst $ execRWS ((silverCard ^. action) silverCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. money) `shouldBe` 2

    it "gives money for a gold" $ do
      let afterCard = fst $ execRWS ((goldCard ^. action) goldCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. money) `shouldBe` 3

    it "gives victory for an estate" $ do
      let afterCard = fst $ execRWS ((estateCard ^. action) estateCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. victory) `shouldBe` 1

    it "gives victory for a duchy" $ do
      let afterCard = fst $ execRWS ((duchyCard ^. action) duchyCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. victory) `shouldBe` 3

    it "gives victory for a province" $ do
      let afterCard = fst $ execRWS ((provinceCard ^. action) provinceCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard  ^. victory) `shouldBe` 6

    it "takes victory for a curse" $ do
      let afterCard = fst $ execRWS ((curseCard ^. action) curseCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. victory) `shouldBe` (-1)

  describe "Utils.basicCardAction" $ do
    it "it works with market" $ do
      let afterCard = fst $ execRWS ((marketCard ^. action) marketCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 6
      (p1AfterCard ^. actions) `shouldBe` 1
      (p1AfterCard ^. buys) `shouldBe` 2
      (p1AfterCard ^. money) `shouldBe` 1

    it "it works with moat" $ do
      let afterCard = fst $ execRWS ((moatCard ^. action) moatCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 0

    it "it works with smithy" $ do
      let afterCard = fst $ execRWS ((smithyCard ^. action) smithyCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 8
      (p1AfterCard ^. actions) `shouldBe` 0

    it "it works with village" $ do
      let afterCard = fst $ execRWS ((villageCard ^. action) villageCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 6
      (p1AfterCard ^. actions) `shouldBe` 2

    it "it works with festival" $ do
      let afterCard = fst $ execRWS ((festivalCard ^. action) festivalCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. actions) `shouldBe` 2
      (p1AfterCard ^. buys) `shouldBe` 2
      (p1AfterCard ^. money) `shouldBe` 2

    it "it works with laboratory" $ do
      let afterCard = fst $ execRWS ((laboratoryCard ^. action) laboratoryCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 1
