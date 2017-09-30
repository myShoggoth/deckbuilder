module Dominion.Cards.IntrigueSpec
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

  describe "courtyardCardAction" $ do
    let (p1AfterCard, afterCard) = runState ((courtyardCard ^. action) courtyardCard p1AfterDeal) afterDeal
    it "draws three cards and puts one back on the deck" $ do
      length (p1AfterCard ^. deck) `shouldBe` 3
      length (p1AfterCard ^. hand) `shouldBe` 7

  describe "lurkerCardAction" $ do
    let (p1AfterCard, afterCard) = runState ((lurkerCard ^. action) lurkerCard p1AfterDeal) afterDeal
    it "trashes an action card from supply" $ do
      length (afterCard ^. trash) `shouldBe` 1
      head (afterCard ^. trash) ^. cardType `shouldBe` Action

  describe "shantyTownCardAction" $ do
    let (p1AfterCard, afterCard) = runState ((shantyTownCard ^. action) shantyTownCard p1AfterDeal) afterDeal
    it "gets two cards and two actions with no action cards in hand" $ do
      length (p1AfterCard ^. hand) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 2
