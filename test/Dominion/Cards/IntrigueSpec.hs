module Dominion.Cards.IntrigueSpec
    ( spec
    ) where

import DeckBuilding.Dominion
import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Utils
import DeckBuilding.Dominion.Strategies.Basic
import DeckBuilding.Dominion.Cards
import DeckBuilding.Dominion.Cards.Utils

import System.Random
import Data.List
import Test.Hspec
import qualified Test.QuickCheck as QC
import Control.Exception (evaluate)
import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map

gainAction :: Int -> Player -> State Game Player
gainAction = gainCard firstGameKingdomCards

gainVictory :: Int -> Player -> State Game Player
gainVictory = gainCard (delete curseCard victoryCards)

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

  describe "conspiratorCardAction" $ do
    let (p1AfterCard, afterCard) = runState ((conspiratorCard ^. action) conspiratorCard p1AfterDeal) afterDeal
    it "gets only two money when fewer than two actions have been played" $ do
      length (p1AfterCard ^. hand) `shouldBe` 5
      (p1AfterCard ^. actions) `shouldBe` 0
      (p1AfterCard ^. money) `shouldBe` 2
    it "gets one card, one action, and two cards when two actions have been played" $ do
      let p1Prepped = over played ([conspiratorCard, conspiratorCard] ++) p1
      let (p1BeforeConspirator, startConspirator) = runState (deal 5 p1Prepped) $ Game [p1Prepped, p2] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      let (p1AfterConspirator, afterConspirator) = runState ((conspiratorCard ^. action) conspiratorCard p1BeforeConspirator) startConspirator
      length (p1AfterConspirator ^. hand) `shouldBe` 6
      (p1AfterConspirator ^. actions) `shouldBe` 1
      (p1AfterConspirator ^. money) `shouldBe` 2

  describe "ironworksCardAction" $ do
    it "gets +action for an action card" $ do
      let forcedDeal = Player "Bureaurat Deal" (replicate 5 copperCard) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 $ Strategy "Ironworks Action" bigSmithyBuy bigMoneyDiscard bigMoneyTrash bigMoneyRetrieve bigMoneyOrderHand gainAction bigMoneyThroneRoom bigMoneyLibrary bigMoneySentry bigMoneyHandToDeck bigMoneyLurker
      let (p2AfterCard, afterCard) = runState ((ironworksCard ^. action) ironworksCard forcedDeal) $ Game [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      (p2AfterCard ^. actions) `shouldBe` 1
      (p2AfterCard ^. money) `shouldBe` 0
      length (p2AfterCard ^. hand) `shouldBe` 5
    it "gets +money for a treasure card" $ do
      let forcedDeal = Player "Bureaurat Deal" (replicate 5 copperCard) [] [copperCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigMoneyStrategy
      let (p2AfterCard, afterCard) = runState ((ironworksCard ^. action) ironworksCard forcedDeal) $ Game [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      (p2AfterCard ^. actions) `shouldBe` 0
      (p2AfterCard ^. money) `shouldBe` 1
      length (p2AfterCard ^. hand) `shouldBe` 5
    it "gets +card for a victory card" $ do
      let forcedDeal = Player "Bureaurat Deal" (replicate 5 copperCard) [] [estateCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 $ Strategy "Ironworks Victory" bigSmithyBuy bigMoneyDiscard bigMoneyTrash bigMoneyRetrieve bigMoneyOrderHand gainVictory bigMoneyThroneRoom bigMoneyLibrary bigMoneySentry bigMoneyHandToDeck bigMoneyLurker
      let (p2AfterCard, afterCard) = runState ((ironworksCard ^. action) ironworksCard forcedDeal) $ Game [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      (p2AfterCard ^. actions) `shouldBe` 0
      (p2AfterCard ^. money) `shouldBe` 0
      length (p2AfterCard ^. hand) `shouldBe` 6
