module Dominion.Cards.IntrigueSpec
    ( spec
    ) where

import           DeckBuilding.Dominion
import           DeckBuilding.Dominion.Cards
import           DeckBuilding.Dominion.Cards.Utils
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

gainAction :: Int -> Int -> State DominionGame (Maybe Card)
gainAction = gainCard firstGameKingdomCards

gainVictory :: Int -> Int -> State DominionGame (Maybe Card)
gainVictory = gainCard (delete curseCard victoryCards)

spec :: Spec
spec = do
  let g = mkStdGen 45752345316
  let p1                                = newPlayer "Player 1" bigMoneyStrategy
  let p2                                = newPlayer "Player 2" bigSmithyStrategy
  let afterDeal           = execState (deal 5 0) $ DominionGame [p1, p2] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
  let (Just p1AfterDeal)  = afterDeal ^? players . ix 0
  let afterDeal2          = execState (deal 5 1) afterDeal
  let afterEvaluate       = execState (evaluateHand 0) afterDeal2
  let afterReset          = execState (resetTurn 0) afterEvaluate

  describe "courtyardCardAction" $ do
    let afterCard = execState ((courtyardCard ^. action) courtyardCard 0) afterDeal
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "draws three cards and puts one back on the deck" $ do
      length (p1AfterCard ^. deck) `shouldBe` 3
      length (p1AfterCard ^. hand) `shouldBe` 7

  describe "lurkerCardAction" $ do
    let afterCard = execState ((lurkerCard ^. action) lurkerCard 0) afterDeal
    let (Just p1AfterDeal) = afterCard ^? players . ix 0
    it "trashes an action card from supply" $ do
      length (afterCard ^. trash) `shouldBe` 1
      head (afterCard ^. trash) ^. cardType `shouldBe` Action

  describe "shantyTownCardAction" $ do
    let afterCard = execState ((shantyTownCard ^. action) shantyTownCard 0) afterDeal
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "gets two cards and two actions with no action cards in hand" $ do
      length (p1AfterCard ^. hand) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 2

  describe "conspiratorCardAction" $ do
    let afterCard = execState ((conspiratorCard ^. action) conspiratorCard 0) afterDeal
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "gets only two money when fewer than two actions have been played" $ do
      length (p1AfterCard ^. hand) `shouldBe` 5
      (p1AfterCard ^. actions) `shouldBe` 0
      (p1AfterCard ^. money) `shouldBe` 2
    it "gets one card, one action, and two cards when two actions have been played" $ do
      let p1Prepped = over played ([conspiratorCard, conspiratorCard] ++) p1
      let startConspirator = execState (deal 5 0) $ DominionGame [p1Prepped, p2] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      let afterConspirator = execState ((conspiratorCard ^. action) conspiratorCard 0) startConspirator
      let (Just p1AfterConspirator) = afterConspirator ^? players . ix 0
      length (p1AfterConspirator ^. hand) `shouldBe` 6
      (p1AfterConspirator ^. actions) `shouldBe` 1
      (p1AfterConspirator ^. money) `shouldBe` 2

  describe "ironworksCardAction" $ do
    it "gets +action for an action card" $ do
      let forcedDeal = Player "Ironworks Deal" (replicate 5 copperCard) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 $ Strategy "Ironworks Action" bigSmithyBuy bigMoneyDiscard bigMoneyTrash bigMoneyRetrieve bigMoneyOrderHand gainAction bigMoneyThroneRoom bigMoneyLibrary bigMoneySentry bigMoneyHandToDeck bigMoneyLurker
      let afterCard = execState ((ironworksCard ^. action) ironworksCard 1) $ DominionGame [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      let (Just p2AfterCard) = afterCard ^? players . ix 1
      (p2AfterCard ^. actions) `shouldBe` 1
      (p2AfterCard ^. money) `shouldBe` 0
      length (p2AfterCard ^. hand) `shouldBe` 5
    it "gets +money for a treasure card" $ do
      let forcedDeal = Player "Ironworks Deal" (replicate 5 copperCard) [] [copperCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigMoneyStrategy
      let afterCard = execState ((ironworksCard ^. action) ironworksCard 1) $ DominionGame [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      let (Just p2AfterCard) = afterCard ^? players . ix 1
      (p2AfterCard ^. actions) `shouldBe` 0
      (p2AfterCard ^. money) `shouldBe` 1
      length (p2AfterCard ^. hand) `shouldBe` 5
    it "gets +card for a victory card" $ do
      let forcedDeal = Player "Ironworks Deal" (replicate 5 copperCard) [] [estateCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 $ Strategy "Ironworks Victory" bigSmithyBuy bigMoneyDiscard bigMoneyTrash bigMoneyRetrieve bigMoneyOrderHand gainVictory bigMoneyThroneRoom bigMoneyLibrary bigMoneySentry bigMoneyHandToDeck bigMoneyLurker
      let afterCard = execState ((ironworksCard ^. action) ironworksCard 1) $ DominionGame [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      let (Just p2AfterCard) = afterCard ^? players . ix 1
      (p2AfterCard ^. actions) `shouldBe` 0
      (p2AfterCard ^. money) `shouldBe` 0
      length (p2AfterCard ^. hand) `shouldBe` 6

  describe "dukeCardAction" $ do
    it "is worth no points without duchies" $ do
      let afterCard = execState ((dukeCard ^. action) dukeCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. actions) `shouldBe` 1
      (p1AfterCard ^. victory) `shouldBe` 0
    it "is worth one point per duchy" $ do
      let forcedDeal = Player "Ironworks Deal" (replicate 5 copperCard) [duchyCard, duchyCard] [copperCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigMoneyStrategy
      let afterCard = execState ((dukeCard ^. action) dukeCard 1) $ DominionGame [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      let (Just p2AfterCard) = afterCard ^? players . ix 1
      (p2AfterCard ^. victory) `shouldBe` 2
