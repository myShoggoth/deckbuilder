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
import           Control.Monad.RWS
import qualified Data.DList                             as DL
import           Data.List
import qualified Data.Map                               as Map
import           System.Random
import           Test.Hspec
import qualified Test.QuickCheck                        as QC

gainAction :: Int -> Int -> DominionState (Maybe Card)
gainAction = gainCard firstGameKingdomCards

gainVictory :: Int -> Int -> DominionState (Maybe Card)
gainVictory = gainCard (delete curseCard victoryCards)

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
  let afterDeal           = fst $ execRWS (deal 5 0) c dg
  let (Just p1AfterDeal)  = afterDeal ^? players . ix 0
  let afterDeal2          = fst $ execRWS (deal 5 1) c afterDeal
  let afterEvaluate       = fst $ execRWS (evaluateHand 0) c afterDeal2
  let afterReset          = fst $ execRWS (resetTurn 0) c afterEvaluate

  describe "courtyardCardAction" $ do
    let afterCard = fst $ execRWS ((courtyardCard ^. action) courtyardCard 0) c afterDeal
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "draws three cards and puts one back on the deck" $ do
      length (p1AfterCard ^. deck) `shouldBe` 3
      length (p1AfterCard ^. hand) `shouldBe` 7

  describe "lurkerCardAction" $ do
    let afterCard = fst $ execRWS ((lurkerCard ^. action) lurkerCard 0) c afterDeal
    let (Just p1AfterDeal) = afterCard ^? players . ix 0
    it "trashes an action card from supply" $ do
      length (afterCard ^. trash) `shouldBe` 1
      head (afterCard ^. trash) ^. cardType `shouldBe` Action

  describe "shantyTownCardAction" $ do
    let afterCard = fst $ execRWS ((shantyTownCard ^. action) shantyTownCard 0) c afterDeal
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "gets two cards and two actions with no action cards in hand" $ do
      length (p1AfterCard ^. hand) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 2

  describe "conspiratorCardAction" $ do
    let afterCard = fst $ execRWS ((conspiratorCard ^. action) conspiratorCard 0) c afterDeal
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "gets only two money when fewer than two actions have been played" $ do
      length (p1AfterCard ^. hand) `shouldBe` 5
      (p1AfterCard ^. actions) `shouldBe` 0
      (p1AfterCard ^. money) `shouldBe` 2
    it "gets one card, one action, and two cards when two actions have been played" $ do
      let (Just p1) = afterDeal ^? players . ix 0
      let (Just p2) = afterDeal ^? players . ix 1
      let p1Prepped = over played ([conspiratorCard, conspiratorCard] ++) p1
      let (afterConspirator, output) = execRWS ((conspiratorCard ^. action) conspiratorCard 0) c $ DominionGame [p1Prepped, p2] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      let (Just p1AfterConspirator) = afterConspirator ^? players . ix 0
      length (p1Prepped ^. hand) `shouldBe` 5
      length (p1Prepped ^. played) `shouldBe` 2
      length (p1AfterConspirator ^. hand) `shouldBe` 6
      (p1AfterConspirator ^. actions) `shouldBe` 1
      (p1AfterConspirator ^. money) `shouldBe` 2

  describe "ironworksCardAction" $ do
    it "gets +action for an action card" $ do
      let forcedDeal = Player "Ironworks Deal" (replicate 5 copperCard) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 $ Strategy "Ironworks Action" bigSmithyBuy bigMoneyDiscard bigMoneyTrash bigMoneyRetrieve bigMoneyOrderHand gainAction bigMoneyThroneRoom bigMoneyLibrary bigMoneySentry bigMoneyHandToDeck bigMoneyLurker
      let afterCard = fst $ execRWS ((ironworksCard ^. action) ironworksCard 1) c $ DominionGame [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      let (Just p2AfterCard) = afterCard ^? players . ix 1
      (p2AfterCard ^. actions) `shouldBe` 1
      (p2AfterCard ^. money) `shouldBe` 0
      length (p2AfterCard ^. hand) `shouldBe` 5
    it "gets +money for a treasure card" $ do
      let forcedDeal = Player "Ironworks Deal" (replicate 5 copperCard) [] [copperCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigMoneyStrategy
      let afterCard = fst $ execRWS ((ironworksCard ^. action) ironworksCard 1) c $ DominionGame [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      let (Just p2AfterCard) = afterCard ^? players . ix 1
      (p2AfterCard ^. actions) `shouldBe` 0
      (p2AfterCard ^. money) `shouldBe` 1
      length (p2AfterCard ^. hand) `shouldBe` 5
    it "gets +card for a victory card" $ do
      let forcedDeal = Player "Ironworks Deal" (replicate 5 copperCard) [] [estateCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 $ Strategy "Ironworks Victory" bigSmithyBuy bigMoneyDiscard bigMoneyTrash bigMoneyRetrieve bigMoneyOrderHand gainVictory bigMoneyThroneRoom bigMoneyLibrary bigMoneySentry bigMoneyHandToDeck bigMoneyLurker
      let afterCard = fst $ execRWS ((ironworksCard ^. action) ironworksCard 1) c $ DominionGame [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      let (Just p2AfterCard) = afterCard ^? players . ix 1
      (p2AfterCard ^. actions) `shouldBe` 0
      (p2AfterCard ^. money) `shouldBe` 0
      length (p2AfterCard ^. hand) `shouldBe` 6

  describe "dukeCardAction" $ do
    it "is worth no points without duchies" $ do
      let afterCard = fst $ execRWS ((dukeCard ^. action) dukeCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. actions) `shouldBe` 1
      (p1AfterCard ^. victory) `shouldBe` 0
    it "is worth one point per duchy" $ do
      let forcedDeal = Player "Ironworks Deal" (replicate 5 copperCard) [duchyCard, duchyCard] [copperCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigMoneyStrategy
      let afterCard = fst $ execRWS ((dukeCard ^. action) dukeCard 1) c $ DominionGame [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      let (Just p2AfterCard) = afterCard ^? players . ix 1
      (p2AfterCard ^. victory) `shouldBe` 2
