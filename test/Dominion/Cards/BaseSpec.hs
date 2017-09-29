module Dominion.Cards.BaseSpec
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

  describe "cellarCardAction" $
    it "discards all starting cards" $ do
      let (p1AfterCard, afterCard) = runState ((cellarCard ^. action) cellarCard p1AfterDeal) afterDeal
      length (p1AfterCard ^. hand) `shouldBe` 5
      length (p1AfterCard ^. discard) `shouldBe` 5
      length (p1AfterCard ^. played) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 0

  describe "chapelCardAction" $
    it "trashes 4 of the starting cards" $ do
      let (p1AfterCard, afterCard) = runState ((chapelCard ^. action) chapelCard p1AfterDeal) afterDeal
      length (p1AfterCard ^. hand) `shouldBe` 1
      length (p1AfterCard ^. discard) `shouldBe` 0
      length (p1AfterCard ^. played) `shouldBe` 1
      length (p1AfterCard ^. played ++ p1AfterCard ^. discard ++ p1AfterCard ^. hand ++ p1AfterCard ^. deck) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 0
      length (afterCard ^. trash) `shouldBe` 4

  describe "harbingerCardAction" $
    it "takes a silver from the discard pile and puts it on the deck" $ do
      let (p1BeforeCard, beforeCard)  = runState (updatePlayer (over discard (silverCard:) p1AfterDeal)) afterDeal
      let (p1AfterCard, afterCard)    = runState ((harbingerCard ^. action) harbingerCard p1BeforeCard) beforeCard
      head (p1AfterCard ^. deck) `shouldBe` silverCard
      length (p1AfterCard ^. hand) `shouldBe` 6
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "merchantCardAction" $ do
    it "adds one money if there is no silver played" $ do
      let (p1AfterCard, afterCard) = runState ((merchantCard ^. action) merchantCard p1AfterDeal) afterDeal
      (p1AfterCard ^. money) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 1
    it "adds two money if there has been a silver played" $ do
      let (p1AfterSilver, afterSilver)  = runState ((silverCard ^. action) silverCard p1AfterDeal) afterDeal
      let (p1AfterCard, afterCard)      = runState ((merchantCard ^. action) merchantCard p1AfterSilver) afterSilver
      (p1AfterCard ^. money) `shouldBe` 4 -- 2 for silver, 2 for merchant
      (p1AfterCard ^. actions) `shouldBe` 1
    it "adds two money if a silver is played after" $ do
      let (p1AfterSilver, afterCard)  = runState ((merchantCard ^. action) merchantCard p1AfterDeal) afterDeal
      let (p1AfterCard, afterSilver)  = runState ((silverCard ^. action) silverCard p1AfterSilver) afterCard
      (p1AfterCard ^. money) `shouldBe` 4
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "vassalCardAction" $
    it "draws a value card" $ do
      let forcedDeal = Player "Vassal Deal" (replicate 5 copperCard) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigMoneyStrategy
      let (p1AfterCard, afterCard) = runState (evaluateHand forcedDeal) $ Game [forcedDeal] (basicDecks 2) [] g
      (p1AfterCard ^. money) `shouldBe` 4
      length (p1AfterCard ^. hand) `shouldBe` 0
      length (p1AfterCard ^. played) `shouldBe` 5
      length (p1AfterCard ^. deck) `shouldBe` 4
      length (p1AfterCard ^. discard) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 0

  describe "bureaucratCardAction" $ do
    let forcedDeal = Player "Bureaurat Deal" (replicate 5 copperCard) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigMoneyStrategy
    let (p1AfterCard, afterCard) = runState ((bureaucratCard ^. action) bureaucratCard p1AfterDeal) $ Game [p1AfterDeal, forcedDeal] (basicDecks 2) [] g
    it "puts a silver on the deck" $ do
      head (p1AfterCard ^. deck) `shouldBe` silverCard
      (p1AfterCard ^. actions) `shouldBe` 0
    it "makes other players discard a victory card" $ do
      let (Just p2') = find (== forcedDeal) (afterCard ^. players)
      length (p2' ^. hand) `shouldBe` 4

  describe "gardensCardAction" $
    it "gives 1 point for the starting deck" $ do
      let (p1AfterCard, afterCard) = runState ((gardensCard ^. action) gardensCard p1AfterDeal) afterDeal
      (p1AfterCard ^. victory) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "militiaCardAction" $ do
    let (p1AfterCard, afterCard) = runState ((militiaCard ^. action) militiaCard p1AfterDeal) afterDeal2
    it "gives two money" $ do
      (p1AfterCard ^. money) `shouldBe` 2
      (p1AfterCard ^. actions) `shouldBe` 0
    it "makes other players discard down to three cards" $ do
      let (Just p2') = find (== p2) (afterCard ^. players)
      length (p2' ^. hand) `shouldBe` 3

  describe "moneylenderCardAction" $ do
    let (p1AfterCard, afterCard) = runState ((moneylenderCard ^. action) moneylenderCard p1AfterDeal) afterDeal2
    it "gives 3 money" $ do
      (p1AfterCard ^. money) `shouldBe` 3
      (p1AfterCard ^. actions) `shouldBe` 0
    it "trashes a copper" $ do
      length ((p1AfterCard ^. hand) ++ (p1AfterCard ^. discard) ++ (p1AfterCard ^. played) ++ (p1AfterCard ^. deck)) `shouldBe` 10 -- includes the moneylender card itself
      length (afterCard ^. trash) `shouldBe` 1

  describe "poacherCardAction" $ do
    it "gives a card, action, and money" $ do
      let (p1AfterCard, afterCard) = runState ((poacherCard ^. action) poacherCard p1AfterDeal) afterDeal
      length (p1AfterCard ^. hand) `shouldBe` 6
      (p1AfterCard ^. money) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 1
    it "causes a discard per empty supply pile" $ do
      let preCard = set decks (Map.fromList [ (copperCard, 46), (silverCard, 0), (goldCard, 30), (estateCard, 0), (duchyCard, 8), (provinceCard, 8) ]) afterDeal
      let (p1AfterCard, afterCard) = runState ((poacherCard ^. action) poacherCard p1AfterDeal) preCard
      length (p1AfterCard ^. hand) `shouldBe` 4
      (p1AfterCard ^. money) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "throneRoomCardAction" $ do
    it "will not play if there are no actions left in the hand" $ do
      let (p1AfterCard, afterCard) = runState ((throneRoomCard ^. action) throneRoomCard p1AfterDeal) afterDeal
      (p1AfterCard ^. actions) `shouldBe` 1
      length (p1AfterCard ^. hand) `shouldBe` 5
    it "will play Smithy twice in the bigSmithyStrategy" $ do
      let forcedDeal = Player "Throne Room Deal" (replicate 5 copperCard) [] [throneRoomCard, smithyCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigSmithyStrategy
      let (p1AfterCard, afterCard) = runState (evaluateHand forcedDeal) $ Game [forcedDeal] (basicDecks 2) [] g
      (p1AfterCard ^. actions) `shouldBe` 0
      length (p1AfterCard ^. hand) `shouldBe` 0
      length (p1AfterCard ^. discard) `shouldBe` 0
      length (p1AfterCard ^. deck) `shouldBe` 0
      length (p1AfterCard ^. played) `shouldBe` 10
      (p1AfterCard ^. money) `shouldBe` 7
      (p1AfterCard ^. victory) `shouldBe` 1

  describe "banditCardAction" $ do
    it "gives a gold onto the discard pile" $ do
      let (p1AfterCard, afterCard) = runState ((banditCard ^. action) banditCard p1AfterDeal) afterDeal
      length (p1AfterCard ^. discard) `shouldBe` 1
      head (p1AfterCard ^. discard) `shouldBe` goldCard
      (p1AfterCard ^. actions) `shouldBe` 0
    it "trashes an opponent's silver" $ do
      let forcedDeal = Player "Bandit Deal" (silverCard: replicate 5 copperCard) [] [estateCard, smithyCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigSmithyStrategy
      let (p1AfterCard, afterCard) = runState ((banditCard ^. action) banditCard p1) $ Game [p1, forcedDeal] (basicDecks 2) [] g
      let (Just fd) = find (== forcedDeal) (afterCard ^. players)
      length (fd ^. discard) `shouldBe` 1
      length (fd ^. deck) `shouldBe` 4
      length (afterCard ^. trash) `shouldBe` 1

  describe "councilRoomCardAction" $ do
    let (p1AfterCard, afterCard) = runState ((councilRoomCard ^. action) councilRoomCard p1AfterDeal) afterDeal2
    let (Just p2') = find (== p2) (afterCard ^. players)
    it "draws four cards" $ do
      length (p1AfterCard ^. hand) `shouldBe` 9
      length (p1AfterCard ^. deck) `shouldBe` 1
    it "causes the other players to draw a card" $
      length (p2' ^. hand) `shouldBe` 6

  describe "witchCardAction" $ do
    let (p1AfterCard, afterCard) = runState ((witchCard ^. action) witchCard p1AfterDeal) afterDeal2
    let (Just p2') = find (== p2) (afterCard ^. players)
    it "draws two cards" $ do
      length (p1AfterCard ^. hand) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 0
    it "causes other players to get curses" $
      head (p2' ^. discard) `shouldBe` curseCard

  describe "mineCardAction" $ do
    let (p1AfterCard, afterCard) = runState ((mineCard ^. action) mineCard p1AfterDeal) afterDeal
    it "upgrades a copper to silver" $
      head (p1AfterCard ^. hand) `shouldBe` silverCard

  describe "libraryCardAction" $ do
    let (p1AfterCard, afterCard) = runState ((libraryCard ^. action) libraryCard p1AfterDeal) afterDeal
    it "draws to seven cards" $
      length (p1AfterCard ^. hand) `shouldBe` 7

  describe "sentryCardAction" $ do
    let (p1AfterCard, afterCard) = runState ((sentryCard ^. action) sentryCard p1AfterDeal) afterDeal
    it "discards coppers and estates from the top of the deck" $ do
      length (p1AfterCard ^. deck) `shouldBe` 2 -- draw one plus look at two and discard both
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "artisanCardAction" $ do
    let (p1AfterCard, afterCard) = runState ((artisanCard ^. action) artisanCard p1AfterDeal) afterDeal
    it "gains a card to the hand and puts another onto the deck" $ do
      length (p1AfterCard ^. deck) `shouldBe` 6
      length (p1AfterCard ^. hand) `shouldBe` 5
      (p1AfterCard ^. actions) `shouldBe` 0
