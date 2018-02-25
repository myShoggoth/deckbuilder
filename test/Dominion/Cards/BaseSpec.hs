module Dominion.Cards.BaseSpec
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
  let g             = mkStdGen 45752345316
  let p1            = newPlayer "Player 1" bigMoneyStrategy
  let p2            = newPlayer "Player 2" bigSmithyStrategy
  let afterDeal     = execState (deal 5 0) $ Game [p1, p2] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
  let afterDeal2    = execState (deal 5 1) afterDeal
  let afterEvaluate = execState (evaluateHand 0) afterDeal2
  let afterReset    = execState (resetTurn 0) afterEvaluate

  describe "cellarCardAction" $
    it "discards all starting cards" $ do
      let afterCard = execState ((cellarCard ^. action) cellarCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 5
      length (p1AfterCard ^. discard) `shouldBe` 5
      length (p1AfterCard ^. played) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 0

  describe "chapelCardAction" $
    it "trashes 4 of the starting cards" $ do
      let afterCard = execState ((chapelCard ^. action) chapelCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 1
      length (p1AfterCard ^. discard) `shouldBe` 0
      length (p1AfterCard ^. played) `shouldBe` 1
      length (p1AfterCard ^. played ++ p1AfterCard ^. discard ++ p1AfterCard ^. hand ++ p1AfterCard ^. deck) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 0
      length (afterCard ^. trash) `shouldBe` 4

  describe "harbingerCardAction" $
    it "takes a silver from the discard pile and puts it on the deck" $ do
      let beforeCard  = execState ((players . ix 0 . discard) %= (silverCard:)) afterDeal
      let afterCard   = execState ((harbingerCard ^. action) harbingerCard 0) beforeCard
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      head (p1AfterCard ^. deck) `shouldBe` silverCard
      length (p1AfterCard ^. hand) `shouldBe` 6
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "merchantCardAction" $ do
    it "adds no money if there are no silver played" $ do
      let afterCard = execState ((merchantCard ^. action) merchantCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. money) `shouldBe` 0
      (p1AfterCard ^. actions) `shouldBe` 1
    it "adds one money if there has been a silver played" $ do
      let afterSilver = execState ((silverCard ^. action) silverCard 0) afterDeal
      let afterCard   = execState ((merchantCard ^. action) merchantCard 0) afterSilver
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. money) `shouldBe` 3 -- 2 for silver, 1 for merchant
      (p1AfterCard ^. actions) `shouldBe` 1
    it "adds one money if a silver is played after" $ do
      let afterCard   = execState ((merchantCard ^. action) merchantCard 0) afterDeal
      let afterSilver = execState ((silverCard ^. action) silverCard 0) afterCard
      let (Just p1AfterCard) = afterSilver ^? players . ix 0
      (p1AfterCard ^. money) `shouldBe` 3
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "vassalCardAction" $
    it "draws a value card" $ do
      let forcedDeal = Player "Vassal Deal" (replicate 5 copperCard) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigMoneyStrategy
      let afterCard = execState (evaluateHand 0) $ Game [forcedDeal] (basicDecks 2) [] g
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. money) `shouldBe` 4
      length (p1AfterCard ^. hand) `shouldBe` 0
      length (p1AfterCard ^. played) `shouldBe` 5
      length (p1AfterCard ^. deck) `shouldBe` 4
      length (p1AfterCard ^. discard) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 0

  describe "bureaucratCardAction" $ do
    let forcedDeal  = Player "Bureaurat Deal" (replicate 5 copperCard) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigMoneyStrategy
    let (Just p1AfterDeal) = afterDeal ^? players . ix 0
    let afterCard   = execState ((bureaucratCard ^. action) bureaucratCard 0) $ Game [p1AfterDeal, forcedDeal] (basicDecks 2) [] g
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "puts a silver on the deck" $ do
      head (p1AfterCard ^. deck) `shouldBe` silverCard
      (p1AfterCard ^. actions) `shouldBe` 0
    it "makes other players discard a victory card" $ do
      let (Just p2) = afterCard ^? players . ix 1
      length (p2 ^. hand) `shouldBe` 4

  describe "gardensCardAction" $
    it "gives 1 point for the starting deck" $ do
      let afterCard = execState ((gardensCard ^. action) gardensCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. victory) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "militiaCardAction" $ do
    let afterCard = execState ((militiaCard ^. action) militiaCard 0) afterDeal2
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "gives two money" $ do
      (p1AfterCard ^. money) `shouldBe` 2
      (p1AfterCard ^. actions) `shouldBe` 0
    it "makes other players discard down to three cards" $ do
      let (Just p2') = find (== p2) (afterCard ^. players)
      length (p2' ^. hand) `shouldBe` 3

  describe "moneylenderCardAction" $ do
    let afterCard = execState ((moneylenderCard ^. action) moneylenderCard 0) afterDeal2
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "gives 3 money" $ do
      (p1AfterCard ^. money) `shouldBe` 3
      (p1AfterCard ^. actions) `shouldBe` 0
    it "trashes a copper" $ do
      length ((p1AfterCard ^. hand) ++ (p1AfterCard ^. discard) ++ (p1AfterCard ^. played) ++ (p1AfterCard ^. deck)) `shouldBe` 10 -- includes the moneylender card itself
      length (afterCard ^. trash) `shouldBe` 1

  describe "poacherCardAction" $ do
    it "gives a card, action, and money" $ do
      let afterCard = execState ((poacherCard ^. action) poacherCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 6
      (p1AfterCard ^. money) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 1
    it "causes a discard per empty supply pile" $ do
      let preCard = set decks (Map.fromList [ (copperCard, 46), (silverCard, 0), (goldCard, 30), (estateCard, 0), (duchyCard, 8), (provinceCard, 8) ]) afterDeal
      let afterCard = execState ((poacherCard ^. action) poacherCard 0) preCard
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 4
      (p1AfterCard ^. money) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "throneRoomCardAction" $ do
    it "will not play if there are no actions left in the hand" $ do
      let afterCard = execState ((throneRoomCard ^. action) throneRoomCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. actions) `shouldBe` 1
      length (p1AfterCard ^. hand) `shouldBe` 5
    it "will play Smithy twice in the bigSmithyStrategy" $ do
      let forcedDeal = Player "Throne Room Deal" (replicate 5 copperCard) [] [throneRoomCard, smithyCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigSmithyStrategy
      let afterCard = execState (evaluateHand 0) $ Game [forcedDeal] (basicDecks 2) [] g
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. actions) `shouldBe` 0
      length (p1AfterCard ^. hand) `shouldBe` 0
      length (p1AfterCard ^. discard) `shouldBe` 0
      length (p1AfterCard ^. deck) `shouldBe` 0
      length (p1AfterCard ^. played) `shouldBe` 10
      (p1AfterCard ^. money) `shouldBe` 7
      (p1AfterCard ^. victory) `shouldBe` 1

  describe "banditCardAction" $ do
    it "gives a gold onto the discard pile" $ do
      let afterCard = execState ((banditCard ^. action) banditCard 0) afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. discard) `shouldBe` 1
      head (p1AfterCard ^. discard) `shouldBe` goldCard
      (p1AfterCard ^. actions) `shouldBe` 0
    it "trashes an opponent's silver" $ do
      let forcedDeal = Player "Bandit Deal" (silverCard: replicate 5 copperCard) [] [estateCard, smithyCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigSmithyStrategy
      let afterCard = execState ((banditCard ^. action) banditCard 0) $ Game [p1, forcedDeal] (basicDecks 2) [] g
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      let (Just fd) = afterCard ^? players . ix 1
      length (fd ^. discard) `shouldBe` 1
      length (fd ^. deck) `shouldBe` 4
      length (afterCard ^. trash) `shouldBe` 1

  describe "councilRoomCardAction" $ do
    let afterCard = execState ((councilRoomCard ^. action) councilRoomCard 0) afterDeal2
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    let (Just p2') = afterCard ^? players . ix 1
    it "draws four cards" $ do
      length (p1AfterCard ^. hand) `shouldBe` 9
      length (p1AfterCard ^. deck) `shouldBe` 1
    it "causes the other players to draw a card" $
      length (p2' ^. hand) `shouldBe` 6

  describe "witchCardAction" $ do
    let afterCard = execState ((witchCard ^. action) witchCard 0) afterDeal2
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    let (Just p2') = afterCard ^? players . ix 1
    it "draws two cards" $ do
      length (p1AfterCard ^. hand) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 0
    it "causes other players to get curses" $
      head (p2' ^. discard) `shouldBe` curseCard

  describe "mineCardAction" $ do
    let afterCard = execState ((mineCard ^. action) mineCard 0) afterDeal
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "upgrades a copper to silver" $
      head (p1AfterCard ^. hand) `shouldBe` silverCard

  describe "libraryCardAction" $ do
    let afterCard = execState ((libraryCard ^. action) libraryCard 0) afterDeal
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "draws to seven cards" $
      length (p1AfterCard ^. hand) `shouldBe` 7

  describe "sentryCardAction" $ do
    let afterCard = execState ((sentryCard ^. action) sentryCard 0) afterDeal
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "discards coppers and estates from the top of the deck" $ do
      length (p1AfterCard ^. deck) `shouldBe` 2 -- draw one plus look at two and discard both
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "artisanCardAction" $ do
    let afterCard = execState ((artisanCard ^. action) artisanCard 0) afterDeal
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "gains a card to the hand and puts another onto the deck" $ do
      length (p1AfterCard ^. deck) `shouldBe` 6
      length (p1AfterCard ^. hand) `shouldBe` 5
      (p1AfterCard ^. actions) `shouldBe` 0
