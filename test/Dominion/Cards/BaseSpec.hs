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
import           Control.Monad.RWS
import           Data.List
import qualified Data.Map                               as Map
import           System.Random
import           Test.Hspec
import qualified Test.QuickCheck                        as QC

spec :: Spec
spec = do
  let g        = mkStdGen 45752345316
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
  let afterReset    = fst $ execRWS (resetTurn 0) c afterEvaluate

  describe "cellarCardAction" $
    it "discards all starting cards" $ do
      let afterCard = fst $ execRWS ((cellarCard ^. action) cellarCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 5
      length (p1AfterCard ^. discard) `shouldBe` 5
      length (p1AfterCard ^. played) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 0

  describe "chapelCardAction" $
    it "trashes 4 of the starting cards" $ do
      let afterCard = fst $ execRWS ((chapelCard ^. action) chapelCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 1
      length (p1AfterCard ^. discard) `shouldBe` 0
      length (p1AfterCard ^. played) `shouldBe` 1
      length (p1AfterCard ^. played ++ p1AfterCard ^. discard ++ p1AfterCard ^. hand ++ p1AfterCard ^. deck) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 0
      length (afterCard ^. trash) `shouldBe` 4

  describe "harbingerCardAction" $
    it "takes a silver from the discard pile and puts it on the deck" $ do
      let beforeCard  = fst $ execRWS (((players . ix 0 . discard) %= (silverCard:)) :: DominionState ()) c afterDeal
      let afterCard   = fst $ execRWS ((harbingerCard ^. action) harbingerCard 0) c beforeCard
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      head (p1AfterCard ^. deck) `shouldBe` silverCard
      length (p1AfterCard ^. hand) `shouldBe` 6
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "merchantCardAction" $ do
    it "adds no money if there are no silver played" $ do
      let afterCard = fst $ execRWS ((merchantCard ^. action) merchantCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. money) `shouldBe` 0
      (p1AfterCard ^. actions) `shouldBe` 1
    it "adds one money if there has been a silver played" $ do
      let afterSilver = fst $ execRWS ((silverCard ^. action) silverCard 0) c afterDeal
      let afterCard   = fst $ execRWS ((merchantCard ^. action) merchantCard 0) c afterSilver
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. money) `shouldBe` 3 -- 2 for silver, 1 for merchant
      (p1AfterCard ^. actions) `shouldBe` 1
    it "adds one money if a silver is played after" $ do
      let afterCard   = fst $ execRWS ((merchantCard ^. action) merchantCard 0) c afterDeal
      let afterSilver = fst $ execRWS ((silverCard ^. action) silverCard 0) c afterCard
      let (Just p1AfterCard) = afterSilver ^? players . ix 0
      (p1AfterCard ^. money) `shouldBe` 3
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "vassalCardAction" $
    it "draws a value card" $ do
      let forcedDeal = DominionPlayer "Vassal Deal" (replicate 5 copperCard) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigMoneyStrategy
      let afterCard = fst $ execRWS (evaluateHand 0) c $ DominionGame [forcedDeal] (basicDecks 2) [] g
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. money) `shouldBe` 4
      length (p1AfterCard ^. hand) `shouldBe` 0
      length (p1AfterCard ^. played) `shouldBe` 5
      length (p1AfterCard ^. deck) `shouldBe` 4
      length (p1AfterCard ^. discard) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 0

  describe "bureaucratCardAction" $ do
    let forcedDeal  = DominionPlayer "Bureaurat Deal" (replicate 5 copperCard) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigMoneyStrategy
    let (Just p1AfterDeal) = afterDeal ^? players . ix 0
    let afterCard   = fst $ execRWS ((bureaucratCard ^. action) bureaucratCard 0) c $ DominionGame [p1AfterDeal, forcedDeal] (basicDecks 2) [] g
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "puts a silver on the deck" $ do
      head (p1AfterCard ^. deck) `shouldBe` silverCard
      (p1AfterCard ^. actions) `shouldBe` 0
    it "makes other players discard a victory card" $ do
      let (Just p2) = afterCard ^? players . ix 1
      length (p2 ^. hand) `shouldBe` 4

  describe "gardensCardAction" $
    it "gives 1 point for the starting deck" $ do
      let afterCard = fst $ execRWS ((gardensCard ^. action) gardensCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. victory) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "militiaCardAction" $ do
    let afterCard = fst $ execRWS ((militiaCard ^. action) militiaCard 0) c afterDeal2
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "gives two money" $ do
      (p1AfterCard ^. money) `shouldBe` 2
      (p1AfterCard ^. actions) `shouldBe` 0
    it "makes other players discard down to three cards" $ do
      let (Just p2AfterCard)  = afterCard ^? players . ix 1
      length (p2AfterCard ^. hand) `shouldBe` 3

  describe "moneylenderCardAction" $ do
    let afterCard = fst $ execRWS ((moneylenderCard ^. action) moneylenderCard 0) c afterDeal2
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "gives 3 money" $ do
      (p1AfterCard ^. money) `shouldBe` 3
      (p1AfterCard ^. actions) `shouldBe` 0
    it "trashes a copper" $ do
      length ((p1AfterCard ^. hand) ++ (p1AfterCard ^. discard) ++ (p1AfterCard ^. played) ++ (p1AfterCard ^. deck)) `shouldBe` 10 -- includes the moneylender card itself
      length (afterCard ^. trash) `shouldBe` 1

  describe "poacherCardAction" $ do
    it "gives a card, action, and money" $ do
      let afterCard = fst $ execRWS ((poacherCard ^. action) poacherCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 6
      (p1AfterCard ^. money) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 1
    it "causes a discard per empty supply pile" $ do
      let preCard = set decks (Map.fromList [ (copperCard, 46), (silverCard, 0), (goldCard, 30), (estateCard, 0), (duchyCard, 8), (provinceCard, 8) ]) afterDeal
      let afterCard = fst $ execRWS ((poacherCard ^. action) poacherCard 0) c preCard
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. hand) `shouldBe` 4
      (p1AfterCard ^. money) `shouldBe` 1
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "throneRoomCardAction" $ do
    it "will not play if there are no actions left in the hand" $ do
      let afterCard = fst $ execRWS ((throneRoomCard ^. action) throneRoomCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      (p1AfterCard ^. actions) `shouldBe` 1
      length (p1AfterCard ^. hand) `shouldBe` 5
    it "will play Smithy twice in the bigSmithyStrategy" $ do
      let forcedDeal = DominionPlayer "Throne Room Deal" (replicate 5 copperCard) [] [throneRoomCard, smithyCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigSmithyStrategy
      let afterCard = fst $ execRWS (evaluateHand 0) c $ DominionGame [forcedDeal] (basicDecks 2) [] g
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
      let afterCard = fst $ execRWS ((banditCard ^. action) banditCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      length (p1AfterCard ^. discard) `shouldBe` 1
      head (p1AfterCard ^. discard) `shouldBe` goldCard
      (p1AfterCard ^. actions) `shouldBe` 0
    it "trashes an opponent's silver" $ do
      let forcedDeal = DominionPlayer "Bandit Deal" (silverCard: replicate 5 copperCard) [] [estateCard, smithyCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigSmithyStrategy
      let (Just p1AfterDeal)  = afterDeal ^? players . ix 0
      let afterCard = fst $ execRWS ((banditCard ^. action) banditCard 0) c $ DominionGame [p1AfterDeal, forcedDeal] (basicDecks 2) [] g
      let (Just p1AfterCard) = afterCard ^? players . ix 0
      let (Just fd) = afterCard ^? players . ix 1
      length (fd ^. discard) `shouldBe` 1
      length (fd ^. deck) `shouldBe` 4
      length (afterCard ^. trash) `shouldBe` 1

  describe "councilRoomCardAction" $ do
    let afterCard = fst $ execRWS ((councilRoomCard ^. action) councilRoomCard 0) c afterDeal2
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    let (Just p2') = afterCard ^? players . ix 1
    it "draws four cards" $ do
      length (p1AfterCard ^. hand) `shouldBe` 9
      length (p1AfterCard ^. deck) `shouldBe` 1
    it "causes the other players to draw a card" $
      length (p2' ^. hand) `shouldBe` 6

  describe "witchCardAction" $ do
    let afterCard = fst $ execRWS ((witchCard ^. action) witchCard 0) c afterDeal2
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    let (Just p2') = afterCard ^? players . ix 1
    it "draws two cards" $ do
      length (p1AfterCard ^. hand) `shouldBe` 7
      (p1AfterCard ^. actions) `shouldBe` 0
    it "causes other players to get curses" $
      head (p2' ^. discard) `shouldBe` curseCard

  describe "mineCardAction" $ do
    let afterCard = fst $ execRWS ((mineCard ^. action) mineCard 0) c afterDeal
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "upgrades a copper to silver" $
      head (p1AfterCard ^. hand) `shouldBe` silverCard

  describe "libraryCardAction" $ do
    let afterCard = fst $ execRWS ((libraryCard ^. action) libraryCard 0) c afterDeal
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "draws to seven cards" $
      length (p1AfterCard ^. hand) `shouldBe` 7

  describe "sentryCardAction" $ do
    let afterCard = fst $ execRWS ((sentryCard ^. action) sentryCard 0) c afterDeal
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "discards coppers and estates from the top of the deck" $ do
      length (p1AfterCard ^. deck) `shouldBe` 2 -- draw one plus look at two and discard both
      (p1AfterCard ^. actions) `shouldBe` 1

  describe "artisanCardAction" $ do
    let afterCard = fst $ execRWS ((artisanCard ^. action) artisanCard 0) c afterDeal
    let (Just p1AfterCard) = afterCard ^? players . ix 0
    it "gains a card to the hand and puts another onto the deck" $ do
      length (p1AfterCard ^. deck) `shouldBe` 6
      length (p1AfterCard ^. hand) `shouldBe` 5
      (p1AfterCard ^. actions) `shouldBe` 0
