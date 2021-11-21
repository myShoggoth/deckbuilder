{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}

module Dominion.Cards.BaseSpec
    ( spec
    ) where

import Control.Lens ( (^?), (^.), (%=), set, Ixed(ix) )
import Control.Monad.State ( execState )
import Data.Generics.Product ( HasField(field) )
import qualified Data.Map as Map
import DeckBuilding.Dominion
    ( basicDecks, configToGame, evaluateHand )
import DeckBuilding.Dominion.Cards
    ( artisanCard,
      banditCard,
      bureaucratCard,
      cellarCard,
      chapelCard,
      copperCard,
      councilRoomCard,
      curseCard,
      duchyCard,
      estateCard,
      firstGameKingdomCards,
      gardensCard,
      goldCard,
      harbingerCard,
      libraryCard,
      merchantCard,
      militiaCard,
      mineCard,
      moneylenderCard,
      poacherCard,
      provinceCard,
      remodelCard,
      sentryCard,
      silverCard,
      smithyCard,
      throneRoomCard,
      vassalCard,
      witchCard, moatCard )
import DeckBuilding.Dominion.Strategies.Basic
    ( bigMoneyStrategy, bigSmithyStrategy )
import DeckBuilding.Dominion.Types
    ( DominionConfig(DominionConfig),
      DominionBoard(DominionBoard),
      DominionPlayer(DominionPlayer),
      DominionState, Card (Card) )
import DeckBuilding.Dominion.Utils ( deal, cardPlayed )
import DeckBuilding.Types ( PlayerNumber(PlayerNumber) )
import System.Random ( mkStdGen )
import Test.Hspec ( Spec, describe, it, shouldBe )

spec :: Spec
spec = do
  let g        = mkStdGen 45752345316
  let c = DominionConfig
              [ ("Player 1", bigMoneyStrategy)
              , ("Player 2", bigSmithyStrategy)
              ]
              firstGameKingdomCards
  let p0 = PlayerNumber 0
      p1 = PlayerNumber 1
  let dg = configToGame c g
  let afterDeal     = execState (deal 5 p0) dg
  let afterDeal2    = execState (deal 5 p1) afterDeal

  describe "cellarCardAction" $
    it "discards all starting cards" $ do
      let afterCard = execState ((cellarCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      length (p1AfterCard ^. #hand) `shouldBe` 5
      length (p1AfterCard ^. #discard) `shouldBe` 5
      (p1AfterCard ^. #actions) `shouldBe` 1

  describe "chapelCardAction" $
    it "trashes 4 of the starting cards" $ do
      let afterCard = execState ((chapelCard ^. #action) p0) afterDeal
      let (Just p1AfterDeal) = afterDeal ^? #players . ix 0
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      length (p1AfterCard ^. #hand) `shouldBe` 1
      length (p1AfterCard ^. #discard) `shouldBe` 0
      length (p1AfterCard ^. #played ++ p1AfterCard ^. #discard ++ p1AfterCard ^. #hand ++ p1AfterCard ^. #deck) `shouldBe` 6
      (p1AfterCard ^. #actions) `shouldBe` 0
      length (afterCard ^. #trash) `shouldBe` 4

  describe "harbingerCardAction" $
    it "takes a silver from the discard pile and puts it on the deck" $ do
      let beforeCard  = execState ((( #players . ix 0 . #discard) %= (silverCard:)) :: DominionState ()) afterDeal
      let afterCard   = execState ((harbingerCard ^. #action) p0) beforeCard
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      head (p1AfterCard ^. #deck) `shouldBe` silverCard
      length (p1AfterCard ^. #hand) `shouldBe` 6
      (p1AfterCard ^. #actions) `shouldBe` 1

  describe "merchantCardAction" $ do
    it "adds no money if there are no silver played" $ do
      let afterCard = execState ((merchantCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      (p1AfterCard ^. #money) `shouldBe` 0
      (p1AfterCard ^. #actions) `shouldBe` 1
    it "adds one money if there has been a silver played" $ do
      let afterSilver = execState ((silverCard ^. #action) p0) afterDeal
          afterSilverPlayed = execState (cardPlayed silverCard p0) afterSilver
      let afterCard   = execState ((merchantCard ^. #action) p0) afterSilverPlayed
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      (p1AfterCard ^. #money) `shouldBe` 3 -- 2 for silver, 1 for merchant
      (p1AfterCard ^. #actions) `shouldBe` 1
    it "adds one money if a silver is played after" $ do
      let afterCard   = execState ((merchantCard ^. #action) p0) afterDeal
          afterMerchantPlayed = execState (cardPlayed merchantCard p0) afterCard
      let afterSilver = execState ((silverCard ^. #action) p0) afterMerchantPlayed
      let (Just p1AfterCard) = afterSilver ^? #players . ix 0
      (p1AfterCard ^. #money) `shouldBe` 3
      (p1AfterCard ^. #actions) `shouldBe` 1

  describe "vassalCardAction" $
    it "draws a value card" $ do
      let forcedDeal = DominionPlayer "Vassal Deal" (replicate 5 copperCard) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 [] [] [] 0 bigMoneyStrategy
      let afterCard = execState (evaluateHand p0) $ DominionBoard [forcedDeal] (basicDecks 2) [] (Map.fromList [] :: Map.Map Card Int) [moatCard] curseCard g
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      (p1AfterCard ^. #money) `shouldBe` 5
      length (p1AfterCard ^. #hand) `shouldBe` 0
      length (p1AfterCard ^. #played) `shouldBe` 6
      length (p1AfterCard ^. #deck) `shouldBe` 4
      length (p1AfterCard ^. #discard) `shouldBe` 1
      (p1AfterCard ^. #actions) `shouldBe` 0

  describe "bureaucratCardAction" $ do
    let forcedDeal  = DominionPlayer "Bureaurat Deal" (replicate 5 copperCard) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 [] [] [] 0 bigMoneyStrategy
    let (Just p1AfterDeal) = afterDeal ^? #players . ix 0
    let afterCard   = execState ((bureaucratCard ^. #action) p0) $ DominionBoard [p1AfterDeal, forcedDeal] (basicDecks 2) [] (Map.fromList [] :: Map.Map Card Int) [moatCard] curseCard g
    let (Just p1AfterCard) = afterCard ^? #players . ix 0
    it "puts a silver on the deck" $ do
      head (p1AfterCard ^. #deck) `shouldBe` silverCard
      (p1AfterCard ^. #actions) `shouldBe` 0
    it "makes other players discard a victory card" $ do
      let (Just p2) = afterCard ^? #players . ix 1
      length (p2 ^. #hand) `shouldBe` 4

  describe "gardensCardAction" $
    it "gives 1 point for the starting deck" $ do
      let afterCard = execState ((gardensCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      (p1AfterCard ^. #actions) `shouldBe` 1

  describe "militiaCardAction" $ do
    let afterCard = execState ((militiaCard ^. #action) p0) afterDeal2
    let (Just p1AfterCard) = afterCard ^? #players . ix 0
    it "gives two money" $ do
      (p1AfterCard ^. #money) `shouldBe` 2
      (p1AfterCard ^. #actions) `shouldBe` 0
    it "makes other players discard down to three cards" $ do
      let (Just p2AfterCard)  = afterCard ^? #players . ix 1
      length (p2AfterCard ^. #hand) `shouldBe` 3

  describe "moneylenderCardAction" $ do
    let afterCard = execState ((moneylenderCard ^. #action) p0) afterDeal2
    let (Just p1AfterCard) = afterCard ^? #players . ix 0
    it "gives 3 money" $ do
      (p1AfterCard ^. #money) `shouldBe` 3
      (p1AfterCard ^. #actions) `shouldBe` 0
    it "trashes a copper" $ do
      length ((p1AfterCard ^. #hand) ++ (p1AfterCard ^. #discard) ++ (p1AfterCard ^. #played) ++ (p1AfterCard ^. #deck)) `shouldBe` 9 -- the moneylender was not played
      length (afterCard ^. #trash) `shouldBe` 1

  describe "poacherCardAction" $ do
    it "gives a card, action, and money" $ do
      let afterCard = execState ((poacherCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      length (p1AfterCard ^. #hand) `shouldBe` 6
      (p1AfterCard ^. #money) `shouldBe` 1
      (p1AfterCard ^. #actions) `shouldBe` 1
    it "causes a discard per empty supply pile" $ do
      let preCard = set (#decks) (Map.fromList [ (copperCard, 46), (silverCard, 0), (goldCard, 30), (estateCard, 0), (duchyCard, 8), (provinceCard, 8) ]) afterDeal
      let afterCard = execState ((poacherCard ^. #action) p0) preCard
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      length (p1AfterCard ^. #hand) `shouldBe` 4
      (p1AfterCard ^. #money) `shouldBe` 1
      (p1AfterCard ^. #actions) `shouldBe` 1

  describe "throneRoomCardAction" $ do
    it "will not play if there are no actions left in the hand" $ do
      let afterCard = execState ((throneRoomCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      (p1AfterCard ^. #actions) `shouldBe` 1
      length (p1AfterCard ^. #hand) `shouldBe` 5
    it "will play Smithy twice in the bigSmithyStrategy" $ do
      let forcedDeal = DominionPlayer "Throne Room Deal" (replicate 5 copperCard) [] [throneRoomCard, smithyCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 [] [] [] 0 bigSmithyStrategy
      let afterCard = execState (evaluateHand p0) $ DominionBoard [forcedDeal] (basicDecks 2) [] (Map.fromList [] :: Map.Map Card Int) [moatCard] curseCard g
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      (p1AfterCard ^. #actions) `shouldBe` 0
      length (p1AfterCard ^. #hand) `shouldBe` 0
      length (p1AfterCard ^. #discard) `shouldBe` 0
      length (p1AfterCard ^. #deck) `shouldBe` 0
      length (p1AfterCard ^. #played) `shouldBe` 10
      (p1AfterCard ^. #money) `shouldBe` 7

  describe "banditCardAction" $ do
    it "gives a gold onto the discard pile" $ do
      let afterCard = execState ((banditCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      length (p1AfterCard ^. #discard) `shouldBe` 1
      head (p1AfterCard ^. #discard) `shouldBe` goldCard
      (p1AfterCard ^. #actions) `shouldBe` 0
    it "trashes an opponent's silver" $ do
      let forcedDeal = DominionPlayer "Bandit Deal" (silverCard: replicate 5 copperCard) [] [estateCard, smithyCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 [] [] [] 0 bigSmithyStrategy
      let (Just p1AfterDeal)  = afterDeal ^? #players . ix 0
      let afterCard = execState ((banditCard ^. #action) p0) $ DominionBoard [p1AfterDeal, forcedDeal] (basicDecks 2) [] (Map.fromList [] :: Map.Map Card Int) [moatCard] curseCard g
      let (Just fd) = afterCard ^? #players . ix 1
      length (fd ^. #discard) `shouldBe` 1
      length (fd ^. #deck) `shouldBe` 4
      length (afterCard ^. #trash) `shouldBe` 1

  describe "councilRoomCardAction" $ do
    let afterCard = execState ((councilRoomCard ^. #action) p0) afterDeal2
    let (Just p1AfterCard) = afterCard ^? #players . ix 0
    let (Just p2') = afterCard ^? #players . ix 1
    it "draws four cards" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 9
      length (p1AfterCard ^. #deck) `shouldBe` 1
    it "causes the other players to draw a card" $
      length (p2' ^. #hand) `shouldBe` 6

  describe "witchCardAction" $ do
    let afterCard = execState ((witchCard ^. #action) p0) afterDeal2
    let (Just p1AfterCard) = afterCard ^? #players . ix 0
    let (Just p2') = afterCard ^? #players . ix 1
    it "draws two cards" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 7
      (p1AfterCard ^. #actions) `shouldBe` 0
    it "causes other players to get curses" $ do
      p1AfterCard ^. #discard `shouldBe` []
      p2' ^. #discard `shouldBe` [curseCard]
      head (p2' ^. #discard) `shouldBe` curseCard

  describe "mineCardAction" $ do
    let afterCard = execState ((mineCard ^. #action) p0) afterDeal
    let (Just p1AfterCard) = afterCard ^? #players . ix 0
    it "upgrades a copper to silver" $
      head (p1AfterCard ^. #hand) `shouldBe` silverCard

  describe "libraryCardAction" $ do
    let afterCard = execState ((libraryCard ^. #action) p0) afterDeal
    let (Just p1AfterCard) = afterCard ^? #players . ix 0
    it "draws to seven cards" $
      length (p1AfterCard ^. #hand) `shouldBe` 7

  describe "sentryCardAction" $ do
    let afterCard = execState ((sentryCard ^. #action) p0) afterDeal
    let (Just p1AfterCard) = afterCard ^? #players . ix 0
    it "discards coppers and estates from the top of the deck" $ do
      length (p1AfterCard ^. #deck) `shouldBe` 2 -- draw one plus look at two and discard both
      (p1AfterCard ^. #actions) `shouldBe` 1

  describe "artisanCardAction" $ do
    let afterCard = execState ((artisanCard ^. #action) p0) afterDeal
    let (Just p1AfterCard) = afterCard ^? #players . ix 0
    it "gains a card to the hand and puts another onto the deck" $ do
      length (p1AfterCard ^. #deck) `shouldBe` 6
      length (p1AfterCard ^. #hand) `shouldBe` 5
      (p1AfterCard ^. #actions) `shouldBe` 0

  describe "remodelCardAction" $ do
    let afterCard = execState ((remodelCard ^. #action) p0) afterDeal
    let (Just p1AfterCard) = afterCard ^? #players . ix 0
    it "trashes a copper and gains a silver into the hand" $ do
      length (afterCard ^. #trash) `shouldBe` 1
      length (p1AfterCard ^. #hand) `shouldBe` 4
      length (p1AfterCard ^. #discard) `shouldBe` 1
      length (p1AfterCard ^. #deck) `shouldBe` 5
