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
import Control.Monad.RWS ( execRWS )
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
      witchCard )
import DeckBuilding.Dominion.Strategies.Basic
    ( bigMoneyStrategy, bigSmithyStrategy )
import DeckBuilding.Dominion.Types
    ( DominionConfig(DominionConfig),
      DominionGame(DominionGame),
      DominionPlayer(DominionPlayer),
      DominionState )
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
              1
              [g]
  let p0 = PlayerNumber 0
      p1 = PlayerNumber 1
  let dg = configToGame c g
  let afterDeal     = fst $ execRWS (deal 5 p0) c dg
  let afterDeal2    = fst $ execRWS (deal 5 p1) c afterDeal

  describe "cellarCardAction" $
    it "discards all starting cards" $ do
      let afterCard = fst $ execRWS ((cellarCard ^. field @"action") cellarCard p0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      length (p1AfterCard ^. field @"hand") `shouldBe` 5
      length (p1AfterCard ^. field @"discard") `shouldBe` 5
      (p1AfterCard ^. field @"actions") `shouldBe` 1

  describe "chapelCardAction" $
    it "trashes 4 of the starting cards" $ do
      let afterCard = fst $ execRWS ((chapelCard ^. field @"action") chapelCard p0) c afterDeal
      let (Just p1AfterDeal) = afterDeal ^? field @"players" . ix 0
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      length (p1AfterCard ^. field @"hand") `shouldBe` 1
      length (p1AfterCard ^. field @"discard") `shouldBe` 0
      length (p1AfterCard ^. field @"played" ++ p1AfterCard ^. field @"discard" ++ p1AfterCard ^. field @"hand" ++ p1AfterCard ^. field @"deck") `shouldBe` 6
      (p1AfterCard ^. field @"actions") `shouldBe` 0
      length (afterCard ^. field @"trash") `shouldBe` 4

  describe "harbingerCardAction" $
    it "takes a silver from the discard pile and puts it on the deck" $ do
      let beforeCard  = fst $ execRWS (((field @"players" . ix 0 . field @"discard") %= (silverCard:)) :: DominionState ()) c afterDeal
      let afterCard   = fst $ execRWS ((harbingerCard ^. field @"action") harbingerCard p0) c beforeCard
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      head (p1AfterCard ^. field @"deck") `shouldBe` silverCard
      length (p1AfterCard ^. field @"hand") `shouldBe` 6
      (p1AfterCard ^. field @"actions") `shouldBe` 1

  describe "merchantCardAction" $ do
    it "adds no money if there are no silver played" $ do
      let afterCard = fst $ execRWS ((merchantCard ^. field @"action") merchantCard p0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      (p1AfterCard ^. field @"money") `shouldBe` 0
      (p1AfterCard ^. field @"actions") `shouldBe` 1
    it "adds one money if there has been a silver played" $ do
      let afterSilver = fst $ execRWS ((silverCard ^. field @"action") silverCard p0) c afterDeal
          afterSilverPlayed = fst $ execRWS (cardPlayed silverCard p0) c afterSilver
      let afterCard   = fst $ execRWS ((merchantCard ^. field @"action") merchantCard p0) c afterSilverPlayed
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      (p1AfterCard ^. field @"money") `shouldBe` 3 -- 2 for silver, 1 for merchant
      (p1AfterCard ^. field @"actions") `shouldBe` 1
    it "adds one money if a silver is played after" $ do
      let afterCard   = fst $ execRWS ((merchantCard ^. field @"action") merchantCard p0) c afterDeal
          afterMerchantPlayed = fst $ execRWS (cardPlayed merchantCard p0) c afterCard
      let afterSilver = fst $ execRWS ((silverCard ^. field @"action") silverCard p0) c afterMerchantPlayed
      let (Just p1AfterCard) = afterSilver ^? field @"players" . ix 0
      (p1AfterCard ^. field @"money") `shouldBe` 3
      (p1AfterCard ^. field @"actions") `shouldBe` 1

  describe "vassalCardAction" $
    it "draws a value card" $ do
      let forcedDeal = DominionPlayer "Vassal Deal" (replicate 5 copperCard) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigMoneyStrategy
      let afterCard = fst $ execRWS (evaluateHand p0) c $ DominionGame [forcedDeal] (basicDecks 2) [] g
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      (p1AfterCard ^. field @"money") `shouldBe` 5
      length (p1AfterCard ^. field @"hand") `shouldBe` 0
      length (p1AfterCard ^. field @"played") `shouldBe` 6
      length (p1AfterCard ^. field @"deck") `shouldBe` 4
      length (p1AfterCard ^. field @"discard") `shouldBe` 1
      (p1AfterCard ^. field @"actions") `shouldBe` 0

  describe "bureaucratCardAction" $ do
    let forcedDeal  = DominionPlayer "Bureaurat Deal" (replicate 5 copperCard) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigMoneyStrategy
    let (Just p1AfterDeal) = afterDeal ^? field @"players" . ix 0
    let afterCard   = fst $ execRWS ((bureaucratCard ^. field @"action") bureaucratCard p0) c $ DominionGame [p1AfterDeal, forcedDeal] (basicDecks 2) [] g
    let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
    it "puts a silver on the deck" $ do
      head (p1AfterCard ^. field @"deck") `shouldBe` silverCard
      (p1AfterCard ^. field @"actions") `shouldBe` 0
    it "makes other players discard a victory card" $ do
      let (Just p2) = afterCard ^? field @"players" . ix 1
      length (p2 ^. field @"hand") `shouldBe` 4

  describe "gardensCardAction" $
    it "gives 1 point for the starting deck" $ do
      let afterCard = fst $ execRWS ((gardensCard ^. field @"action") gardensCard p0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      (p1AfterCard ^. field @"actions") `shouldBe` 1

  describe "militiaCardAction" $ do
    let afterCard = fst $ execRWS ((militiaCard ^. field @"action") militiaCard p0) c afterDeal2
    let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
    it "gives two money" $ do
      (p1AfterCard ^. field @"money") `shouldBe` 2
      (p1AfterCard ^. field @"actions") `shouldBe` 0
    it "makes other players discard down to three cards" $ do
      let (Just p2AfterCard)  = afterCard ^? field @"players" . ix 1
      length (p2AfterCard ^. field @"hand") `shouldBe` 3

  describe "moneylenderCardAction" $ do
    let afterCard = fst $ execRWS ((moneylenderCard ^. field @"action") moneylenderCard p0) c afterDeal2
    let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
    it "gives 3 money" $ do
      (p1AfterCard ^. field @"money") `shouldBe` 3
      (p1AfterCard ^. field @"actions") `shouldBe` 0
    it "trashes a copper" $ do
      length ((p1AfterCard ^. field @"hand") ++ (p1AfterCard ^. field @"discard") ++ (p1AfterCard ^. field @"played") ++ (p1AfterCard ^. field @"deck")) `shouldBe` 9 -- the moneylender was not played
      length (afterCard ^. field @"trash") `shouldBe` 1

  describe "poacherCardAction" $ do
    it "gives a card, action, and money" $ do
      let afterCard = fst $ execRWS ((poacherCard ^. field @"action") poacherCard p0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      length (p1AfterCard ^. field @"hand") `shouldBe` 6
      (p1AfterCard ^. field @"money") `shouldBe` 1
      (p1AfterCard ^. field @"actions") `shouldBe` 1
    it "causes a discard per empty supply pile" $ do
      let preCard = set (field @"decks") (Map.fromList [ (copperCard, 46), (silverCard, 0), (goldCard, 30), (estateCard, 0), (duchyCard, 8), (provinceCard, 8) ]) afterDeal
      let afterCard = fst $ execRWS ((poacherCard ^. field @"action") poacherCard p0) c preCard
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      length (p1AfterCard ^. field @"hand") `shouldBe` 4
      (p1AfterCard ^. field @"money") `shouldBe` 1
      (p1AfterCard ^. field @"actions") `shouldBe` 1

  describe "throneRoomCardAction" $ do
    it "will not play if there are no actions left in the hand" $ do
      let afterCard = fst $ execRWS ((throneRoomCard ^. field @"action") throneRoomCard p0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      (p1AfterCard ^. field @"actions") `shouldBe` 1
      length (p1AfterCard ^. field @"hand") `shouldBe` 5
    it "will play Smithy twice in the bigSmithyStrategy" $ do
      let forcedDeal = DominionPlayer "Throne Room Deal" (replicate 5 copperCard) [] [throneRoomCard, smithyCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigSmithyStrategy
      let afterCard = fst $ execRWS (evaluateHand p0) c $ DominionGame [forcedDeal] (basicDecks 2) [] g
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      (p1AfterCard ^. field @"actions") `shouldBe` 0
      length (p1AfterCard ^. field @"hand") `shouldBe` 0
      length (p1AfterCard ^. field @"discard") `shouldBe` 0
      length (p1AfterCard ^. field @"deck") `shouldBe` 0
      length (p1AfterCard ^. field @"played") `shouldBe` 10
      (p1AfterCard ^. field @"money") `shouldBe` 7

  describe "banditCardAction" $ do
    it "gives a gold onto the discard pile" $ do
      let afterCard = fst $ execRWS ((banditCard ^. field @"action") banditCard p0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      length (p1AfterCard ^. field @"discard") `shouldBe` 1
      head (p1AfterCard ^. field @"discard") `shouldBe` goldCard
      (p1AfterCard ^. field @"actions") `shouldBe` 0
    it "trashes an opponent's silver" $ do
      let forcedDeal = DominionPlayer "Bandit Deal" (silverCard: replicate 5 copperCard) [] [estateCard, smithyCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigSmithyStrategy
      let (Just p1AfterDeal)  = afterDeal ^? field @"players" . ix 0
      let afterCard = fst $ execRWS ((banditCard ^. field @"action") banditCard p0) c $ DominionGame [p1AfterDeal, forcedDeal] (basicDecks 2) [] g
      let (Just fd) = afterCard ^? field @"players" . ix 1
      length (fd ^. field @"discard") `shouldBe` 1
      length (fd ^. field @"deck") `shouldBe` 4
      length (afterCard ^. field @"trash") `shouldBe` 1

  describe "councilRoomCardAction" $ do
    let afterCard = fst $ execRWS ((councilRoomCard ^. field @"action") councilRoomCard p0) c afterDeal2
    let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
    let (Just p2') = afterCard ^? field @"players" . ix 1
    it "draws four cards" $ do
      length (p1AfterCard ^. field @"hand") `shouldBe` 9
      length (p1AfterCard ^. field @"deck") `shouldBe` 1
    it "causes the other players to draw a card" $
      length (p2' ^. field @"hand") `shouldBe` 6

  describe "witchCardAction" $ do
    let afterCard = fst $ execRWS ((witchCard ^. field @"action") witchCard p0) c afterDeal2
    let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
    let (Just p2') = afterCard ^? field @"players" . ix 1
    it "draws two cards" $ do
      length (p1AfterCard ^. field @"hand") `shouldBe` 7
      (p1AfterCard ^. field @"actions") `shouldBe` 0
    it "causes other players to get curses" $ do
      p1AfterCard ^. #discard `shouldBe` []
      p2' ^. #discard `shouldBe` [curseCard]
      head (p2' ^. field @"discard") `shouldBe` curseCard

  describe "mineCardAction" $ do
    let afterCard = fst $ execRWS ((mineCard ^. field @"action") mineCard p0) c afterDeal
    let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
    it "upgrades a copper to silver" $
      head (p1AfterCard ^. field @"hand") `shouldBe` silverCard

  describe "libraryCardAction" $ do
    let afterCard = fst $ execRWS ((libraryCard ^. field @"action") libraryCard p0) c afterDeal
    let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
    it "draws to seven cards" $
      length (p1AfterCard ^. field @"hand") `shouldBe` 7

  describe "sentryCardAction" $ do
    let afterCard = fst $ execRWS ((sentryCard ^. field @"action") sentryCard p0) c afterDeal
    let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
    it "discards coppers and estates from the top of the deck" $ do
      length (p1AfterCard ^. field @"deck") `shouldBe` 2 -- draw one plus look at two and discard both
      (p1AfterCard ^. field @"actions") `shouldBe` 1

  describe "artisanCardAction" $ do
    let afterCard = fst $ execRWS ((artisanCard ^. field @"action") artisanCard p0) c afterDeal
    let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
    it "gains a card to the hand and puts another onto the deck" $ do
      length (p1AfterCard ^. field @"deck") `shouldBe` 6
      length (p1AfterCard ^. field @"hand") `shouldBe` 5
      (p1AfterCard ^. field @"actions") `shouldBe` 0

  describe "remodelCardAction" $ do
    let afterCard = fst $ execRWS ((remodelCard ^. field @"action") remodelCard p0) c afterDeal
    let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
    it "trashes a copper and gains a silver into the hand" $ do
      length (afterCard ^. field @"trash") `shouldBe` 1
      length (p1AfterCard ^. field @"hand") `shouldBe` 5
      length (p1AfterCard ^. field @"deck") `shouldBe` 5
      -- TODO: look up remodel card and make sure we're doing the right thing
--      silverCard `elem` (p1AfterCard ^. field @"hand") `shouldBe` True
