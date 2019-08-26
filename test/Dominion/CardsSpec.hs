{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}

module Dominion.CardsSpec
    ( spec
    ) where

import           Control.Lens
import           Control.Monad.RWS
import           Data.Generics.Product
import           DeckBuilding.Dominion
import           DeckBuilding.Dominion.Cards
import           DeckBuilding.Dominion.Strategies.Basic
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils
import           System.Random
import           Test.Hspec

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
  describe "Utils.valueCard" $ do
    it "gives money for a copper" $ do
      let afterCard = fst $ execRWS ((copperCard ^. field @"action") copperCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      (p1AfterCard ^. field @"money") `shouldBe` 1

    it "gives money for a silver" $ do
      let afterCard = fst $ execRWS ((silverCard ^. field @"action") silverCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      (p1AfterCard ^. field @"money") `shouldBe` 2

    it "gives money for a gold" $ do
      let afterCard = fst $ execRWS ((goldCard ^. field @"action") goldCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      (p1AfterCard ^. field @"money") `shouldBe` 3

  -- Changed how scoring works, only happens at the end of the game now, and only for
  -- cards in hand.  More correct, harder to test, need to rethink this section.
  {-
    it "gives victory for an estate" $ do
      let afterCard = fst $ execRWS ((estateCard ^. field @"action") estateCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      (p1AfterCard ^. field @"victory") `shouldBe` 1

    it "gives victory for a duchy" $ do
      let afterCard = fst $ execRWS ((duchyCard ^. field @"action") duchyCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      (p1AfterCard ^. field @"victory") `shouldBe` 3

    it "gives victory for a province" $ do
      let afterCard = fst $ execRWS ((provinceCard ^. field @"action") provinceCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      (p1AfterCard  ^. field @"victory") `shouldBe` 6

    it "takes victory for a curse" $ do
      let afterCard = fst $ execRWS ((curseCard ^. field @"action") curseCard 0) c afterDeal
      let afterCardDone = fst $ execRWS ((tallyPoints 0) :: DominionState ()) c afterCard
      let (Just p1AfterCard) = afterCardDone ^? field @"players" . ix 0
      (p1AfterCard ^. field @"victory") `shouldBe` (-1)
      -}

  describe "Utils.basicCardAction" $ do
    it "it works with market" $ do
      let afterCard = fst $ execRWS ((marketCard ^. field @"action") marketCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      length (p1AfterCard ^. field @"hand") `shouldBe` 6
      (p1AfterCard ^. field @"actions") `shouldBe` 1
      (p1AfterCard ^. field @"buys") `shouldBe` 2
      (p1AfterCard ^. field @"money") `shouldBe` 1

    it "it works with moat" $ do
      let afterCard = fst $ execRWS ((moatCard ^. field @"action") moatCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      length (p1AfterCard ^. field @"hand") `shouldBe` 7
      (p1AfterCard ^. field @"actions") `shouldBe` 0

    it "it works with smithy" $ do
      let afterCard = fst $ execRWS ((smithyCard ^. field @"action") smithyCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      length (p1AfterCard ^. field @"hand") `shouldBe` 8
      (p1AfterCard ^. field @"actions") `shouldBe` 0

    it "it works with village" $ do
      let afterCard = fst $ execRWS ((villageCard ^. field @"action") villageCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      length (p1AfterCard ^. field @"hand") `shouldBe` 6
      (p1AfterCard ^. field @"actions") `shouldBe` 2

    it "it works with festival" $ do
      let afterCard = fst $ execRWS ((festivalCard ^. field @"action") festivalCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      (p1AfterCard ^. field @"actions") `shouldBe` 2
      (p1AfterCard ^. field @"buys") `shouldBe` 2
      (p1AfterCard ^. field @"money") `shouldBe` 2

    it "it works with laboratory" $ do
      let afterCard = fst $ execRWS ((laboratoryCard ^. field @"action") laboratoryCard 0) c afterDeal
      let (Just p1AfterCard) = afterCard ^? field @"players" . ix 0
      length (p1AfterCard ^. field @"hand") `shouldBe` 7
      (p1AfterCard ^. field @"actions") `shouldBe` 1
