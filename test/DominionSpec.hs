{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}

module DominionSpec
    ( spec
    ) where

import           Control.Lens
import           Control.Monad.RWS
import           Data.Generics.Product
import           DeckBuilding
import           DeckBuilding.Dominion
import           DeckBuilding.Dominion.Cards.Base
import           DeckBuilding.Dominion.Strategies.Basic
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils
import           DeckBuilding.Types
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
  let p0 = PlayerNumber 0
      p1 = PlayerNumber 1
  let dg = configToGame c g
  let afterDeal               = fst $ execRWS (deal 5 p0) c dg
  let (Just p1AfterDeal)      = afterDeal ^? field @"players" . ix 0
  let afterDeal2              = fst $ execRWS (deal 5 p1) c afterDeal
  let afterEvaluate           = fst $ execRWS (evaluateHand p0) c afterDeal2
  let (Just p1AfterEvaluate)  = afterEvaluate ^? field @"players" . ix 0
  let afterReset              = fst $ execRWS (resetTurn p0) c afterEvaluate
  let (Just p1AfterReset)     = afterReset ^? field @"players" . ix 0

  describe "Utils.deal" $ do
    it "deals the correct number of cards" $ do
      length (p1AfterDeal  ^. field @"hand") `shouldBe` 5
      length (p1AfterDeal ^. field @"deck") `shouldBe` 5
      length (p1AfterDeal ^. field @"discard") `shouldBe` 0

    it "has a total of seven copper" $
      length (filter (== copperCard) ((p1AfterDeal ^. field @"hand") ++ (p1AfterDeal ^. field @"deck"))) `shouldBe` 7

    it "has a total of three estates" $
      length (filter (== estateCard) ((p1AfterDeal ^. field @"hand") ++ (p1AfterDeal ^. field @"deck"))) `shouldBe` 3

  describe "evaluateHand" $ do
    it "has no more cards in hand" $ do
      length (p1AfterEvaluate ^. field @"played") `shouldBe` 5
      length (p1AfterEvaluate ^. field @"hand") `shouldBe` 0
      length (p1AfterDeal ^. field @"discard") `shouldBe` 0

    it "calculates the right amount of money" $
      p1AfterEvaluate ^. field @"money" `shouldBe` length (filter (== copperCard) (p1AfterEvaluate ^. field @"played"))

  describe "resetTurn" $ do
    it "has an empty played pile" $
      length (p1AfterReset ^. field @"played") `shouldBe` 0

    it "has zero money" $
      (p1AfterReset ^. field @"money") `shouldBe` 0

    it "has zero victory" $
      (p1AfterReset ^. field @"victory") `shouldBe` 0

    it "has an empty hand" $
      length (p1AfterReset ^. field @"hand") `shouldBe` 0

    it "has only one buy" $
      (p1AfterReset ^. field @"buys") `shouldBe` 1

    it "has only one action" $
      (p1AfterReset ^. field @"actions") `shouldBe` 1

  describe "doTurn" $ do
    let afterDoTurn = fst $ execRWS ((runTurn p0) :: DominionState Bool) c afterDeal2

    it "bought a card" $ do
      length ((afterDoTurn ^. field @"players") !! 0 ^. field @"discard") `shouldBe` 6

  describe "doTurns" $ do
    let afterDoTurns = fst $ execRWS ((runTurns (PlayerNumber <$> [0..1]) False) :: DominionState Bool) c afterDeal2
    it "has players with more cards" $ do
      length ((afterDoTurns ^. field @"players") !! 0 ^. field @"discard") `shouldBe` 6
