{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}

module LegendarySpec
    ( spec
    ) where

import           Control.Lens
import           Control.Monad.RWS
import           Data.Generics.Product
import           DeckBuilding
import           DeckBuilding.Legendary
import           DeckBuilding.Legendary.Cards.Base
import           DeckBuilding.Legendary.Strategies.Basic
import           DeckBuilding.Legendary.Types
import           DeckBuilding.Legendary.Utils
import           DeckBuilding.Types
import           System.Random
import           Test.Hspec

spec :: Spec
spec = do
  let g = mkStdGen 45752345316
  let c = LegendaryConfig
              [ ("Player 1", dumbStrategy)
              , ("Player 2", dumbStrategy)
              ]
              1
              []
              [doombots, mastersOfEvil, mastersOfEvil]
              (take 30 $ repeat bystander)
              defaultCity
              legacyVirus
              drdoom
              [g]
  let dg = configToGame c g
  let afterDeal               = fst $ execRWS (deal 6 0) c dg
  let (Just p1AfterDeal)      = afterDeal ^? field @"players" . ix 0
  let afterDeal2              = fst $ execRWS (deal 6 1) c afterDeal
  let afterEvaluate           = fst $ execRWS (evaluateHand 0) c afterDeal2
  let (Just p1AfterEvaluate)  = afterEvaluate ^? field @"players" . ix 0
  let afterReset              = fst $ execRWS (resetTurn 0) c afterEvaluate
  let (Just p1AfterReset)     = afterReset ^? field @"players" . ix 0

  describe "Utils.deal" $ do
    it "deals the correct number of cards" $ do
      length (p1AfterDeal  ^. field @"hand") `shouldBe` 6
      length (p1AfterDeal ^. field @"deck") `shouldBe` 6
      length (p1AfterDeal ^. field @"discard") `shouldBe` 0

    it "has a total of eight shield agents" $
      length (filter (== shieldAgent) ((p1AfterDeal ^. field @"hand") ++ (p1AfterDeal ^. field @"deck"))) `shouldBe` 8

    it "has a total of four field troopers" $
      length (filter (== shieldTrooper) ((p1AfterDeal ^. field @"hand") ++ (p1AfterDeal ^. field @"deck"))) `shouldBe` 4

  describe "evaluateHand" $ do
    it "has no more cards in hand" $ do
      length (p1AfterEvaluate ^. field @"played") `shouldBe` 6
      length (p1AfterEvaluate ^. field @"hand") `shouldBe` 0
      length (p1AfterDeal ^. field @"discard") `shouldBe` 0

    it "calculates the right amount of money" $
      p1AfterEvaluate ^. field @"unusedMoney" `shouldBe` length (filter (== shieldAgent) (p1AfterEvaluate ^. field @"played"))

  {- Don't calculate victory points as we go anymore
    it "calculates the right amount of victory" $
      p1AfterEvaluate ^. field @"victory" `shouldBe` length (filter (== estateCard) (p1AfterEvaluate ^. field @"played"))
      -}

  describe "resetTurn" $ do
    it "has an empty played pile" $
      length (p1AfterReset ^. field @"played") `shouldBe` 0

    it "has zero money" $
      (p1AfterReset ^. field @"unusedMoney") `shouldBe` 0

    it "has zero victory" $
      (p1AfterReset ^. field @"victory") `shouldBe` 0

    it "has an empty hand" $
      length (p1AfterReset ^. field @"hand") `shouldBe` 0

  describe "doTurn" $ do
    let afterDoTurn = fst $ execRWS ((runTurn 0) :: LegendaryState Bool) c afterDeal2

    it "bought a card" $ do
      length ((afterDoTurn ^. field @"players") !! 0 ^. field @"discard") `shouldBe` 6

  describe "doTurns" $ do
    let afterDoTurns = fst $ execRWS ((runTurns [0..1] False) :: LegendaryState Bool) c afterDeal2
    it "has players with more cards" $ do
      length ((afterDoTurns ^. field @"players") !! 0 ^. field @"discard") `shouldBe` 6
