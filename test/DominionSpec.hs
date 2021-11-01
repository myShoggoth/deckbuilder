{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}

module DominionSpec
    ( spec
    ) where

import Control.Lens ( (^.), (^?), Ixed(ix) )
import Control.Monad.State ( evalState, execState )
import Data.Generics.Product ()
import DeckBuilding ()
import DeckBuilding.Dominion
    ( resetTurn, evaluateHand, configToGame, runPlayerTurns, runPlayerTurn )
import DeckBuilding.Dominion.Cards.Base
    ( estateCard, copperCard, firstGameKingdomCards )
import DeckBuilding.Dominion.Strategies.Basic
    ( bigSmithyStrategy, bigMoneyStrategy )
import DeckBuilding.Dominion.Types
    ( DominionState, DominionConfig(DominionConfig) )
import DeckBuilding.Dominion.Utils ( deal )
import DeckBuilding.Types ( PlayerNumber(PlayerNumber) )
import System.Random ( mkStdGen )
import Test.Hspec ( shouldBe, it, describe, Spec )

spec :: Spec
spec = do
  let g = mkStdGen 45752345316
  let c = DominionConfig
              [ ("Player 1", bigMoneyStrategy)
              , ("Player 2", bigSmithyStrategy)
              ]
              firstGameKingdomCards
  let p0 = PlayerNumber 0
      p1 = PlayerNumber 1
  let dg = configToGame c g
  let afterDeal               = execState (deal 5 p0) dg
  let (Just p1AfterDeal)      = afterDeal ^? #players . ix 0
  let afterDeal2              = execState (deal 5 p1) afterDeal
  let afterEvaluate           = execState (evaluateHand p0) afterDeal2
  let (Just p1AfterEvaluate)  = afterEvaluate ^? #players . ix 0
  let afterReset              = execState (resetTurn p0) afterEvaluate
  let (Just p1AfterReset)     = afterReset ^? #players . ix 0

  describe "Utils.deal" $ do
    it "deals the correct number of cards" $ do
      length (p1AfterDeal  ^. #hand) `shouldBe` 5
      length (p1AfterDeal ^. #deck) `shouldBe` 5
      length (p1AfterDeal ^. #discard) `shouldBe` 0

    it "has a total of seven copper" $
      length (filter (== copperCard) ((p1AfterDeal ^. #hand) ++ (p1AfterDeal ^. #deck))) `shouldBe` 7

    it "has a total of three estates" $
      length (filter (== estateCard) ((p1AfterDeal ^. #hand) ++ (p1AfterDeal ^. #deck))) `shouldBe` 3

  describe "evaluateHand" $ do
    it "has no more cards in hand" $ do
      p1AfterEvaluate ^. #hand `shouldBe` []
      p1AfterEvaluate ^. #played `shouldBe` [copperCard, estateCard, copperCard, copperCard, copperCard]
      length (p1AfterEvaluate ^. #played) `shouldBe` 5
      length (p1AfterEvaluate ^. #hand) `shouldBe` 0
      length (p1AfterDeal ^. #discard) `shouldBe` 0

    it "calculates the right amount of money" $
      p1AfterEvaluate ^. #money `shouldBe` length (filter (== copperCard) (p1AfterEvaluate ^. #played))

  describe "resetTurn" $ do
    it "has an empty played pile" $
      length (p1AfterReset ^. #played) `shouldBe` 0

    it "has zero money" $
      (p1AfterReset ^. #money) `shouldBe` 0

    it "has zero victory" $
      (p1AfterReset ^. #victory) `shouldBe` 0

    it "has an empty hand" $
      length (p1AfterReset ^. #hand) `shouldBe` 0

    it "has only one buy" $
      (p1AfterReset ^. #buys) `shouldBe` 1

    it "has only one action" $
      (p1AfterReset ^. #actions) `shouldBe` 1

  describe "doTurn" $ do
    let afterDoTurn = execState (runPlayerTurn p0) afterDeal2

    it "bought a card" $ do
      length (head (afterDoTurn ^. #players) ^. #discard) `shouldBe` 6

  describe "doTurns" $ do
    let afterDoTurns = execState (runPlayerTurns (PlayerNumber <$> [0..1])) afterDeal2
    it "has players with more cards" $ do
      length (head (afterDoTurns ^. #players) ^. #discard) `shouldBe` 6
