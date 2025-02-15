{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}

module Dominion.CardsSpec
    ( spec
    ) where

import Control.Lens ( (^.), (^?), Ixed(ix) )
import Control.Monad.State ( execState )
import Data.Generics.Product ()
import DeckBuilding.Dominion ( configToGame )
import DeckBuilding.Dominion.Cards
    ( copperCard,
      firstGameKingdomCards,
      goldCard,
      silverCard,
      festivalCard,
      laboratoryCard,
      marketCard,
      moatCard,
      smithyCard,
      villageCard )
import DeckBuilding.Dominion.Strategies.Basic
    ( bigSmithyStrategy, bigMoneyStrategy )
import DeckBuilding.Dominion.Types
    ( DominionConfig(DominionConfig) )
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
  let dg = configToGame c g
  let afterDeal     = execState (deal 5 p0) dg
  describe "Utils.valueCard" $ do
    it "gives money for a copper" $ do
      let afterCard = execState ((copperCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      (p1AfterCard ^. #money) `shouldBe` 1

    it "gives money for a silver" $ do
      let afterCard = execState ((silverCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      (p1AfterCard ^. #money) `shouldBe` 2

    it "gives money for a gold" $ do
      let afterCard = execState ((goldCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      (p1AfterCard ^. #money) `shouldBe` 3

  describe "Utils.basicCardAction" $ do
    it "it works with market" $ do
      let afterCard = execState ((marketCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      length (p1AfterCard ^. #hand) `shouldBe` 6
      (p1AfterCard ^. #actions) `shouldBe` 1
      (p1AfterCard ^. #buys) `shouldBe` 2
      (p1AfterCard ^. #money) `shouldBe` 1

    it "it works with moat" $ do
      let afterCard = execState ((moatCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      length (p1AfterCard ^. #hand) `shouldBe` 7
      (p1AfterCard ^. #actions) `shouldBe` 0

    it "it works with smithy" $ do
      let afterCard = execState ((smithyCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      length (p1AfterCard ^. #hand) `shouldBe` 8
      (p1AfterCard ^. #actions) `shouldBe` 0

    it "it works with village" $ do
      let afterCard = execState ((villageCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      length (p1AfterCard ^. #hand) `shouldBe` 6
      (p1AfterCard ^. #actions) `shouldBe` 2

    it "it works with festival" $ do
      let afterCard = execState ((festivalCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      (p1AfterCard ^. #actions) `shouldBe` 2
      (p1AfterCard ^. #buys) `shouldBe` 2
      (p1AfterCard ^. #money) `shouldBe` 2

    it "it works with laboratory" $ do
      let afterCard = execState ((laboratoryCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      length (p1AfterCard ^. #hand) `shouldBe` 7
      (p1AfterCard ^. #actions) `shouldBe` 1
