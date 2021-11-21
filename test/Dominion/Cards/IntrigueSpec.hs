{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}

module Dominion.Cards.IntrigueSpec
    ( spec
    ) where

import Control.Lens ( (^.), (^?), Ixed(ix), over )
import Control.Monad.State ( execState )
import Data.Generics.Product ( HasField(field) )
import Data.List ( delete )
import qualified Data.Map                               as Map
import DeckBuilding.Dominion
    ( basicDecks, configToGame, makeDecks )
import DeckBuilding.Dominion.Cards
    ( copperCard,
      curseCard,
      estateCard,
      firstGameKingdomCards,
      vassalCard,
      victoryCards, moatCard )
import DeckBuilding.Dominion.Cards.Utils ( gainCard )
import DeckBuilding.Dominion.Strategies.Basic
    ( bigMoneyCardWeight,
      bigMoneyDiscard,
      bigMoneyHandToDeck,
      bigMoneyLibrary,
      bigMoneyLurker,
      bigMoneyRetrieve,
      bigMoneySentry,
      bigMoneyStrategy,
      bigMoneyThroneRoom,
      bigMoneyTrash,
      bigSmithyBuy,
      bigSmithyStrategy,
      bigMoneyIsland,
      nextCardByWeight,
      bigMoneyAmbassador,
      bigMoneyEmbargo,
      bigMoneyHaven,
      bigMoneyNativeVillage,
      bigMoneyPearlDiver,
      bigMoneyLookout )
import DeckBuilding.Dominion.Types
    ( Card,
      DominionPlayer(DominionPlayer),
      DominionConfig(DominionConfig),
      Strategy(Strategy),
      DominionState,
      CardType(Action),
      DominionGame(DominionGame), DominionBoard(DominionBoard), DominionAIGame )
import DeckBuilding.Dominion.Utils ( deal )
import DeckBuilding.Types ( PlayerNumber(PlayerNumber) )
import System.Random ( mkStdGen )
import Test.Hspec ( shouldBe, it, describe, Spec )
import DeckBuilding.Dominion.Cards.Intrigue (courtyardCard, lurkerCard, shantyTownCard, conspiratorCard, ironworksCard, dukeCard)

gainAction :: DominionAIGame -> Int -> Maybe Card
gainAction _ = gainCard firstGameKingdomCards

gainVictory :: DominionAIGame -> Int -> Maybe Card
gainVictory _ = gainCard (delete curseCard victoryCards)

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
  let afterDeal           = execState (deal 5 p0) dg
  let (Just p1AfterDeal)  = afterDeal ^? #players . ix 0

  describe "courtyardCardAction" $ do
    let afterCard = execState ((courtyardCard ^. #action) p0) afterDeal
    let (Just p1AfterCard) = afterCard ^? #players . ix 0
    it "draws three cards and puts one back on the deck" $ do
      length (p1AfterCard ^. #deck) `shouldBe` 3
      length (p1AfterCard ^. #hand) `shouldBe` 7

  describe "lurkerCardAction" $ do
    let afterCard = execState ((lurkerCard ^. #action) p0) afterDeal
    it "trashes an action card from supply" $ do
      length (afterCard ^. #trash) `shouldBe` 1
      head (afterCard ^. #trash) ^. #cardType `shouldBe` Action

  describe "shantyTownCardAction" $ do
    let afterCard = execState ((shantyTownCard ^. #action) p0) afterDeal
    let (Just p1AfterCard) = afterCard ^? #players . ix 0
    it "gets two cards and two actions with no action cards in hand" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 7
      (p1AfterCard ^. #actions) `shouldBe` 2

  describe "conspiratorCardAction" $ do
    let afterCard = execState ((conspiratorCard ^. #action) p0) afterDeal
    let (Just p1AfterCard) = afterCard ^? #players . ix 0
    it "gets only two money when fewer than two actions have been played" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 5
      (p1AfterCard ^. #actions) `shouldBe` 0
      (p1AfterCard ^. #money) `shouldBe` 2
    it "gets one card, one action, and two cards when two actions have been played" $ do
      let (Just player1) = afterDeal ^? #players . ix 0
      let (Just player2) = afterDeal ^? #players . ix 1
      let p1Prepped = over (#played) ([conspiratorCard, conspiratorCard] ++) player1
      let afterConspirator = execState ((conspiratorCard ^. #action) p0) $ DominionBoard [p1Prepped, player2] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] (Map.fromList [] :: Map.Map Card Int) [moatCard] curseCard g
      let (Just p1AfterConspirator) = afterConspirator ^? #players . ix 0
      length (p1Prepped ^. #hand) `shouldBe` 5
      length (p1Prepped ^. #played) `shouldBe` 2
      length (p1AfterConspirator ^. #hand) `shouldBe` 6
      (p1AfterConspirator ^. #actions) `shouldBe` 1
      (p1AfterConspirator ^. #money) `shouldBe` 2

  describe "ironworksCardAction" $ do
    it "gets +action for an action card" $ do
      let forcedDeal = DominionPlayer "Ironworks Deal" (replicate 5 copperCard) [] [vassalCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 [] [] [] 0 $ Strategy "Ironworks Action" bigSmithyBuy bigMoneyDiscard bigMoneyTrash bigMoneyRetrieve (nextCardByWeight bigMoneyCardWeight) gainAction bigMoneyThroneRoom bigMoneyLibrary bigMoneySentry bigMoneyHandToDeck bigMoneyLurker bigMoneyIsland bigMoneyAmbassador bigMoneyEmbargo bigMoneyHaven bigMoneyNativeVillage bigMoneyPearlDiver bigMoneyLookout
      let afterCard = execState ((ironworksCard ^. #action) p1) $ DominionBoard [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] (Map.fromList [] :: Map.Map Card Int) [moatCard] curseCard g
      let (Just p2AfterCard) = afterCard ^? #players . ix 1
      (p2AfterCard ^. #actions) `shouldBe` 1
      (p2AfterCard ^. #money) `shouldBe` 0
      length (p2AfterCard ^. #hand) `shouldBe` 5
    it "gets +money for a treasure card" $ do
      let forcedDeal = DominionPlayer "Ironworks Deal" (replicate 5 copperCard) [] [copperCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 [] [] [] 0 bigMoneyStrategy
      let afterCard = execState ((ironworksCard ^. #action) p1) $ DominionBoard [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] (Map.fromList [] :: Map.Map Card Int) [moatCard] curseCard g
      let (Just p2AfterCard) = afterCard ^? #players . ix 1
      (p2AfterCard ^. #actions) `shouldBe` 0
      (p2AfterCard ^. #money) `shouldBe` 1
      length (p2AfterCard ^. #hand) `shouldBe` 5
    it "gets +card for a victory card" $ do
      let forcedDeal = DominionPlayer "Ironworks Deal" (replicate 5 copperCard) [] [estateCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 [] [] [] 0 $ Strategy "Ironworks Victory" bigSmithyBuy bigMoneyDiscard bigMoneyTrash bigMoneyRetrieve (nextCardByWeight bigMoneyCardWeight) gainVictory bigMoneyThroneRoom bigMoneyLibrary bigMoneySentry bigMoneyHandToDeck bigMoneyLurker bigMoneyIsland bigMoneyAmbassador bigMoneyEmbargo bigMoneyHaven bigMoneyNativeVillage bigMoneyPearlDiver bigMoneyLookout
      let afterCard = execState ((ironworksCard ^. #action) p1) $ DominionBoard [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] (Map.fromList [] :: Map.Map Card Int) [moatCard] curseCard g
      let (Just p2AfterCard) = afterCard ^? #players . ix 1
      (p2AfterCard ^. #actions) `shouldBe` 0
      (p2AfterCard ^. #money) `shouldBe` 0
      length (p2AfterCard ^. #hand) `shouldBe` 6

  describe "dukeCardAction" $ do
    it "is worth no points without duchies" $ do
      let afterCard = execState ((dukeCard ^. #action) p0) afterDeal
      let (Just p1AfterCard) = afterCard ^? #players . ix 0
      (p1AfterCard ^. #actions) `shouldBe` 1
      (p1AfterCard ^. #victory) `shouldBe` 0
    {- We don't calculate victory points like this anymore
    it "is worth one point per duchy" $ do
      let forcedDeal = DominionPlayer "Ironworks Deal" (replicate 5 copperCard) [duchyCard, duchyCard] [copperCard, estateCard, estateCard, copperCard, copperCard] [] 1 1 0 0 0 bigMoneyStrategy
      let afterCard = execState ((dukeCard ^. #action) dukeCard 1) $ DominionGame [p1AfterDeal, forcedDeal] (basicDecks 2 `Map.union` makeDecks firstGameKingdomCards) [] g
      let (Just p2AfterCard) = afterCard ^? #players . ix 1
      (p2AfterCard ^. #victory) `shouldBe` 2
      -}
