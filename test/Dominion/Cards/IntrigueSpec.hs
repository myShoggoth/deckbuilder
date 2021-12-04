{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE DisambiguateRecordFields  #-}

module Dominion.Cards.IntrigueSpec
    ( spec
    ) where

import Control.Lens ( (^.), (^?), Ixed(ix), over, (%=), (.=) )
import Control.Monad.State ( execState )
import Data.Generics.Product ( HasField(field) )
import Data.List ( delete )
import qualified Data.Map                               as Map
import DeckBuilding.Dominion
    ( basicDecks, configToGame, makeDecks, mkDominionAIGame )
import DeckBuilding.Dominion.Cards
    ( copperCard,
      curseCard,
      estateCard,
      firstGameKingdomCards,
      vassalCard,
      victoryCards, moatCard )
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
      bigMoneyLookout,
      bigMoneyNavigator,
      bigMoneyPirateShip,
      bigMoneyPirateShipDecision,
      bigMoneySalvage )
import DeckBuilding.Dominion.Types
    ( Card,
      DominionPlayer(DominionPlayer, strategy),
      DominionConfig(DominionConfig),
      Strategy(Strategy, gainCardStrategy),
      DominionState,
      CardType(Action),
      DominionGame(DominionGame), DominionBoard(DominionBoard), DominionAIGame )
import DeckBuilding.Dominion.Utils ( deal, findPlayer )
import DeckBuilding.Types ( PlayerNumber(PlayerNumber, unPlayerNumber) )
import System.Random ( mkStdGen )
import Test.Hspec ( shouldBe, it, describe, Spec )
import DeckBuilding.Dominion.Cards.Intrigue (courtyardCard, lurkerCard, shantyTownCard, conspiratorCard, ironworksCard, dukeCard)
import DeckBuilding.Dominion.Strategies.Utils (gainWhichCard)
import Dominion.Utils ( defaultConfig, initialState, p0, p1, setDeck, setHand )

gainAction :: DominionAIGame -> Int -> Maybe Card
gainAction _ = gainWhichCard firstGameKingdomCards

gainVictory :: DominionAIGame -> Int -> Maybe Card
gainVictory _ = gainWhichCard (delete curseCard victoryCards)

spec :: Spec
spec = do
  describe "courtyardCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          courtyardCard ^. #action $ p0
          findPlayer p0
    it "draws three cards and puts one back on the deck" $ do
      length (p1AfterCard ^. #deck) `shouldBe` 3
      length (p1AfterCard ^. #hand) `shouldBe` 7

  describe "lurkerCardAction" $ do
    let (_, afterCard) = initialState defaultConfig $ do
          lurkerCard ^. #action $ p0
    it "trashes an action card from supply" $ do
      length (afterCard ^. #trash) `shouldBe` 1
      head (afterCard ^. #trash) ^. #cardType `shouldBe` Action

  describe "shantyTownCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          shantyTownCard ^. #action $ p0
          findPlayer p0
    it "gets two cards and two actions with no action cards in hand" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 7
      (p1AfterCard ^. #actions) `shouldBe` 2

  describe "conspiratorCardAction" $ do
    it "gets only two money when fewer than two actions have been played" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            conspiratorCard ^. #action $ p0
            findPlayer p0
      length (p1AfterCard ^. #hand) `shouldBe` 5
      (p1AfterCard ^. #actions) `shouldBe` 0
      (p1AfterCard ^. #money) `shouldBe` 2
    it "gets one card, one action, and two cards when two actions have been played" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            #players . ix (unPlayerNumber p0) . #played %= ([conspiratorCard, conspiratorCard] ++)
            conspiratorCard ^. #action $ p0
            findPlayer p0
      length (p1AfterCard ^. #hand) `shouldBe` 6
      (p1AfterCard ^. #actions) `shouldBe` 1
      (p1AfterCard ^. #money) `shouldBe` 2

  describe "ironworksCardAction" $ do
    it "gets +action for an action card" $ do
      let (p2AfterCard, _) = initialState defaultConfig $ do
            #players . ix (unPlayerNumber p1) . #strategy . #gainCardStrategy .= gainAction 
            setHand p1 (replicate 5 copperCard)
            setDeck p1 [vassalCard, estateCard, estateCard, copperCard, copperCard]
            ironworksCard ^. #action $ p1
            findPlayer p1
      (p2AfterCard ^. #actions) `shouldBe` 1
      (p2AfterCard ^. #money) `shouldBe` 0
      length (p2AfterCard ^. #hand) `shouldBe` 5
    it "gets +money for a treasure card" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            setHand p0 (replicate 5 copperCard)
            setDeck p0 [copperCard, estateCard, estateCard, copperCard, copperCard]
            ironworksCard ^. #action $ p0
            findPlayer p0
      (p1AfterCard ^. #actions) `shouldBe` 0
      (p1AfterCard ^. #money) `shouldBe` 1
      length (p1AfterCard ^. #hand) `shouldBe` 5
    it "gets +card for a victory card" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            #players . ix (unPlayerNumber p0) . #strategy . #gainCardStrategy .= gainVictory
            setHand p0 (replicate 5 copperCard)
            setDeck p0 [estateCard, estateCard, estateCard, copperCard, copperCard]
            thePlayer <- findPlayer p0
            aig <- mkDominionAIGame p0
            let mc = (thePlayer ^. #strategy . #gainCardStrategy) aig 4
            ironworksCard ^. #action $ p0
            findPlayer p0
      (p1AfterCard ^. #actions) `shouldBe` 0
      (p1AfterCard ^. #money) `shouldBe` 0
      length (p1AfterCard ^. #hand) `shouldBe` 6

  describe "dukeCardAction" $ do
    it "is worth no points without duchies" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            dukeCard ^. #action $ p0
            findPlayer p0
      (p1AfterCard ^. #actions) `shouldBe` 1
      (p1AfterCard ^. #victory) `shouldBe` 0
