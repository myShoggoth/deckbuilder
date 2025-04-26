{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}

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
      duchyCard,
      provinceCard,
      silverCard,
      goldCard,
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
import DeckBuilding.Dominion.Cards.Intrigue (baronCard, courtyardCard, lurkerCard, pawnCard, masqueradeCard, stewardCard, shantyTownCard, swindlerCard, conspiratorCard, ironworksCard, dukeCard, wishingWellCard, bridgeCard, diplomatCard, upgradeCard, millCard, miningVillageCard, secretPassageCard, noblesCard, patrolCard)
import DeckBuilding.Dominion.Strategies.Utils (gainWhichCard)
import Dominion.Utils ( defaultConfig, initialState, p0, p1, setDeck, setHand )

gainAction :: DominionAIGame -> Int -> Maybe Card
gainAction _ = gainWhichCard firstGameKingdomCards

gainVictory :: DominionAIGame -> Int -> Maybe Card
gainVictory _ = gainWhichCard (delete curseCard victoryCards)

spec :: Spec
spec = do
  describe "baronCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          baronCard ^. #action $ p0
          findPlayer p0
    it "adds a buy and four moneys for discarding an estate" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 4
      length (p1AfterCard ^. #discard) `shouldBe` 1
      p1AfterCard ^. #buys `shouldBe` 2
      p1AfterCard ^. #money `shouldBe` 4

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

  describe "pawnCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          pawnCard ^. #action $ p0
          findPlayer p0
    it "draws a card and adds an action" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 6
      p1AfterCard ^. #actions `shouldBe` 1

  describe "masqueradeCardAction" $ do
    let (p1AfterCard, afterCard) = initialState defaultConfig $ do
          masqueradeCard ^. #action $ p0
          findPlayer p0
    it "draws two cards, passing one to the left, then trashes a card" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 6
      length (afterCard ^. #trash) `shouldBe` 1

  describe "stewardCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          stewardCard ^. #action $ p0
          findPlayer p0
    it "draws two cards" $
      length (p1AfterCard ^. #hand) `shouldBe` 7

  describe "shantyTownCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          shantyTownCard ^. #action $ p0
          findPlayer p0
    it "gets two cards and two actions with no action cards in hand" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 7
      (p1AfterCard ^. #actions) `shouldBe` 2

  describe "swindlerAction" $ do
    let (p1AfterCard, afterCard) = initialState defaultConfig $ do
          swindlerCard ^. #action $ p0
          findPlayer p0

    it "gives two money" $
      (p1AfterCard ^. #money) `shouldBe` 2
    
    it "trashes the top of the deck of the other player and replaces with a card of the same cost" $ do
      length (afterCard ^. #players . ix 1 . #deck) `shouldBe` 4
      length (afterCard ^. #players . ix 1 . #discard) `shouldBe` 1

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

  describe "wishingWellCardAction" $ do
    it "draws a card, looks at top of deck and guesses" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            wishingWellCard ^. #action $ p0
            findPlayer p0
      length (p1AfterCard ^. #hand) `shouldBe` 6

  describe "bridgeCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          bridgeCard ^. #action $ p0
          findPlayer p0
    it "adds +1 Buy and +1 Money" $ do
      p1AfterCard ^. #buys `shouldBe` 2
      p1AfterCard ^. #money `shouldBe` 1

  describe "diplomatCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          diplomatCard ^. #action $ p0
          findPlayer p0
    it "draws 2 cards and does not add +2 Actions if hand size > 5" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 7
      p1AfterCard ^. #actions `shouldBe` 0

    let (p1AfterCard', _) = initialState defaultConfig $ do
          setHand p0 (replicate 3 copperCard)
          diplomatCard ^. #action $ p0
          findPlayer p0
  
    it "draws 2 cards and adds +2 Actions if hand size <= 5" $ do
      length (p1AfterCard' ^. #hand) `shouldBe` 5
      p1AfterCard' ^. #actions `shouldBe` 2

  describe "upgradeCardAction" $ do
    let (p1AfterCard, afterCard) = initialState defaultConfig $ do
          upgradeCard ^. #action $ p0
          findPlayer p0
    it "trashes a card and gains a card costing up to $2 more" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 5
      length (afterCard ^. #trash) `shouldBe` 1

  describe "millCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          millCard ^. #action $ p0
          findPlayer p0
    it "draws a card and optionally discards a treasure for +2 money" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 5
      p1AfterCard ^. #money `shouldBe` 2

  describe "miningVillageCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          miningVillageCard ^. #action $ p0
          findPlayer p0
    it "adds +2 actions and gains a card costing up to $4" $ do
      p1AfterCard ^. #actions `shouldBe` 2
      length (p1AfterCard ^. #discard) `shouldBe` 1

  describe "secretPassageCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          secretPassageCard ^. #action $ p0
          findPlayer p0
    it "draws 2 cards, puts one into hand and one on top of the deck" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 6
      length (p1AfterCard ^. #deck) `shouldBe` 4

  describe "noblesCardAction" $ do
    it "draws three cards when that option is chosen" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            setDeck p0 [copperCard, copperCard, copperCard, copperCard, copperCard]
            noblesCard ^. #action $ p0
            findPlayer p0
      length (p1AfterCard ^. #hand) `shouldBe` 8  -- 5 starting + 3 drawn
      p1AfterCard ^. #actions `shouldBe` 0

    it "gets two actions when that option is chosen" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            #players . ix (unPlayerNumber p0) . #strategy . #noblesStrategy .= const False
            noblesCard ^. #action $ p0
            findPlayer p0
      length (p1AfterCard ^. #hand) `shouldBe` 5  -- No cards drawn
      p1AfterCard ^. #actions `shouldBe` 2

  describe "patrolCardAction" $ do
    it "draws three cards and puts victory cards in hand" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            setDeck p0 [estateCard, copperCard, duchyCard, copperCard, provinceCard, copperCard, copperCard]  -- Need 7 cards: 3 for draw + 4 to inspect
            patrolCard ^. #action $ p0
            findPlayer p0
      (p1AfterCard ^. #deck) `shouldBe` [copperCard, copperCard, copperCard]
      length (p1AfterCard ^. #hand) `shouldBe` 9  -- 5 starting + 3 drawn + 1 victory card from reveal
      estateCard `elem` (p1AfterCard ^. #hand) `shouldBe` True
      duchyCard `elem` (p1AfterCard ^. #hand) `shouldBe` True
      provinceCard `elem` (p1AfterCard ^. #hand) `shouldBe` True

    it "puts non-victory cards back in chosen order" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            setDeck p0 [copperCard, silverCard, goldCard, copperCard, copperCard, copperCard, copperCard]  -- Need 7 cards: 3 for draw + 4 to inspect
            patrolCard ^. #action $ p0
            findPlayer p0
      length (p1AfterCard ^. #hand) `shouldBe` 8  -- 5 starting + 3 drawn (no victory cards in reveal)
      length (p1AfterCard ^. #deck) `shouldBe` 4  -- The 4 non-victory cards put back on deck
