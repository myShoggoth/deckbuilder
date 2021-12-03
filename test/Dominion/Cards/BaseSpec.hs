{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Dominion.Cards.BaseSpec
    ( spec
    ) where

import Control.Lens ( (^?), (^.), (%=), (.=), set, Ixed(ix), set' )
import Control.Monad.State ( execState, runState )
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
import DeckBuilding.Dominion.Utils ( deal, cardPlayed, findPlayer )
import DeckBuilding.Types ( PlayerNumber(PlayerNumber, unPlayerNumber), Game (start) )
import System.Random ( mkStdGen )
import Test.Hspec ( Spec, describe, it, shouldBe )
import DeckBuilding.Dominion.Cards.Base (poacherCard)
import DeckBuilding.Dominion.Pretty (witchResponse)

defaultConfig :: DominionConfig
defaultConfig = DominionConfig
  [ ("Player 1", bigMoneyStrategy)
  , ("Player 2", bigSmithyStrategy)
  ]
  firstGameKingdomCards

initialState :: DominionConfig -> DominionState a -> (a, DominionBoard)
initialState c f = runState f $ execState start $ configToGame c $ mkStdGen 45752345316

p0 :: PlayerNumber
p0 = PlayerNumber 0

p1 :: PlayerNumber
p1 = PlayerNumber 1

setHand :: PlayerNumber -> [Card] -> DominionState ()
setHand p xs = #players . ix (unPlayerNumber p) . #hand .= xs

setDeck :: PlayerNumber -> [Card] -> DominionState ()
setDeck p xs = #players . ix (unPlayerNumber p) . #deck .= xs

spec :: Spec
spec = do
  describe "cellarCardAction" $
    it "discards all starting cards" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            cellarCard ^. #action $ p0
            findPlayer p0
      length (p1AfterCard ^. #hand) `shouldBe` 5
      length (p1AfterCard ^. #discard) `shouldBe` 5
      (p1AfterCard ^. #actions) `shouldBe` 1

  describe "chapelCardAction" $
    it "trashes 4 of the starting cards" $ do
      let (p1AfterCard, afterCard) = initialState defaultConfig $ do
            chapelCard ^. #action $ p0
            findPlayer p0
      length (p1AfterCard ^. #hand) `shouldBe` 1
      length (p1AfterCard ^. #discard) `shouldBe` 0
      length (p1AfterCard ^. #played ++ p1AfterCard ^. #discard ++ p1AfterCard ^. #hand ++ p1AfterCard ^. #deck) `shouldBe` 6
      (p1AfterCard ^. #actions) `shouldBe` 0
      length (afterCard ^. #trash) `shouldBe` 4

  describe "harbingerCardAction" $
    it "takes a silver from the discard pile and puts it on the deck" $ do
      let (p1AfterCard, afterCard) = initialState defaultConfig $ do
            #players . ix 0 . #discard %= (silverCard:)
            harbingerCard ^. #action $ p0
            findPlayer p0
      head (p1AfterCard ^. #deck) `shouldBe` silverCard
      length (p1AfterCard ^. #hand) `shouldBe` 6
      (p1AfterCard ^. #actions) `shouldBe` 1

  describe "merchantCardAction" $ do
    it "adds no money if there are no silver played" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            merchantCard ^. #action $ p0
            findPlayer p0
      (p1AfterCard ^. #money) `shouldBe` 0
      (p1AfterCard ^. #actions) `shouldBe` 1
    it "adds one money if there has been a silver played" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            silverCard ^. #action $ p0
            cardPlayed silverCard p0
            merchantCard ^. #action $ p0
            findPlayer p0
      (p1AfterCard ^. #money) `shouldBe` 3 -- 2 for silver, 1 for merchant
      (p1AfterCard ^. #actions) `shouldBe` 1
    it "adds one money if a silver is played after" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            merchantCard ^. #action $ p0
            cardPlayed merchantCard p0
            silverCard ^. #action $ p0
            findPlayer p0
      (p1AfterCard ^. #money) `shouldBe` 3
      (p1AfterCard ^. #actions) `shouldBe` 1

  describe "vassalCardAction" $
    it "draws a value card" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            setHand p0 [vassalCard, estateCard, estateCard, copperCard, copperCard]
            evaluateHand p0
            findPlayer p0
      (p1AfterCard ^. #money) `shouldBe` 5
      length (p1AfterCard ^. #hand) `shouldBe` 0
      length (p1AfterCard ^. #played) `shouldBe` 6
      length (p1AfterCard ^. #deck) `shouldBe` 4
      length (p1AfterCard ^. #discard) `shouldBe` 1
      (p1AfterCard ^. #actions) `shouldBe` 0

  describe "bureaucratCardAction" $ do
    let ((p1AfterCard, p2AfterCard), _) = initialState defaultConfig $ do
          setHand p0 [vassalCard, estateCard, estateCard, copperCard, copperCard]
          bureaucratCard ^. #action $ p0
          p0' <- findPlayer p0
          p1' <- findPlayer p1
          return (p0', p1')
    it "puts a silver on the deck" $ do
      head (p1AfterCard ^. #deck) `shouldBe` silverCard
      (p1AfterCard ^. #actions) `shouldBe` 0
    it "makes other players discard a victory card" $ do
      length (p2AfterCard ^. #hand) `shouldBe` 4

  describe "militiaCardAction" $ do
    let ((p1AfterCard, p2AfterCard), afterCard) = initialState defaultConfig $ do
          militiaCard ^. #action $ p0
          p0' <- findPlayer p0
          p1' <- findPlayer p1
          return (p0', p1')
    it "gives two money" $ do
      (p1AfterCard ^. #money) `shouldBe` 2
      (p1AfterCard ^. #actions) `shouldBe` 0
    it "makes other players discard down to three cards" $ do
      length (p2AfterCard ^. #hand) `shouldBe` 3

  describe "moneylenderCardAction" $ do
    let (p1AfterCard, afterCard) = initialState defaultConfig $ do
          moneylenderCard ^. #action $ p0
          findPlayer p0
    it "gives 3 money" $ do
      (p1AfterCard ^. #money) `shouldBe` 3
      (p1AfterCard ^. #actions) `shouldBe` 0
    it "trashes a copper" $ do
      length ((p1AfterCard ^. #hand) ++ (p1AfterCard ^. #discard) ++ (p1AfterCard ^. #played) ++ (p1AfterCard ^. #deck)) `shouldBe` 9 -- the moneylender was not played
      length (afterCard ^. #trash) `shouldBe` 1

  describe "poacherCardAction" $ do
    it "gives a card, action, and money" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            poacherCard ^. #action $ p0
            findPlayer p0
      length (p1AfterCard ^. #hand) `shouldBe` 6
      (p1AfterCard ^. #money) `shouldBe` 1
      (p1AfterCard ^. #actions) `shouldBe` 1
    it "causes a discard per empty supply pile" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            #decks .= Map.fromList [ (copperCard, 46), (silverCard, 0), (goldCard, 30), (estateCard, 0), (duchyCard, 8), (provinceCard, 8) ]
            poacherCard ^. #action $ p0
            findPlayer p0
      length (p1AfterCard ^. #hand) `shouldBe` 4
      (p1AfterCard ^. #money) `shouldBe` 1
      (p1AfterCard ^. #actions) `shouldBe` 1

  describe "throneRoomCardAction" $ do
    it "will not play if there are no actions left in the hand" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            throneRoomCard ^. #action $ p0
            findPlayer p0
      (p1AfterCard ^. #actions) `shouldBe` 1
      length (p1AfterCard ^. #hand) `shouldBe` 5
    it "will play Smithy twice in the bigSmithyStrategy" $ do
      let (p2AfterCard, _) = initialState defaultConfig $ do
            setHand p1 [throneRoomCard, smithyCard, estateCard, copperCard, copperCard]
            evaluateHand p1
            findPlayer p1
      (p2AfterCard ^. #actions) `shouldBe` 0
      length (p2AfterCard ^. #hand) `shouldBe` 0
      length (p2AfterCard ^. #discard) `shouldBe` 0
      length (p2AfterCard ^. #played) `shouldBe` 10
      length (p2AfterCard ^. #deck) `shouldBe` 0
      (p2AfterCard ^. #money) `shouldBe` 6

  describe "banditCardAction" $ do
    it "gives a gold onto the discard pile" $ do
      let (p1AfterCard, _) = initialState defaultConfig $ do
            banditCard ^. #action $ p0
            findPlayer p0
      length (p1AfterCard ^. #discard) `shouldBe` 1
      head (p1AfterCard ^. #discard) `shouldBe` goldCard
      (p1AfterCard ^. #actions) `shouldBe` 0
    it "trashes an opponent's silver" $ do
      let (p2AfterCard, afterCard) = initialState defaultConfig $ do
            setHand p1 [estateCard, smithyCard, estateCard, copperCard, copperCard]
            setDeck p1 $ silverCard : replicate 5 copperCard
            banditCard ^. #action $ p0
            findPlayer p1
      length (p2AfterCard ^. #discard) `shouldBe` 1
      length (p2AfterCard ^. #deck) `shouldBe` 4
      length (afterCard ^. #trash) `shouldBe` 1

  describe "councilRoomCardAction" $ do
    let ((p1AfterCard, p2AfterCard), _) = initialState defaultConfig $ do
          councilRoomCard ^. #action $ p0
          p0' <- findPlayer p0
          p1' <- findPlayer p1
          return (p0', p1')
    it "draws four cards" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 9
      length (p1AfterCard ^. #deck) `shouldBe` 1
    it "causes the other players to draw a card" $
      length (p2AfterCard ^. #hand) `shouldBe` 6

  describe "witchCardAction" $ do
    let ((p1AfterCard, p2AfterCard), _) = initialState defaultConfig $ do
          witchCard ^. #action $ p0
          p0' <- findPlayer p0
          p1' <- findPlayer p1
          return (p0', p1')
    it "draws two cards" $ do
      length (p1AfterCard ^. #hand) `shouldBe` 7
      (p1AfterCard ^. #actions) `shouldBe` 0
    it "causes other players to get curses" $ do
      p1AfterCard ^. #discard `shouldBe` []
      p2AfterCard ^. #discard `shouldBe` [curseCard]
      head (p2AfterCard ^. #discard) `shouldBe` curseCard

  describe "mineCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          mineCard ^. #action $ p0
          findPlayer p0
    it "upgrades a copper to silver" $
      head (p1AfterCard ^. #hand) `shouldBe` silverCard

  describe "libraryCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          libraryCard ^. #action $ p0
          findPlayer p0
    it "draws to seven cards" $
      length (p1AfterCard ^. #hand) `shouldBe` 7

  describe "sentryCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          sentryCard ^. #action $ p0
          findPlayer p0
    it "discards coppers and estates from the top of the deck" $ do
      length (p1AfterCard ^. #deck) `shouldBe` 2 -- draw one plus look at two and discard both
      (p1AfterCard ^. #actions) `shouldBe` 1

  describe "artisanCardAction" $ do
    let (p1AfterCard, _) = initialState defaultConfig $ do
          artisanCard ^. #action $ p0
          findPlayer p0
    it "gains a card to the hand and puts another onto the deck" $ do
      length (p1AfterCard ^. #deck) `shouldBe` 6
      length (p1AfterCard ^. #hand) `shouldBe` 5
      (p1AfterCard ^. #actions) `shouldBe` 0

  describe "remodelCardAction" $ do
    let (p1AfterCard, afterCard) = initialState defaultConfig $ do
          remodelCard ^. #action $ p0
          findPlayer p0
    it "trashes a copper and gains a silver into the hand" $ do
      length (afterCard ^. #trash) `shouldBe` 1
      length (p1AfterCard ^. #hand) `shouldBe` 4
      length (p1AfterCard ^. #discard) `shouldBe` 1
      length (p1AfterCard ^. #deck) `shouldBe` 5
