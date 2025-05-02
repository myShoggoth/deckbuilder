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
      victoryCards, moatCard,
      tradingPostCard,
      replaceCard,
      courtierCard,
      smithyCard )
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
      bigMoneySalvage,
      bigMoneyCourtierBonus,
      bigMoneyCourtierReveal )
import DeckBuilding.Dominion.Types
    ( Card(Card, action, cardType, victoryPoints, numImplicitTypes, cost)
    , DominionPlayer(DominionPlayer)
    , DominionConfig(DominionConfig, playerDefs, kingdomCards)
    , Strategy( Strategy -- Constructor
              , trashStrategy -- Field used in updates
              , gainCardStrategy -- Field used in updates
              , courtierRevealStrategy -- Field used in updates
              , courtierBonusStrategy -- Field used in updates
              )
    , DominionState,
      DominionGame(DominionGame), DominionBoard(DominionBoard), DominionAIGame
    , DominionAction (TradingPost, Replace, Courtier, Minion, Torturer)
    , CardType (Action, Value, Victory, CurseType)
    , CardLocation(..)
    , CourtierChoice (..)
    )
import DeckBuilding.Dominion.Utils ( deal, findPlayer )
import DeckBuilding.Types ( PlayerNumber(PlayerNumber, unPlayerNumber) )
import System.Random ( mkStdGen )
import Test.Hspec ( shouldBe, it, describe, Spec, shouldContain, shouldNotContain, shouldNotBe )
import DeckBuilding.Dominion.Cards.Intrigue (baronCard, courtyardCard, lurkerCard, pawnCard, masqueradeCard, stewardCard, shantyTownCard, swindlerCard, conspiratorCard, ironworksCard, dukeCard, wishingWellCard, bridgeCard, diplomatCard, upgradeCard, millCard, miningVillageCard, secretPassageCard, noblesCard, patrolCard, minionCard, torturerCard)
import DeckBuilding.Dominion.Strategies.Utils (gainWhichCard)
import Dominion.Utils ( defaultConfig, initialState, p0, p1, setDeck, setHand )
import Safe (headMay)
import DeckBuilding.Dominion.Cards.Utils (simpleVictory)

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
      length (p1AfterCard ^. #hand) `shouldBe` 8

  describe "Trading Post" $ do
    it "trashes 2 cards and gains a Silver to hand" $ do
      let trashStrat = \_ _ _ -> [copperCard, copperCard]
      let p0Strategy = bigMoneyStrategy { trashStrategy = trashStrat }
      let config = defaultConfig { playerDefs = [("Player 0", p0Strategy), ("Player 1", bigMoneyStrategy)] }
      let (p0AfterCard, finalState) = initialState config $ do
            tradingPostCard ^. #action $ p0
            findPlayer p0
      length (p0AfterCard ^. #hand) `shouldBe` 4 -- 5 initial - 2 trashed + 1 gained
      headMay (p0AfterCard ^. #hand) `shouldBe` Just silverCard
      length (finalState ^. #trash) `shouldBe` 2
      finalState ^. #trash `shouldBe` [copperCard, copperCard]

    it "does not gain Silver if fewer than 2 cards are trashed" $ do
      let trashStrat = \_ _ _ -> [copperCard]
      let p0Strategy = bigMoneyStrategy { trashStrategy = trashStrat }
      let config = defaultConfig { playerDefs = [("Player 0", p0Strategy), ("Player 1", bigMoneyStrategy)] }
      let (p0AfterCard, finalState) = initialState config $ do
            #players . ix 0 . #hand .= [copperCard, estateCard, estateCard, estateCard, estateCard] -- Start with only 1 copper
            tradingPostCard ^. #action $ p0
            findPlayer p0
      length (p0AfterCard ^. #hand) `shouldBe` 4 -- 5 initial - 1 trashed
      p0AfterCard ^. #hand `shouldNotContain` [silverCard]
      length (finalState ^. #trash) `shouldBe` 1
      finalState ^. #trash `shouldBe` [copperCard]

  describe "Replace" $ do
    it "trashes a Copper, gains a Silver to deck" $ do
      let trashStrat = \_ _ _ -> [copperCard]
      let gainStrat = \_ cost -> if cost == 2 then Just silverCard else Nothing
      let p0Strategy = bigMoneyStrategy { trashStrategy = trashStrat, gainCardStrategy = gainStrat }
      let config = defaultConfig { playerDefs = [("Player 0", p0Strategy), ("Player 1", bigMoneyStrategy)] }
      let (p0AfterCard, finalState) = initialState config $ do
            replaceCard ^. #action $ p0
            findPlayer p0
      length (p0AfterCard ^. #hand) `shouldBe` 4 -- 5 initial - 1 trashed
      headMay (p0AfterCard ^. #deck) `shouldBe` Just silverCard
      length (finalState ^. #trash) `shouldBe` 1
      finalState ^. #trash `shouldBe` [copperCard]

    it "trashes a Silver, gains a Duchy to hand" $ do
      let trashStrat = \_ _ _ -> [silverCard]
      let gainStrat = \_ cost -> if cost == 5 then Just duchyCard else Nothing
      let p0Strategy = bigMoneyStrategy { trashStrategy = trashStrat, gainCardStrategy = gainStrat }
      let config = defaultConfig { playerDefs = [("Player 0", p0Strategy), ("Player 1", bigMoneyStrategy)] }
      let (p0AfterCard, finalState) = initialState config $ do
            #players . ix 0 . #hand .= [silverCard, copperCard, copperCard, copperCard, copperCard]
            replaceCard ^. #action $ p0
            findPlayer p0
      length (p0AfterCard ^. #hand) `shouldBe` 5 -- 5 initial - 1 trashed + 1 gained
      p0AfterCard ^. #hand `shouldContain` [duchyCard]
      headMay (p0AfterCard ^. #deck) `shouldNotBe` Just duchyCard
      length (finalState ^. #trash) `shouldBe` 1
      finalState ^. #trash `shouldBe` [silverCard]

  describe "Courtier" $ do
    it "reveals Smithy (Action, 1 type) and chooses +1 Action" $ do
      let testSmithy = smithyCard { numImplicitTypes = 1 }
      let revealStrat = \_ -> testSmithy
      let bonusStrat = \_ _ numT -> take numT [Just CourtierAction]
      let p0Strategy = bigMoneyStrategy { courtierRevealStrategy = revealStrat, courtierBonusStrategy = bonusStrat }
      let config = defaultConfig { playerDefs = [("Player 0", p0Strategy), ("Player 1", bigMoneyStrategy)] }
      let (p0AfterCard, _) = initialState config $ do
            #players . ix 0 . #hand .= [testSmithy, copperCard, copperCard, copperCard, estateCard]
            courtierCard ^. #action $ p0
            findPlayer p0
      p0AfterCard ^. #actions `shouldBe` 1 -- 0 initial -1 played + 1 gained
      p0AfterCard ^. #buys `shouldBe` 1
      p0AfterCard ^. #money `shouldBe` 0

    it "reveals Gold (Treasure, 1 type) and chooses +$3" $ do
      let testGold = goldCard { numImplicitTypes = 1 }
      let revealStrat = \_ -> testGold
      let bonusStrat = \_ _ numT -> take numT [Just CourtierMoney]
      let p0Strategy = bigMoneyStrategy { courtierRevealStrategy = revealStrat, courtierBonusStrategy = bonusStrat }
      let config = defaultConfig { playerDefs = [("Player 0", p0Strategy), ("Player 1", bigMoneyStrategy)] }
      let (p0AfterCard, _) = initialState config $ do
            #players . ix 0 . #hand .= [testGold, copperCard, copperCard, copperCard, estateCard]
            courtierCard ^. #action $ p0
            findPlayer p0
      p0AfterCard ^. #actions `shouldBe` 0
      p0AfterCard ^. #buys `shouldBe` 1
      p0AfterCard ^. #money `shouldBe` 3

    it "reveals Province (Victory, 1 type) and chooses +1 Buy" $ do
      let testProvince = provinceCard { numImplicitTypes = 1 }
      let revealStrat = \_ -> testProvince
      let bonusStrat = \_ _ numT -> take numT [Just CourtierBuy]
      let p0Strategy = bigMoneyStrategy { courtierRevealStrategy = revealStrat, courtierBonusStrategy = bonusStrat }
      let config = defaultConfig { playerDefs = [("Player 0", p0Strategy), ("Player 1", bigMoneyStrategy)] }
      let (p0AfterCard, _) = initialState config $ do
            #players . ix 0 . #hand .= [testProvince, copperCard, copperCard, copperCard, estateCard]
            courtierCard ^. #action $ p0
            findPlayer p0
      p0AfterCard ^. #actions `shouldBe` 0
      p0AfterCard ^. #buys `shouldBe` 2 -- 1 initial + 1 gained
      p0AfterCard ^. #money `shouldBe` 0

    it "reveals Harem (Treasure/Victory, 2 types) and gets +$3 and +1 Buy" $ do
        let haremCard = Card "Harem" 6 undefined Value (simpleVictory 2) 2
        let revealStrat = \_ -> haremCard
        let bonusStrat = \_ card numT -> take numT [Just CourtierMoney, Just CourtierBuy]
        let p0Strategy = bigMoneyStrategy { courtierRevealStrategy = revealStrat, courtierBonusStrategy = bonusStrat }
        let config = defaultConfig { playerDefs = [("Player 0", p0Strategy), ("Player 1", bigMoneyStrategy)] }
        let (p0AfterCard, _) = initialState config $ do
              #players . ix 0 . #hand .= [haremCard, copperCard, copperCard, copperCard, estateCard]
              courtierCard ^. #action $ p0
              findPlayer p0
        p0AfterCard ^. #actions `shouldBe` 0
        p0AfterCard ^. #buys `shouldBe` 2 -- 1 initial + 1 gained
        p0AfterCard ^. #money `shouldBe` 3