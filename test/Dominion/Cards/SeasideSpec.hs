{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}

module Dominion.Cards.SeasideSpec
    ( spec
    ) where

import Test.Hspec ( shouldBe, it, describe, Spec )
import Control.Lens ( (^?), (^.), (%=), set, Ixed(ix) )
import DeckBuilding.Dominion.Cards (ambassadorCard, embargoCard, islandCard, firstGameKingdomCards)
import Control.Monad.State ( execState, evalState )
import System.Random ( mkStdGen )
import DeckBuilding.Dominion.Utils ( deal )
import DeckBuilding.Types ( PlayerNumber(PlayerNumber), Game (tallyPoints) )
import DeckBuilding.Dominion.Types
    ( Card,
      DominionPlayer(DominionPlayer),
      DominionConfig(DominionConfig),
      Strategy(Strategy),
      DominionState,
      CardType(Action),
      DominionGame(DominionGame), DominionBoard(DominionBoard), DominionAIGame )
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
      nextCardByWeight )
import DeckBuilding.Dominion
    ( basicDecks, configToGame, makeDecks )
import qualified Data.Map as Map

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

    describe "Ambassador action" $ do
        let afterCard = execState ((ambassadorCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        let (Just p2AfterCard) = afterCard ^? #players . ix 1
        it "gives up a trash card (estate)" $ do
            length (p1AfterCard ^. #hand) `shouldBe` 4
        it "gives the other player a copy" $ do
            length (p2AfterCard ^. #discard) `shouldBe` 11

    describe "Embargo action" $  do
        let afterCard = execState ((embargoCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        let (Just p2AfterCard) = afterCard ^? #players . ix 1
        it "adds an embargo token to the first Supply deck" $ do
            (snd . head $ Map.toList $ afterCard ^. #embargoes) `shouldBe` 1
        it "gives two money" $ do
            (p1AfterCard ^. #money) `shouldBe` 2

    describe "Island action" $ do
        let afterCard = execState ((islandCard ^. #action) p0) afterDeal
        let (Just p1AfterCard) = afterCard ^? #players . ix 0
        it "puts two cards on the Island mat, one is the Island" $ do
            length (p1AfterCard ^. #island) `shouldBe` 2
            islandCard `elem` (p1AfterCard ^. #island) `shouldBe` True
        it "counts cards on the Island in the final score" $ do
            let afterCardDone = evalState tallyPoints afterCard
            Map.fromList afterCardDone Map.! "Player 1" `shouldBe` 5
