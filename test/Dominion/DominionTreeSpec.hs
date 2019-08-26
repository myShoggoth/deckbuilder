{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}

module Dominion.DominionTreeSpec
    ( spec
    ) where

import           DeckBuilding.Dominion.Cards
import           DeckBuilding.Dominion.DominionTree
import           DeckBuilding.Dominion.Strategies.Basic
import           DeckBuilding.Dominion.Types
import           Test.Hspec

spec :: Spec
spec = do
  describe "minimal games" $ do
    it "can build a minimal tree" $ do
      let moves = [ Turn 0 $ DominionPlayer "player 1" [] [] [] [] 1 1 0 0 0 bigMoneyStrategy
                  , Play copperCard
                  , Play copperCard
                  , Buy silverCard
                  , GameOver [("player 1", 1)]
                  ]
      let dtree = [DominionTree
                  [ GameTurn 0 [ PlayerTurn "player 1" [ Standard copperCard, Standard copperCard ] [ BoughtCard silverCard ] ] ]
                  (Left "player 1")
                  ]
      buildDominionTrees moves [Left "player 1"] `shouldBe` dtree
    it "can build a tree with two players" $ do
      let moves = [ Turn 0 $ DominionPlayer "player 1" [] [] [] [] 1 1 0 0 0 bigMoneyStrategy
                  , Play copperCard
                  , Play copperCard
                  , Buy silverCard
                  , Turn 0 $ DominionPlayer "player 2" [] [] [] [] 1 1 0 0 0 bigMoneyStrategy
                  , Play estateCard
                  , Play copperCard
                  , Play copperCard
                  , Buy silverCard
                  , GameOver [("blat", 1)]
                  ]
      let dtree = [DominionTree
                    [ GameTurn 0 [ PlayerTurn "player 1" [ Standard copperCard, Standard copperCard ] [ BoughtCard silverCard ]
                                 , PlayerTurn "player 2" [ Standard estateCard, Standard copperCard, Standard copperCard ] [ BoughtCard silverCard ] 
                                 ]
                    ]
                    (Right 2)
                  ]
      buildDominionTrees moves [Right 2] `shouldBe` dtree
    it "can build a tree with two turns" $ do
      let moves = [ Turn 0 $ DominionPlayer "player 1" [] [] [] [] 1 1 0 0 0 bigMoneyStrategy
                  , Play copperCard
                  , Play copperCard
                  , Buy silverCard
                  , Turn 0 $ DominionPlayer "player 2" [] [] [] [] 1 1 0 0 0 bigMoneyStrategy
                  , Play estateCard
                  , Play copperCard
                  , Turn 1 $ DominionPlayer "player 1" [] [] [] [] 1 1 0 0 0 bigMoneyStrategy
                  , Play copperCard
                  , Play copperCard
                  , Buy silverCard
                  , Turn 1 $ DominionPlayer "player 2" [] [] [] [] 1 1 0 0 0 bigMoneyStrategy
                  , Play copperCard
                  , Play copperCard
                  , Buy silverCard
                  , GameOver [("blat", 1)]
                  ]
      let dtree = [DominionTree
                    [ GameTurn 0 [ PlayerTurn "player 1" [ Standard copperCard, Standard copperCard ] [ BoughtCard silverCard ]
                                 , PlayerTurn "player 2" [ Standard estateCard, Standard copperCard ] [] 
                                 ]
                    , GameTurn 1 [ PlayerTurn "player 1" [ Standard copperCard, Standard copperCard ] [ BoughtCard silverCard ]
                                 , PlayerTurn "player 2" [ Standard copperCard, Standard copperCard ] [ BoughtCard silverCard ] 
                                 ]
                    ]
                    (Right 2)
                  ]
      buildDominionTrees moves [Right 2] `shouldBe` dtree
