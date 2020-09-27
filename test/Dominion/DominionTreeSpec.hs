{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE OverloadedStrings         #-}

module Dominion.DominionTreeSpec
    ( spec
    ) where

import           DeckBuilding.Dominion.Cards
import           DeckBuilding.Dominion.DominionTree
import           DeckBuilding.Dominion.Strategies.Basic
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Types
import           Test.Hspec

spec :: Spec
spec = do
  describe "minimal games" $ do
    let p0 = PlayerNumber 0
        p1 = PlayerNumber 1
    it "can build a minimal tree" $ do
      let moves = [ Turn p0 0 $ DominionPlayer "player 1" [] [] [] [] 1 1 0 0 0 bigMoneyStrategy
                  , Play p0 copperCard
                  , Play p0 copperCard
                  , Buy p0 silverCard
                  , GameOver [("player 1", 1)]
                  ]
          dtree = [DominionTree
                  [ GameTurn 0 [ PlayerTurn p0 [ Standard copperCard, Standard copperCard ] [ BoughtCard silverCard ] ] ]
                  (Left ("player 1", 10))
                  ]
      buildDominionTrees moves [Left ("player 1", 10)] `shouldBe` dtree
    it "can build a tree with two players" $ do
      let moves = [ Turn p0 0 $ DominionPlayer "player 1" [] [] [] [] 1 1 0 0 0 bigMoneyStrategy
                  , Play p0 copperCard
                  , Play p0 copperCard
                  , Buy p0 silverCard
                  , Turn p1 0 $ DominionPlayer "player 2" [] [] [] [] 1 1 0 0 0 bigMoneyStrategy
                  , Play p1 estateCard
                  , Play p1 copperCard
                  , Play p1 copperCard
                  , Buy p1 silverCard
                  , GameOver [("blat", 1)]
                  ]
      let dtree = [DominionTree
                    [ GameTurn 0 [ PlayerTurn p0 [ Standard copperCard, Standard copperCard ] [ BoughtCard silverCard ]
                                 , PlayerTurn p1 [ Standard estateCard, Standard copperCard, Standard copperCard ] [ BoughtCard silverCard ] 
                                 ]
                    ]
                    (Right 2)
                  ]
      buildDominionTrees moves [Right 2] `shouldBe` dtree
    it "can build a tree with two turns" $ do
      let moves = [ Turn p0 0 $ DominionPlayer "player 1" [] [] [] [] 1 1 0 0 0 bigMoneyStrategy
                  , Play p0 copperCard
                  , Play p0 copperCard
                  , Buy p0 silverCard
                  , Turn p1 0 $ DominionPlayer "player 2" [] [] [] [] 1 1 0 0 0 bigMoneyStrategy
                  , Play p1 estateCard
                  , Play p1 copperCard
                  , Turn p0 1 $ DominionPlayer "player 1" [] [] [] [] 1 1 0 0 0 bigMoneyStrategy
                  , Play p0 copperCard
                  , Play p0 copperCard
                  , Buy p0 silverCard
                  , Turn p1 1 $ DominionPlayer "player 2" [] [] [] [] 1 1 0 0 0 bigMoneyStrategy
                  , Play p1 copperCard
                  , Play p1 copperCard
                  , Buy p1 silverCard
                  , GameOver [("blat", 1)]
                  ]
      let dtree = [DominionTree
                    [ GameTurn 0 [ PlayerTurn p0 [ Standard copperCard, Standard copperCard ] [ BoughtCard silverCard ]
                                 , PlayerTurn p1 [ Standard estateCard, Standard copperCard ] [] 
                                 ]
                    , GameTurn 1 [ PlayerTurn p0 [ Standard copperCard, Standard copperCard ] [ BoughtCard silverCard ]
                                 , PlayerTurn p1 [ Standard copperCard, Standard copperCard ] [ BoughtCard silverCard ] 
                                 ]
                    ]
                    (Right 2)
                  ]
      buildDominionTrees moves [Right 2] `shouldBe` dtree
