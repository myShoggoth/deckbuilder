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
  describe "real game" $ do
    it "can build a tree with a real game" $ do
      let moves = [ Turn 0 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [], discard = [copperCard,copperCard,copperCard,copperCard,copperCard,copperCard,copperCard,estateCard,estateCard,estateCard], hand = [], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 0, strategy = bigMoneyStrategy})
                  , Deal 5 [estateCard,copperCard,copperCard,estateCard,copperCard]
                  , Turn 0 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [], discard = [copperCard,copperCard,copperCard,copperCard,copperCard,copperCard,copperCard,estateCard,estateCard,estateCard], hand = [], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 0, strategy = bigSmithyStrategy})
                  , Deal 5 [estateCard,copperCard,copperCard,estateCard,copperCard]
                  , Turn 0 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [], discard = [copperCard,copperCard,copperCard,copperCard,copperCard,copperCard,copperCard,estateCard,estateCard,estateCard], hand = [], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 0, strategy = villageSmithyEngine4})
                  , Deal 5 [estateCard,copperCard,copperCard,copperCard,copperCard]
                  , Turn 1 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [copperCard,copperCard,estateCard,copperCard,copperCard], discard = [], hand = [estateCard,copperCard,copperCard,estateCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 1, strategy = bigMoneyStrategy})
                  , Play estateCard,Play copperCard,Play copperCard,Play estateCard,Play copperCard
                  , Buy silverCard
                  , Deal 5 [copperCard,copperCard,estateCard,copperCard,copperCard]
                  , Turn 1 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [copperCard,copperCard,copperCard,estateCard,copperCard], discard = [], hand = [estateCard,copperCard,copperCard,estateCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 1, strategy = bigSmithyStrategy})
                  , Play estateCard,Play copperCard,Play copperCard,Play estateCard,Play copperCard
                  , Buy silverCard
                  , Deal 5 [copperCard,copperCard,copperCard,estateCard,copperCard]
                  , Turn 1 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [copperCard,copperCard,estateCard,copperCard,estateCard], discard = [], hand = [estateCard,copperCard,copperCard,copperCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 1, strategy = villageSmithyEngine4})
                  , Play estateCard,Play copperCard,Play copperCard,Play copperCard,Play copperCard
                  , Buy smithyCard
                  , Deal 5 [copperCard,copperCard,estateCard,copperCard,estateCard]
                  , Turn 2 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [], discard = [copperCard,estateCard,copperCard,copperCard,estateCard,silverCard], hand = [copperCard,copperCard,estateCard,copperCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 2, strategy = bigMoneyStrategy})
                  , Play copperCard,Play copperCard,Play estateCard,Play copperCard,Play copperCard
                  , Buy silverCard
                  , Deal 5 [copperCard,copperCard,copperCard,copperCard,silverCard]
                  , Turn 2 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [], discard = [copperCard,estateCard,copperCard,copperCard,estateCard,silverCard], hand = [copperCard,copperCard,copperCard,estateCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 2, strategy = bigSmithyStrategy})
                  , Play copperCard,Play copperCard,Play copperCard,Play estateCard,Play copperCard
                  , Buy smithyCard
                  , Deal 5 [estateCard,copperCard,silverCard,smithyCard,copperCard]
                  , Turn 2 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [], discard = [copperCard,copperCard,copperCard,copperCard,estateCard,smithyCard], hand = [copperCard,copperCard,estateCard,copperCard,estateCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 2, strategy = villageSmithyEngine4})
                  , Play copperCard,Play copperCard,Play estateCard,Play copperCard,Play estateCard
                  , Buy villageCard
                  , Deal 5 [copperCard,copperCard,copperCard,copperCard,copperCard]
                  , Turn 3 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [silverCard,estateCard,copperCard,copperCard,copperCard,estateCard,estateCard], discard = [], hand = [copperCard,copperCard,copperCard,copperCard,silverCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 3, strategy = bigMoneyStrategy})
                  , Play copperCard,Play copperCard,Play copperCard,Play copperCard,Play silverCard,Buy goldCard,Deal 5 [silverCard,estateCard,copperCard,copperCard,copperCard]
                  , Turn 3 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [estateCard,copperCard,copperCard,copperCard,copperCard,estateCard,copperCard], discard = [], hand = [estateCard,copperCard,silverCard,smithyCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 3, strategy = bigSmithyStrategy})
                  , Play estateCard,Play copperCard,Play silverCard,Play smithyCard
                  , Deal 3 [estateCard,copperCard,copperCard]
                  , Play copperCard,Play estateCard,Play copperCard,Play copperCard
                  , Buy smithyCard
                  , Deal 5 [copperCard,copperCard,estateCard,copperCard,smithyCard]
                  , Turn 3 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [estateCard,copperCard,copperCard,villageCard,estateCard,smithyCard,estateCard], discard = [], hand = [copperCard,copperCard,copperCard,copperCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 3, strategy = villageSmithyEngine4})
                  , Play copperCard,Play copperCard,Play copperCard,Play copperCard,Play copperCard,Buy smithyCard,Deal 5 [estateCard,copperCard,copperCard,villageCard,estateCard]
                  , Turn 4 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [estateCard,estateCard], discard = [silverCard,copperCard,copperCard,copperCard,copperCard,goldCard], hand = [silverCard,estateCard,copperCard,copperCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 4, strategy = bigMoneyStrategy})
                  , Play silverCard,Play estateCard,Play copperCard,Play copperCard,Play copperCard,Buy silverCard,Deal 5 [estateCard,estateCard,copperCard,copperCard,goldCard]
                  , Turn 4 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [estateCard,estateCard,copperCard,copperCard,silverCard,copperCard,copperCard,smithyCard], discard = [], hand = [copperCard,copperCard,estateCard,copperCard,smithyCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 4, strategy = bigSmithyStrategy})
                  , Play copperCard,Play copperCard,Play estateCard,Play copperCard,Play smithyCard,Deal 3 [estateCard,estateCard,copperCard],Play estateCard,Play estateCard,Play copperCard,Buy silverCard,Deal 5 [copperCard,silverCard,copperCard,copperCard,smithyCard]
                  , Turn 4 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [smithyCard,estateCard], discard = [copperCard,copperCard,copperCard,copperCard,copperCard,smithyCard], hand = [estateCard,copperCard,copperCard,villageCard,estateCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 4, strategy = villageSmithyEngine4})
                  , Play estateCard,Play copperCard,Play copperCard,Play villageCard,Deal 1 [smithyCard],Play estateCard,Play smithyCard,Deal 3 [estateCard,smithyCard,copperCard],Play estateCard,Play smithyCard,Deal 3 [copperCard,copperCard,copperCard],Play copperCard,Play copperCard,Play copperCard,Play copperCard,Buy goldCard,Deal 5 [copperCard,estateCard,estateCard,copperCard,copperCard]
                  , Turn 5 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [copperCard,copperCard,estateCard,copperCard,silverCard,silverCard,copperCard,copperCard,silverCard], discard = [], hand = [estateCard,estateCard,copperCard,copperCard,goldCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 5, strategy = bigMoneyStrategy})
                  , Play estateCard,Play estateCard,Play copperCard,Play copperCard,Play goldCard,Buy silverCard,Deal 5 [copperCard,copperCard,estateCard,copperCard,silverCard]
                  , Turn 5 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [], discard = [copperCard,estateCard,estateCard,smithyCard,copperCard,estateCard,copperCard,copperCard,silverCard], hand = [copperCard,silverCard,copperCard,copperCard,smithyCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 5, strategy = bigSmithyStrategy})
                  , Play copperCard,Play silverCard,Play copperCard,Play copperCard,Play smithyCard,Deal 3 [copperCard,copperCard,copperCard],Play copperCard,Play copperCard,Play copperCard,Buy provinceCard,Deal 5 [estateCard,estateCard,silverCard,copperCard,smithyCard]
                  , Turn 5 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [goldCard,copperCard,copperCard,smithyCard,copperCard,smithyCard,copperCard,estateCard,villageCard], discard = [], hand = [copperCard,estateCard,estateCard,copperCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 5, strategy = villageSmithyEngine4})
                  , Play copperCard,Play estateCard,Play estateCard,Play copperCard,Play copperCard,Buy villageCard,Deal 5 [goldCard,copperCard,copperCard,smithyCard,copperCard]
                  , Turn 6 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [silverCard,copperCard,copperCard,silverCard], discard = [goldCard,copperCard,copperCard,estateCard,estateCard,silverCard], hand = [copperCard,copperCard,estateCard,copperCard,silverCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 6, strategy = bigMoneyStrategy})
                  , Play copperCard,Play copperCard,Play estateCard,Play copperCard,Play silverCard,Buy silverCard,Deal 5 [silverCard,copperCard,copperCard,silverCard,estateCard]
                  , Turn 6 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [estateCard], discard = [copperCard,copperCard,copperCard,smithyCard,copperCard,copperCard,silverCard,copperCard,provinceCard], hand = [estateCard,estateCard,silverCard,copperCard,smithyCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 6, strategy = bigSmithyStrategy})
                  , Play estateCard,Play estateCard,Play silverCard,Play copperCard,Play smithyCard,Deal 3 [estateCard,copperCard,copperCard],Play estateCard,Play copperCard,Play copperCard,Buy silverCard,Deal 5 [copperCard,provinceCard,smithyCard,copperCard,copperCard]
                  , Turn 6 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [smithyCard,copperCard,estateCard,villageCard], discard = [copperCard,copperCard,estateCard,estateCard,copperCard,villageCard], hand = [goldCard,copperCard,copperCard,smithyCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 6, strategy = villageSmithyEngine4})
                  , Play goldCard,Play copperCard,Play copperCard,Play smithyCard,Deal 3 [smithyCard,copperCard,estateCard],Play copperCard,Buy goldCard,Deal 5 [villageCard,villageCard,estateCard,copperCard,estateCard]
                  , Turn 7 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [silverCard,goldCard,copperCard,estateCard,silverCard,estateCard,copperCard,silverCard,copperCard,copperCard,copperCard], discard = [], hand = [silverCard,copperCard,copperCard,silverCard,estateCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 7, strategy = bigMoneyStrategy})
                  , Play silverCard,Play copperCard,Play copperCard,Play silverCard,Play estateCard,Buy goldCard,Deal 5 [silverCard,goldCard,copperCard,estateCard,silverCard]
                  , Turn 7 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [copperCard,silverCard], discard = [copperCard,copperCard,estateCard,smithyCard,copperCard,silverCard,estateCard,estateCard,silverCard], hand = [copperCard,provinceCard,smithyCard,copperCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 7, strategy = bigSmithyStrategy})
                  , Play copperCard,Play provinceCard,Play smithyCard,Deal 3 [copperCard,silverCard,copperCard],Play copperCard,Play copperCard,Play copperCard,Play silverCard,Play copperCard,Buy goldCard,Deal 5 [smithyCard,copperCard,estateCard,copperCard,estateCard]
                  , Turn 7 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [goldCard,copperCard,smithyCard,goldCard,smithyCard,copperCard,copperCard,copperCard,estateCard,copperCard,copperCard], discard = [], hand = [villageCard,villageCard,estateCard,copperCard,estateCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 7, strategy = villageSmithyEngine4})
                  , Play villageCard,Deal 1 [goldCard],Play villageCard,Deal 1 [copperCard],Play estateCard,Play copperCard,Play estateCard,Play goldCard,Play copperCard,Buy smithyCard,Deal 5 [smithyCard,goldCard,smithyCard,copperCard,copperCard]
                  , Turn 8 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [estateCard,copperCard,silverCard,copperCard,copperCard,copperCard], discard = [estateCard,silverCard,copperCard,copperCard,silverCard,goldCard], hand = [silverCard,goldCard,copperCard,estateCard,silverCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 8, strategy = bigMoneyStrategy})
                  , Play silverCard,Play goldCard,Play copperCard,Play estateCard,Play silverCard,Buy provinceCard,Deal 5 [estateCard,copperCard,silverCard,copperCard,copperCard]
                  , Turn 8 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [silverCard,estateCard,silverCard], discard = [copperCard,silverCard,copperCard,copperCard,copperCard,smithyCard,provinceCard,copperCard,goldCard], hand = [smithyCard,copperCard,estateCard,copperCard,estateCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 8, strategy = bigSmithyStrategy})
                  , Play smithyCard,Deal 3 [silverCard,estateCard,silverCard],Play copperCard,Play estateCard,Play copperCard,Play estateCard,Play silverCard,Play estateCard,Play silverCard,Buy goldCard,Deal 5 [silverCard,smithyCard,goldCard,provinceCard,silverCard]
                  , Turn 8 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [copperCard,estateCard,copperCard,copperCard], discard = [copperCard,goldCard,estateCard,copperCard,estateCard,villageCard,villageCard,smithyCard], hand = [smithyCard,goldCard,smithyCard,copperCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 8, strategy = villageSmithyEngine4})
                  , Play smithyCard,Deal 3 [copperCard,estateCard,copperCard],Play goldCard,Buy villageCard,Deal 5 [copperCard,smithyCard,copperCard,villageCard,smithyCard]
                  , Turn 9 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [copperCard], discard = [silverCard,estateCard,copperCard,goldCard,silverCard,provinceCard,estateCard,silverCard,copperCard,copperCard,silverCard,goldCard], hand = [estateCard,copperCard,silverCard,copperCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 9, strategy = bigMoneyStrategy})
                  , Play estateCard,Play copperCard,Play silverCard,Play copperCard,Play copperCard,Buy silverCard,Deal 5 [copperCard,goldCard,estateCard,estateCard,silverCard]
                  , Turn 9 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [goldCard,estateCard,smithyCard,copperCard,copperCard,copperCard,estateCard,copperCard,copperCard,copperCard,copperCard,estateCard,silverCard], discard = [], hand = [silverCard,smithyCard,goldCard,provinceCard,silverCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 9, strategy = bigSmithyStrategy})
                  , Play silverCard,Play smithyCard,Deal 3 [goldCard,estateCard,smithyCard],Play goldCard,Play provinceCard,Play silverCard,Play goldCard,Play estateCard,Buy provinceCard,Deal 5 [copperCard,copperCard,copperCard,estateCard,copperCard]
                  , Turn 9 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [copperCard,estateCard,copperCard,goldCard,copperCard,copperCard,smithyCard,estateCard,estateCard,villageCard,villageCard,goldCard,copperCard], discard = [], hand = [copperCard,smithyCard,copperCard,villageCard,smithyCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 9, strategy = villageSmithyEngine4})
                  , Play copperCard,Play smithyCard,Deal 3 [copperCard,estateCard,copperCard],Play copperCard,Buy cellarCard,Deal 5 [goldCard,copperCard,copperCard,smithyCard,estateCard]
                  , Turn 10 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [provinceCard,copperCard,silverCard,goldCard,silverCard,silverCard,estateCard,copperCard,copperCard,copperCard,silverCard,copperCard,silverCard,copperCard], discard = [], hand = [copperCard,goldCard,estateCard,estateCard,silverCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 10, strategy = bigMoneyStrategy})
                  , Play copperCard,Play goldCard,Play estateCard,Play estateCard,Play silverCard,Buy goldCard,Deal 5 [provinceCard,copperCard,silverCard,goldCard,silverCard]
                  , Turn 10 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [copperCard,copperCard,copperCard,estateCard,silverCard], discard = [smithyCard,estateCard,goldCard,silverCard,provinceCard,goldCard,smithyCard,silverCard,provinceCard], hand = [copperCard,copperCard,copperCard,estateCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 10, strategy = bigSmithyStrategy})
                  , Play copperCard,Play copperCard,Play copperCard,Play estateCard,Play copperCard,Buy silverCard,Deal 5 [copperCard,copperCard,copperCard,estateCard,silverCard]
                  , Turn 10 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [estateCard,villageCard,villageCard,goldCard,copperCard], discard = [villageCard,smithyCard,copperCard,estateCard,copperCard,copperCard,smithyCard,copperCard,cellarCard], hand = [goldCard,copperCard,copperCard,smithyCard,estateCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 10, strategy = villageSmithyEngine4})
                  , Play goldCard,Play copperCard,Play copperCard,Play smithyCard,Deal 3 [estateCard,villageCard,villageCard],Play estateCard,Play estateCard,Buy smithyCard,Deal 5 [goldCard,copperCard,villageCard,copperCard,estateCard]
                  , Turn 11 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [silverCard,estateCard,copperCard,copperCard,copperCard,silverCard,copperCard,silverCard,copperCard], discard = [silverCard,estateCard,estateCard,goldCard,copperCard,goldCard], hand = [provinceCard,copperCard,silverCard,goldCard,silverCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 11, strategy = bigMoneyStrategy})
                  , Play provinceCard,Play copperCard,Play silverCard,Play goldCard,Play silverCard,Buy provinceCard,Deal 5 [silverCard,estateCard,copperCard,copperCard,copperCard]
                  , Turn 11 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [], discard = [copperCard,estateCard,copperCard,copperCard,copperCard,silverCard,smithyCard,estateCard,goldCard,silverCard,provinceCard,goldCard,smithyCard,silverCard,provinceCard], hand = [copperCard,copperCard,copperCard,estateCard,silverCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 11, strategy = bigSmithyStrategy})
                  , Play copperCard,Play copperCard,Play copperCard,Play estateCard,Play silverCard,Buy silverCard,Deal 5 [copperCard,copperCard,provinceCard,goldCard,silverCard]
                  , Turn 11 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [cellarCard,villageCard,copperCard,smithyCard,goldCard,estateCard,smithyCard,copperCard,villageCard,estateCard,smithyCard,smithyCard,copperCard,copperCard,copperCard], discard = [], hand = [goldCard,copperCard,villageCard,copperCard,estateCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 11, strategy = villageSmithyEngine4})
                  , Play goldCard,Play copperCard,Play villageCard,Deal 1 [cellarCard],Play copperCard,Play estateCard,Play cellarCard,Discard [],Discard [],Buy villageCard,Deal 5 [villageCard,copperCard,smithyCard,goldCard,estateCard]
                  , Turn 12 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [silverCard,copperCard,silverCard,copperCard], discard = [silverCard,goldCard,silverCard,copperCard,provinceCard,provinceCard,silverCard,estateCard,estateCard,goldCard,copperCard,goldCard], hand = [silverCard,estateCard,copperCard,copperCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 12, strategy = bigMoneyStrategy})
                  , Play silverCard,Play estateCard,Play copperCard,Play copperCard,Play copperCard,Buy silverCard,Deal 5 [silverCard,copperCard,silverCard,copperCard,copperCard]
                  , Turn 12 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [copperCard,silverCard,copperCard,estateCard,silverCard,provinceCard,estateCard,silverCard,smithyCard,copperCard,silverCard,smithyCard,goldCard,copperCard,copperCard,estateCard], discard = [], hand = [copperCard,copperCard,provinceCard,goldCard,silverCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 12, strategy = bigSmithyStrategy})
                  , Play copperCard,Play copperCard,Play provinceCard,Play goldCard,Play silverCard,Buy goldCard,Deal 5 [copperCard,silverCard,copperCard,estateCard,silverCard]
                  , Turn 12 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [smithyCard,copperCard,villageCard,estateCard,smithyCard,smithyCard,copperCard,copperCard,copperCard], discard = [cellarCard,estateCard,copperCard,villageCard,copperCard,goldCard,villageCard], hand = [villageCard,copperCard,smithyCard,goldCard,estateCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 12, strategy = villageSmithyEngine4})
                  , Play villageCard,Deal 1 [smithyCard],Play copperCard,Play smithyCard,Deal 3 [copperCard,villageCard,estateCard],Play goldCard,Play estateCard,Play smithyCard,Deal 3 [smithyCard,smithyCard,copperCard],Play copperCard,Buy smithyCard,Deal 5 [copperCard,copperCard,copperCard,smithyCard,copperCard]
                  , Turn 13 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [copperCard,silverCard,estateCard,estateCard,estateCard,provinceCard,silverCard,silverCard,silverCard,goldCard,provinceCard,copperCard,goldCard,copperCard,copperCard,goldCard,silverCard], discard = [], hand = [silverCard,copperCard,silverCard,copperCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 13, strategy = bigMoneyStrategy})
                  , Play silverCard,Play copperCard,Play silverCard,Play copperCard,Play copperCard,Buy goldCard,Deal 5 [copperCard,silverCard,estateCard,estateCard,estateCard]
                  , Turn 13 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [provinceCard,estateCard,silverCard,smithyCard,copperCard,silverCard,smithyCard,goldCard,copperCard,copperCard,estateCard], discard = [silverCard,goldCard,provinceCard,copperCard,copperCard,goldCard], hand = [copperCard,silverCard,copperCard,estateCard,silverCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 13, strategy = bigSmithyStrategy})
                  , Play copperCard,Play silverCard,Play copperCard,Play estateCard,Play silverCard,Buy goldCard,Deal 5 [provinceCard,estateCard,silverCard,smithyCard,copperCard]
                  , Turn 13 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [smithyCard,estateCard,estateCard,villageCard,villageCard,goldCard,smithyCard,cellarCard,copperCard,villageCard,copperCard,smithyCard,smithyCard,villageCard,goldCard,copperCard,estateCard], discard = [], hand = [copperCard,copperCard,copperCard,smithyCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 13, strategy = villageSmithyEngine4})
                  , Play copperCard,Play copperCard,Play copperCard,Play smithyCard,Deal 3 [smithyCard,estateCard,estateCard],Play copperCard,Buy villageCard,Deal 5 [villageCard,villageCard,goldCard,smithyCard,cellarCard]
                  , Turn 14 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [provinceCard,silverCard,silverCard,silverCard,goldCard,provinceCard,copperCard,goldCard,copperCard,copperCard,goldCard,silverCard], discard = [copperCard,copperCard,silverCard,copperCard,silverCard,goldCard], hand = [copperCard,silverCard,estateCard,estateCard,estateCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 14, strategy = bigMoneyStrategy})
                  , Play copperCard,Play silverCard,Play estateCard,Play estateCard,Play estateCard,Buy silverCard,Deal 5 [provinceCard,silverCard,silverCard,silverCard,goldCard]
                  , Turn 14 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [silverCard,smithyCard,goldCard,copperCard,copperCard,estateCard], discard = [silverCard,estateCard,copperCard,silverCard,copperCard,goldCard,silverCard,goldCard,provinceCard,copperCard,copperCard,goldCard], hand = [provinceCard,estateCard,silverCard,smithyCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 14, strategy = bigSmithyStrategy})
                  , Play provinceCard,Play estateCard,Play silverCard,Play smithyCard,Deal 3 [silverCard,smithyCard,goldCard],Play copperCard,Play silverCard,Buy silverCard,Deal 5 [copperCard,copperCard,estateCard,goldCard,smithyCard]
                  , Turn 14 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [copperCard,villageCard,copperCard,smithyCard,smithyCard,villageCard,goldCard,copperCard,estateCard], discard = [smithyCard,estateCard,estateCard,copperCard,smithyCard,copperCard,copperCard,copperCard,villageCard], hand = [villageCard,villageCard,goldCard,smithyCard,cellarCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 14, strategy = villageSmithyEngine4})
                  , Play villageCard,Deal 1 [copperCard],Play villageCard,Deal 1 [villageCard],Play goldCard,Play smithyCard,Deal 3 [copperCard,smithyCard,smithyCard],Play cellarCard,Discard [copperCard,copperCard],Discard [copperCard,copperCard],Deal 2 [villageCard,goldCard],Play villageCard,Deal 1 [copperCard],Play smithyCard,Deal 3 [estateCard,estateCard,copperCard],Play smithyCard,Deal 3 [copperCard,copperCard,estateCard],Buy villageCard,Deal 5 [copperCard,copperCard,smithyCard,smithyCard,villageCard]
                  , Turn 15 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [provinceCard,copperCard,goldCard,copperCard,copperCard,goldCard,silverCard], discard = [estateCard,estateCard,estateCard,silverCard,copperCard,silverCard,copperCard,copperCard,silverCard,copperCard,silverCard,goldCard], hand = [provinceCard,silverCard,silverCard,silverCard,goldCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 15, strategy = bigMoneyStrategy})
                  , Play provinceCard,Play silverCard,Play silverCard,Play silverCard,Play goldCard,Buy provinceCard,Deal 5 [provinceCard,copperCard,goldCard,copperCard,copperCard]
                  , Turn 15 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [silverCard,silverCard,goldCard,goldCard,provinceCard,smithyCard,estateCard,goldCard,silverCard,estateCard,provinceCard,copperCard,silverCard,silverCard,copperCard,copperCard,copperCard,silverCard,copperCard], discard = [], hand = [copperCard,copperCard,estateCard,goldCard,smithyCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 15, strategy = bigSmithyStrategy})
                  , Play copperCard,Play copperCard,Play estateCard,Play goldCard,Play smithyCard,Deal 3 [silverCard,silverCard,goldCard],Play silverCard,Play silverCard,Play goldCard,Buy provinceCard,Deal 5 [goldCard,provinceCard,smithyCard,estateCard,goldCard]
                  , Turn 15 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [copperCard], discard = [villageCard,goldCard,copperCard,estateCard,estateCard,copperCard,copperCard,copperCard,estateCard,smithyCard,smithyCard,villageCard,cellarCard,smithyCard,goldCard,villageCard,villageCard,villageCard], hand = [copperCard,copperCard,smithyCard,smithyCard,villageCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 15, strategy = villageSmithyEngine4})
                  , Play copperCard,Play copperCard,Play smithyCard,Deal 3 [copperCard,villageCard,villageCard],Buy cellarCard,Deal 5 [smithyCard,cellarCard,smithyCard,villageCard,estateCard]
                  , Turn 16 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [goldCard,silverCard], discard = [goldCard,silverCard,silverCard,silverCard,provinceCard,provinceCard,estateCard,estateCard,estateCard,silverCard,copperCard,silverCard,copperCard,copperCard,silverCard,copperCard,silverCard,goldCard], hand = [provinceCard,copperCard,goldCard,copperCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 16, strategy = bigMoneyStrategy})
                  , Play provinceCard,Play copperCard,Play goldCard,Play copperCard,Play copperCard,Buy goldCard,Deal 5 [goldCard,silverCard,silverCard,silverCard,copperCard]
                  , Turn 16 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [silverCard,estateCard,provinceCard,copperCard,silverCard,silverCard,copperCard,copperCard,copperCard,silverCard,copperCard], discard = [goldCard,silverCard,silverCard,smithyCard,goldCard,estateCard,copperCard,copperCard,provinceCard], hand = [goldCard,provinceCard,smithyCard,estateCard,goldCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 16, strategy = bigSmithyStrategy})
                  , Play goldCard,Play provinceCard,Play smithyCard,Deal 3 [silverCard,estateCard,provinceCard],Play estateCard,Play goldCard,Play silverCard,Play estateCard,Play provinceCard,Buy provinceCard,Deal 5 [copperCard,silverCard,silverCard,copperCard,copperCard]
                  , Turn 16 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [copperCard,goldCard,goldCard,estateCard,smithyCard,copperCard,estateCard,copperCard,villageCard,villageCard,copperCard], discard = [smithyCard,villageCard,copperCard,villageCard,villageCard,smithyCard,copperCard,copperCard,cellarCard], hand = [smithyCard,cellarCard,smithyCard,villageCard,estateCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 16, strategy = villageSmithyEngine4})
                  , Play smithyCard,Deal 3 [copperCard,goldCard,goldCard],Deal 5 [estateCard,smithyCard,copperCard,estateCard,copperCard]
                  , Turn 17 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [silverCard,goldCard,copperCard,copperCard,goldCard,provinceCard,goldCard,copperCard,copperCard,provinceCard,silverCard,estateCard,silverCard,provinceCard,estateCard,copperCard,goldCard,copperCard,silverCard,estateCard,silverCard], discard = [], hand = [goldCard,silverCard,silverCard,silverCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 17, strategy = bigMoneyStrategy})
                  , Play goldCard,Play silverCard,Play silverCard,Play silverCard,Play copperCard,Buy provinceCard,Deal 5 [silverCard,goldCard,copperCard,copperCard,goldCard]
                  , Turn 17 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [copperCard,silverCard,copperCard], discard = [provinceCard,estateCard,silverCard,goldCard,estateCard,smithyCard,provinceCard,goldCard,provinceCard,goldCard,silverCard,silverCard,smithyCard,goldCard,estateCard,copperCard,copperCard,provinceCard], hand = [copperCard,silverCard,silverCard,copperCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 17, strategy = bigSmithyStrategy})
                  , Play copperCard,Play silverCard,Play silverCard,Play copperCard,Play copperCard,Buy goldCard,Deal 5 [copperCard,silverCard,copperCard,provinceCard,silverCard]
                  , Turn 17 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [villageCard,villageCard,copperCard], discard = [cellarCard,smithyCard,villageCard,estateCard,copperCard,goldCard,goldCard,smithyCard,smithyCard,villageCard,copperCard,villageCard,villageCard,smithyCard,copperCard,copperCard,cellarCard], hand = [estateCard,smithyCard,copperCard,estateCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 17, strategy = villageSmithyEngine4})
                  , Play estateCard,Play smithyCard,Deal 3 [villageCard,villageCard,copperCard],Play copperCard,Play estateCard,Play copperCard,Deal 5 [villageCard,copperCard,smithyCard,estateCard,smithyCard]
                  , Turn 18 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [provinceCard,goldCard,copperCard,copperCard,provinceCard,silverCard,estateCard,silverCard,provinceCard,estateCard,copperCard,goldCard,copperCard,silverCard,estateCard,silverCard], discard = [copperCard,silverCard,silverCard,silverCard,goldCard,provinceCard], hand = [silverCard,goldCard,copperCard,copperCard,goldCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 18, strategy = bigMoneyStrategy})
                  , Play silverCard,Play goldCard,Play copperCard,Play copperCard,Play goldCard,Buy provinceCard,Deal 5 [provinceCard,goldCard,copperCard,copperCard,provinceCard]
                  , Turn 18 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [provinceCard,silverCard,smithyCard,estateCard,goldCard,goldCard,copperCard,copperCard,copperCard,silverCard,provinceCard,copperCard,silverCard,provinceCard,copperCard,goldCard,silverCard,goldCard,estateCard,goldCard,smithyCard,estateCard], discard = [], hand = [copperCard,silverCard,copperCard,provinceCard,silverCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 18, strategy = bigSmithyStrategy})
                  , Play copperCard,Play silverCard,Play copperCard,Play provinceCard,Play silverCard,Buy goldCard,Deal 5 [provinceCard,silverCard,smithyCard,estateCard,goldCard]
                  , Turn 18 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [copperCard,villageCard,estateCard,copperCard,villageCard,copperCard,cellarCard,copperCard,villageCard,villageCard,copperCard,goldCard,copperCard,smithyCard,villageCard,cellarCard,smithyCard,smithyCard,estateCard,goldCard], discard = [], hand = [villageCard,copperCard,smithyCard,estateCard,smithyCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 18, strategy = villageSmithyEngine4})
                  , Play villageCard,Deal 1 [copperCard],Play copperCard,Play smithyCard,Deal 3 [villageCard,estateCard,copperCard],Play estateCard,Play smithyCard,Deal 3 [villageCard,copperCard,cellarCard],Play copperCard,Deal 5 [copperCard,villageCard,villageCard,copperCard,goldCard]
                  , Turn 19 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [silverCard,estateCard,silverCard,provinceCard,estateCard,copperCard,goldCard,copperCard,silverCard,estateCard,silverCard], discard = [goldCard,copperCard,copperCard,goldCard,silverCard,provinceCard,copperCard,silverCard,silverCard,silverCard,goldCard,provinceCard], hand = [provinceCard,goldCard,copperCard,copperCard,provinceCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 19, strategy = bigMoneyStrategy})
                  , Play provinceCard,Play goldCard,Play copperCard,Play copperCard,Play provinceCard,Buy duchyCard,Deal 5 [silverCard,estateCard,silverCard,provinceCard,estateCard]
                  , Turn 19 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [goldCard,copperCard,copperCard,copperCard,silverCard,provinceCard,copperCard,silverCard,provinceCard,copperCard,goldCard,silverCard,goldCard,estateCard,goldCard,smithyCard,estateCard], discard = [silverCard,provinceCard,copperCard,silverCard,copperCard,goldCard], hand = [provinceCard,silverCard,smithyCard,estateCard,goldCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 19, strategy = bigSmithyStrategy})
                  , Play provinceCard,Play silverCard,Play smithyCard,Deal 3 [goldCard,copperCard,copperCard],Play estateCard,Play goldCard,Play goldCard,Play copperCard,Play copperCard,Buy provinceCard,Deal 5 [copperCard,silverCard,provinceCard,copperCard,silverCard]
                  , Turn 19 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [copperCard,smithyCard,villageCard,cellarCard,smithyCard,smithyCard,estateCard,goldCard], discard = [villageCard,estateCard,copperCard,villageCard,copperCard,cellarCard,copperCard,smithyCard,estateCard,smithyCard,copperCard,villageCard], hand = [copperCard,villageCard,villageCard,copperCard,goldCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 19, strategy = villageSmithyEngine4})
                  , Play copperCard,Play villageCard,Deal 1 [copperCard],Play villageCard,Deal 1 [smithyCard],Play copperCard,Play goldCard,Play copperCard,Play smithyCard,Deal 3 [villageCard,cellarCard,smithyCard],Play villageCard,Deal 1 [smithyCard],Play cellarCard,Discard [],Discard [],Play smithyCard,Deal 3 [estateCard,goldCard,copperCard],Play smithyCard,Deal 3 [smithyCard,estateCard,villageCard],Play estateCard,Play goldCard,Play copperCard,Buy provinceCard,Deal 5 [villageCard,villageCard,smithyCard,estateCard,copperCard]
                  , Turn 20 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [copperCard,goldCard,copperCard,silverCard,estateCard,silverCard], discard = [provinceCard,copperCard,copperCard,goldCard,provinceCard,duchyCard,goldCard,copperCard,copperCard,goldCard,silverCard,provinceCard,copperCard,silverCard,silverCard,silverCard,goldCard,provinceCard], hand = [silverCard,estateCard,silverCard,provinceCard,estateCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 20, strategy = bigMoneyStrategy})
                  , Play silverCard,Play estateCard,Play silverCard,Play provinceCard,Play estateCard,Buy estateCard,Deal 5 [copperCard,goldCard,copperCard,silverCard,estateCard]
                  , Turn 20 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [provinceCard,copperCard,goldCard,silverCard,goldCard,estateCard,goldCard,smithyCard,estateCard], discard = [copperCard,copperCard,goldCard,goldCard,estateCard,smithyCard,silverCard,provinceCard,provinceCard,silverCard,provinceCard,copperCard,silverCard,copperCard,goldCard], hand = [copperCard,silverCard,provinceCard,copperCard,silverCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 20, strategy = bigSmithyStrategy})
                  , Play copperCard,Play silverCard,Play provinceCard,Play copperCard,Play silverCard,Buy goldCard,Deal 5 [provinceCard,copperCard,goldCard,silverCard,goldCard]
                  , Turn 20 (DominionPlayer {playerName = "villageSmithyEngine4", deck = [copperCard,cellarCard,copperCard], discard = [smithyCard,estateCard,villageCard,copperCard,goldCard,estateCard,smithyCard,smithyCard,cellarCard,villageCard,smithyCard,copperCard,goldCard,copperCard,villageCard,villageCard,copperCard,provinceCard], hand = [villageCard,villageCard,smithyCard,estateCard,copperCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 20, strategy = villageSmithyEngine4})
                  , Play villageCard,Deal 1 [copperCard],Play villageCard,Deal 1 [cellarCard],Play smithyCard,Deal 3 [copperCard,villageCard,copperCard],Play estateCard,Play copperCard,Play copperCard,Play cellarCard,Discard [copperCard,copperCard],Discard [copperCard,copperCard],Deal 2 [smithyCard,copperCard],Play villageCard,Deal 1 [goldCard],Play smithyCard,Deal 3 [estateCard,copperCard,cellarCard],Play copperCard,Play goldCard,Play estateCard,Play copperCard,Play cellarCard,Discard [],Discard [],Buy duchyCard,Deal 5 [villageCard,villageCard,estateCard,villageCard,goldCard]
                  , Turn 21 (DominionPlayer {playerName = "bigMoneyStrategy", deck = [silverCard], discard = [estateCard,provinceCard,silverCard,estateCard,silverCard,estateCard,provinceCard,copperCard,copperCard,goldCard,provinceCard,duchyCard,goldCard,copperCard,copperCard,goldCard,silverCard,provinceCard,copperCard,silverCard,silverCard,silverCard,goldCard,provinceCard], hand = [copperCard,goldCard,copperCard,silverCard,estateCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 21, strategy = bigMoneyStrategy})
                  , Play copperCard,Play goldCard,Play copperCard,Play silverCard,Play estateCard,Buy duchyCard,Deal 5 [silverCard,goldCard,silverCard,silverCard,silverCard]
                  , Turn 21 (DominionPlayer {playerName = "bigSmithyStrategy", deck = [estateCard,goldCard,smithyCard,estateCard], discard = [silverCard,copperCard,provinceCard,silverCard,copperCard,goldCard,copperCard,copperCard,goldCard,goldCard,estateCard,smithyCard,silverCard,provinceCard,provinceCard,silverCard,provinceCard,copperCard,silverCard,copperCard,goldCard], hand = [provinceCard,copperCard,goldCard,silverCard,goldCard], played = [], actions = 1, buys = 1, money = 0, victory = 0, turns = 21, strategy = bigSmithyStrategy})
                  , Play provinceCard,Play copperCard,Play goldCard,Play silverCard,Play goldCard,Buy provinceCard
                  , Deal 5 [estateCard,goldCard,smithyCard,estateCard,goldCard]
                  , GameOver [("bigMoneyStrategy",40),("bigSmithyStrategy",38),("villageSmithyEngine4",6)]
                  ]
      let dtree = [DominionTree
                    [ GameTurn 0 [ PlayerTurn "bigMoneyStrategy" [] []
                                 , PlayerTurn "bigSmithyStrategy" [] [] 
                                 , PlayerTurn "villageSmithyEngine4" [] []
                                 ]
                    , GameTurn 1 [ PlayerTurn "bigMoneyStrategy"
                                    [ Standard estateCard, Standard copperCard, Standard copperCard, Standard estateCard, Standard copperCard ]
                                    [ BoughtCard silverCard ]
                                 , PlayerTurn "bigSmithyStrategy"
                                    [ Standard estateCard, Standard copperCard, Standard copperCard, Standard estateCard, Standard copperCard ]
                                    [ BoughtCard silverCard ]
                                 , PlayerTurn "villageSmithyEngine4"
                                    [ Standard estateCard, Standard copperCard, Standard copperCard, Standard copperCard, Standard copperCard ]
                                    [ BoughtCard smithyCard ]
                                 ]
                    , GameTurn 2 [ PlayerTurn "bigMoneyStrategy"
                                    [ Standard copperCard, Standard copperCard, Standard estateCard, Standard copperCard, Standard copperCard]
                                    [ BoughtCard silverCard ]
                                 , PlayerTurn "bigSmithyStrategy"
                                    [ Standard copperCard, Standard copperCard, Standard copperCard, Standard estateCard, Standard copperCard]
                                    [ BoughtCard smithyCard ]
                                 , PlayerTurn "villageSmithyEngine4"
                                    [ Standard copperCard, Standard copperCard, Standard estateCard, Standard copperCard, Standard estateCard]
                                    [ BoughtCard villageCard ]
                                 ]
                    ]
                    (Right 2)
                  ]
      buildDominionTrees moves [Right 2] `shouldBe` dtree
