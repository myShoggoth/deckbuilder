module DeckBuilding.Dominion.Cards
    ( goldCard
    , silverCard
    , copperCard
    , provinceCard
    , duchyCard
    , estateCard
    , curseCard
    , marketCard
    , moatCard
    , smithyCard
    , villageCard
    , festivalCard
    , laboratoryCard
    , woodcutterCard
    , cellarCard
    , chapelCard
    , harbingerCard
    , bigMoneyBuy
    , bigMoneyDiscard
    , bigMoneyTrash
    , bigMoneyRetrieve
    ) where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Utils
import DeckBuilding.Dominion.Strategies.Basic

import Data.List (delete, find, sortBy, group, sort, groupBy, intersect)

-- Cards and their actions

goldCard      = Card "Gold" 6 (valueCard 3 0)

silverCard    = Card "Silver" 3 (valueCard 2 0)

copperCard    = Card "Copper" 0 (valueCard 1 0)

provinceCard  = Card "Province" 8 (valueCard 0 6)

duchyCard     = Card "Province" 5 (valueCard 0 3)

estateCard    = Card "Estate" 2 (valueCard 0 1)

curseCard     = Card "Curse" 0 (valueCard 0 (-1))

marketCard      = Card "Market"     5 (basicCardAction 1 0 1 1 0)

moatCard        = Card "Moat"       2 (basicCardAction 2 (-1) 0 0 0)

smithyCard      = Card "Smithy"     4 (basicCardAction 3 (-1) 0 0 0)

villageCard     = Card "Village"    3 (basicCardAction 1 1 0 0 0)

festivalCard    = Card "Festival"   5 (basicCardAction 0 1 1 2 0)

laboratoryCard  = Card "Laboratory" 5 (basicCardAction 2 0 0 0 0)

woodcutterCard  = Card "Woodcutter" 3 (basicCardAction 0 0 1 2 0)

cellarCardAction :: Card -> Player -> GameState -> GameState
cellarCardAction c p gs = gs''
  where player (Player _ _ _ _ _ 0 _ _ _) = gs
        player _                          = bigMoneyDiscard (0, (length (_hand p))) (Player (_playerName p) (_deck p) (_discard p) (delete c (_hand p')) (c : _played p) (_actions p) (_buys p) (_money p) (_victory p')) gs
        Just p'                           = find (== p) (_players gs)
        gs'                               = player p
        gs''                              = deal (length (_hand p) - (length (_hand p''))) p'' gs'
        Just p''                          = find (== p) (_players gs')

cellarCard      = Card "Cellar"     2 cellarCardAction

chapelCardAction :: Card -> Player -> GameState -> GameState
chapelCardAction c p gs = player p
  where player (Player _ _ _ _ _ 0 _ _ _) = gs
        player _                          = bigMoneyTrash (0, 4) (Player (_playerName p') (_deck p') (_discard p') (delete c (_hand p')) (c : _played p') (_actions p') (_buys p') (_money p') (_victory p')) gs
        Just p'                           = find (== p) (_players gs)

chapelCard     = Card "Chapel"      2 chapelCardAction

harbingerCardAction :: Card -> Player -> GameState -> GameState
harbingerCardAction c p gs = player p
  where player (Player _ _ _ _ _ 0 _ _ _) = gs
        player _                          = bigMoneyRetrieve (0, 1) (Player (_playerName p'') (_deck p'') (_discard p'') (delete c (_hand p'')) (c : _played p'') (_actions p'') (_buys p'') (_money p'') (_victory p'')) gs
        Just p'                           = find (== p) (_players gs)
        gs'                               = deal 1 p' gs
        Just p''                          = find (== p) (_players gs')

harbingerCard   = Card "Harbinger"  3 harbingerCardAction

-- Merchant Card does not deal with the case where the silver is played after
-- the merchant card
merchantCardAction :: Card -> Player -> GameState -> GameState
merchantCardAction c p gs = basicCardAction 1 0 0 (money silverPlayed) 0 c p' gs
  where silverPlayed  = silverCard `elem` (_played p' ++ _hand p')
        Just p'       = find (== p) (_players gs)
        money True    = 2
        money False   = 1

merchantCard    = Card "Merchant"   3 merchantCardAction

hasActionsLeft :: Player -> Bool
hasActionsLeft (Player _ _ _ _ _ 0 _ _ _) = False
hasActionsLeft _                          = True

vassalCardAction :: Card -> Player -> GameState -> GameState
vassalCardAction c p gs = topOfDeck $ find (\_ -> True) (_hand p')
  where player (Player _ _ _ _ _ 0 _ _ _)       = gs
        player _                                = basicCardAction 0 0 0 2 0 c p gs
        Just p'                                 = find (== p) $ _players $ player p
        topOfDeck Nothing                       = gs
        topOfDeck (Just c@(Card _ _ valueCard)) = changeTurn (Player (_playerName p') (_deck p') (c : _discard p') (delete c (_hand p')) (_played p') (_actions p') (_buys p') (_money p') (_victory p')) gs
        topOfDeck (Just c)                      = (_action c) c p gs

vassalCard      = Card "Vassal"     3 vassalCardAction


-- Big money

bigMoneyBuy :: Player -> GameState -> GameState
bigMoneyBuy p gs = doBuys p bigMoneyCards gs
  where bigMoneyCards = [provinceCard, goldCard, silverCard]

bigMoneyDiscard :: (Int, Int) -> Player -> GameState -> GameState
bigMoneyDiscard rng = doDiscard rng discardCards
  where discardCards = [curseCard, estateCard, duchyCard, provinceCard, copperCard]

bigMoneyTrash :: (Int, Int) -> Player -> GameState -> GameState
bigMoneyTrash rng = doTrash rng trashCards
  where trashCards = [curseCard, estateCard, copperCard]

bigMoneyRetrieve :: (Int, Int) -> Player -> GameState -> GameState
bigMoneyRetrieve rng = doRetrieveDiscard rng retrieveCards
  where retrieveCards = [goldCard, marketCard, festivalCard, villageCard, laboratoryCard, smithyCard, moatCard, silverCard]
