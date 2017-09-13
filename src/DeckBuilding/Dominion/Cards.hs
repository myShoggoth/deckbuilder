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
    , merchantCard
    , vassalCard
    , bigMoneyBuy
    , bigMoneyDiscard
    , bigMoneyTrash
    , bigMoneyRetrieve
    ) where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Utils
import DeckBuilding.Dominion.Strategies.Basic

import Data.List (delete, find, sortBy, group, sort, groupBy, intersect)
import System.Random.Shuffle
import Control.Lens

-- Cards and their actions

goldCard      = Card "Gold" 6 (valueCard 3 0) Value

silverCardAction :: Card -> Player -> GameState -> GameState
silverCardAction c p gs = doSilver merchantPlayed
  where merchantPlayed  = merchantCard `elem` (p' ^.played)
        Just p'         = find (== p) (gs ^. players)
        doSilver True   = valueCard 3 0 c p gs
        doSilver False  = valueCard 2 0 c p gs

silverCard    = Card "Silver" 3 silverCardAction Value

copperCard    = Card "Copper" 0 (valueCard 1 0) Value

provinceCard  = Card "Province" 8 (valueCard 0 6) Value

duchyCard     = Card "Province" 5 (valueCard 0 3) Value

estateCard    = Card "Estate" 2 (valueCard 0 1) Value

curseCard     = Card "Curse" 0 (valueCard 0 (-1)) Value

marketCard      = Card "Market"     5 (basicCardAction 1 0 1 1 0) Action

moatCard        = Card "Moat"       2 (basicCardAction 2 (-1) 0 0 0) Action

smithyCard      = Card "Smithy"     4 (basicCardAction 3 (-1) 0 0 0) Action

villageCard     = Card "Village"    3 (basicCardAction 1 1 0 0 0) Action

festivalCard    = Card "Festival"   5 (basicCardAction 0 1 1 2 0) Action

laboratoryCard  = Card "Laboratory" 5 (basicCardAction 2 0 0 0 0) Action

woodcutterCard  = Card "Woodcutter" 3 (basicCardAction 0 0 1 2 0) Action

cellarCardAction :: Card -> Player -> GameState -> GameState
cellarCardAction c p gs = gs''
  where player (Player _ _ _ _ _ 0 _ _ _) = gs
        player _                          = bigMoneyDiscard (0, (length (p' ^. hand))) (over played (c:) (over hand (delete c) p')) gs
        Just p'                           = find (== p) (gs ^. players)
        gs'                               = player p'
        Just p''                          = find (== p) (gs' ^. players)
        gs''                              = deal (length (p' ^. hand) - (length (p'' ^. hand))) p'' gs'

cellarCard      = Card "Cellar"     2 cellarCardAction Action

chapelCardAction :: Card -> Player -> GameState -> GameState
chapelCardAction c p gs = player p
  where player (Player _ _ _ _ _ 0 _ _ _) = gs
        player _                          = bigMoneyTrash (0, 4) (over played (c:) (over hand (delete c) p')) gs
        Just p'                           = find (== p) (gs ^. players)

chapelCard     = Card "Chapel"      2 chapelCardAction Action

harbingerCardAction :: Card -> Player -> GameState -> GameState
harbingerCardAction c p gs = player p
  where player (Player _ _ _ _ _ 0 _ _ _) = gs
        player _                          = bigMoneyRetrieve (0, 1) (over played (c:) (over hand (delete c) p'')) gs
        Just p'                           = find (== p) (gs ^. players)
        gs'                               = deal 1 p' gs
        Just p''                          = find (== p) (gs' ^. players)

harbingerCard   = Card "Harbinger"  3 harbingerCardAction Action

-- Merchant Card does not deal with the case where the silver is played after
-- the merchant card
merchantCardAction :: Card -> Player -> GameState -> GameState
merchantCardAction c p gs = basicCardAction 1 0 0 (money silverPlayed) 0 c p' gs
  where silverPlayed  = silverCard `elem` (p' ^. played ++ p' ^. hand)
        Just p'       = find (== p) (gs ^. players)
        money True    = 2
        money False   = 1

merchantCard    = Card "Merchant"   3 merchantCardAction Action

hasActionsLeft :: Player -> Bool
hasActionsLeft (Player _ _ _ _ _ 0 _ _ _) = False
hasActionsLeft _                          = True

vassalCardAction :: Card -> Player -> GameState -> GameState
vassalCardAction c p gs = topOfDeck $ find (\_ -> True) enoughDeck
  where player (Player _ _ _ _ _ 0 _ _ _)         = gs
        player _                                  = basicCardAction 0 0 0 2 0 c p gs
        gs'                                       = player p
        Just p'                                   = find (== p) $ gs' ^. players
        (enoughDeck, newDiscard)
            | length (p' ^. deck) >= 1            = (p' ^. deck, p' ^. discard)
            | otherwise                           = ( (p' ^. deck) ++ (shuffle' (p' ^. discard) (length (p' ^. discard)) (gs ^. random)), [])
        topOfDeck Nothing                         = gs'
        topOfDeck (Just c@(Card _ _ _ Value))     = changeTurn (over discard (c:) (set hand (delete c enoughDeck) p')) gs'
        topOfDeck (Just c)                        = (c ^. action) c (over played (c:) (set discard newDiscard (set hand (delete c enoughDeck) p'))) gs'

vassalCard      = Card "Vassal"     3 vassalCardAction Action


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
