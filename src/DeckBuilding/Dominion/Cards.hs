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
    , bureaucratCard
    , gardensCard
    , militiaCard
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
import Control.Monad.State

-- Cards and their actions

goldCard        = Card "Gold"       6 (valueCard 3 0) Value

silverCardAction :: Card -> Player -> State Game Player
silverCardAction c p    = doSilver merchantPlayed
  where merchantPlayed  = merchantCard `elem` (p ^.played)
        doSilver True   = valueCard 3 0 c p
        doSilver False  = valueCard 2 0 c p

silverCard      = Card "Silver"     3 silverCardAction Value

copperCard      = Card "Copper"     0 (valueCard 1 0) Value

provinceCard    = Card "Province"   8 (valueCard 0 6) Value

duchyCard       = Card "Province"   5 (valueCard 0 3) Value

estateCard      = Card "Estate"     2 (valueCard 0 1) Value

curseCard       = Card "Curse"      0 (valueCard 0 (-1)) Value

victoryCards    = [provinceCard, duchyCard, estateCard, curseCard, gardensCard]

marketCard      = Card "Market"     5 (basicCardAction 1 0 1 1 0) Action

moatCard        = Card "Moat"       2 (basicCardAction 2 (-1) 0 0 0) Action

smithyCard      = Card "Smithy"     4 (basicCardAction 3 (-1) 0 0 0) Action

villageCard     = Card "Village"    3 (basicCardAction 1 1 0 0 0) Action

festivalCard    = Card "Festival"   5 (basicCardAction 0 1 1 2 0) Action

laboratoryCard  = Card "Laboratory" 5 (basicCardAction 2 0 0 0 0) Action

woodcutterCard  = Card "Woodcutter" 3 (basicCardAction 0 0 1 2 0) Action

cellarCardAction :: Card -> Player -> State Game Player
cellarCardAction c p = do
  if hasActionsLeft p
    then do
      p' <- bigMoneyDiscard (0, (length (p ^. hand))) (over played (c:) (over hand (delete c) p))
      deal (length (p ^. hand) - (length (p' ^. hand))) p'
    else return p

cellarCard      = Card "Cellar"     2 cellarCardAction Action

chapelCardAction :: Card -> Player -> State Game Player
chapelCardAction c p = do
  if hasActionsLeft p
    then do
      bigMoneyTrash (0, 4) (over played (c:) (over hand (delete c) p))
    else return p

chapelCard     = Card "Chapel"      2 chapelCardAction Action

harbingerCardAction :: Card -> Player -> State Game Player
harbingerCardAction c p = do
  if hasActionsLeft p
    then do
      p' <- deal 1 p
      bigMoneyRetrieve (0, 1) (over played (c:) (over hand (delete c) p'))
    else return p

harbingerCard   = Card "Harbinger"  3 harbingerCardAction Action

-- Merchant Card does not deal with the case where the silver is played after
-- the merchant card
merchantCardAction :: Card -> Player -> State Game Player
merchantCardAction c p = do
  let silverPlayed = silverCard `elem` (p ^. played ++ p ^. hand)
  basicCardAction 1 0 0 (money silverPlayed) 0 c p
  where money True    = 2
        money False   = 1

merchantCard    = Card "Merchant"   3 merchantCardAction Action

vassalCardAction :: Card -> Player -> State Game Player
vassalCardAction c p = do
  if hasActionsLeft p
    then do
      gs <- get
      p' <- basicCardAction 0 0 0 2 0 c p
      let (enoughDeck, newDiscard)
              | length (p' ^. deck) >= 1        = (p' ^. deck, p' ^. discard)
              | otherwise                       = ( (p' ^. deck) ++ (shuffle' (p' ^. discard) (length (p' ^. discard)) (gs ^. random)), [])
      let topOfDeck Nothing                     = p'
      let topOfDeck (Just c)                    = if (c^. cardType) == Value
                                                    then updatePlayer (over discard (c:) (set deck (tail enoughDeck) p'))
                                                    else do
                                                      p'' <- updatePlayer (over played (c:) (set discard newDiscard (set hand (delete c enoughDeck) p')))
                                                      (c ^. action) c p''
      topOfDeck $ find (\_ -> True) enoughDeck
    else return p

vassalCard      = Card "Vassal"     3 vassalCardAction Action

defendsAgainstAttack :: Card -> Player -> Bool
defendsAgainstAttack _ p = moatCard `elem` (p ^. hand)

discardVictory :: Player -> State Game Player
discardVictory p = if defendsAgainstAttack bureaucratCard p
                      then return p
                      else updatePlayer $ found $ find (\c -> c `elem` victoryCards) (p ^. hand)
  where found Nothing   = p
        found (Just c)  = over hand (delete c) $ over discard (c:) p

bureaucratCardAction :: Card -> Player -> State Game Player
bureaucratCardAction c p = do
  gs <- get
  mapM discardVictory (delete p (gs ^. players))
  updatePlayer $ over deck (silverCard:) p

bureaucratCard  = Card "Bureaucrat" 4 bureaucratCardAction Action

gardensCardAction :: Card -> Player -> State Game Player
gardensCardAction c p = do
  let points = length ( (p ^. hand) ++ (p ^. discard) ++ (p ^. played) ++ (p ^. deck) ) `div` 10
  updatePlayer $ over victory (+ points) p

gardensCard     = Card "Gardens"    4 gardensCardAction Value

militiaDiscard :: Player -> State Game Player
militiaDiscard p = if defendsAgainstAttack militiaCard p
                      then return p
                      else doDiscard ( (length (p ^. hand)) - 3, (length (p ^. hand)) - 3 ) victoryCards p

militiaCardAction :: Card -> Player -> State Game Player
militiaCardAction c p = do
  gs <- get
  mapM militiaDiscard (delete p (gs ^. players))
  updatePlayer $ over money (+2) p

militiaCard     = Card "Militia"    4 militiaCardAction Action

-- Big money

bigMoneyBuy :: Player -> State Game Player
bigMoneyBuy p = doBuys p bigMoneyCards
  where bigMoneyCards = [provinceCard, goldCard, silverCard]

bigMoneyDiscard :: (Int, Int) -> Player -> State Game Player
bigMoneyDiscard rng = doDiscard rng discardCards
  where discardCards = victoryCards ++ [copperCard]

bigMoneyTrash :: (Int, Int) -> Player -> State Game Player
bigMoneyTrash rng = doTrash rng trashCards
  where trashCards = [curseCard, estateCard, copperCard]

bigMoneyRetrieve :: (Int, Int) -> Player -> State Game Player
bigMoneyRetrieve rng = doRetrieveDiscard rng retrieveCards
  where retrieveCards = [goldCard, marketCard, festivalCard, villageCard, laboratoryCard, smithyCard, moatCard, silverCard]
