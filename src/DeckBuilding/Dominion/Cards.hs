module DeckBuilding.Dominion.Cards
    ( platinumCard
    , goldCard
    , silverCard
    , copperCard
    , colonyCard
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
    , cellarCard
    , chapelCard
    , harbingerCard
    , merchantCard
    , vassalCard
    , bureaucratCard
    , gardensCard
    , militiaCard
    , moneylenderCard
    , poacherCard
    , remodelCard
    , throneRoomCard
    , banditCard
    , councilRoomCard
    , witchCard
    , mineCard
    , libraryCard
    , treasureCards
    , victoryCards
    ) where

import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils

import           Control.Lens
import           Control.Monad.State
import           Data.List                   (delete, find, group, groupBy,
                                              intersect, sort, sortBy, (\\))
import qualified Data.Map                    as Map
import           System.Random.Shuffle
import           Data.Foldable               (foldrM)

-- Cards and their actions

valueCard :: Int -> Int -> Card -> Player -> State Game Player
valueCard m v c p = updatePlayer $ over hand (delete c) $ over played (c:) $ over money (+m) $ over victory (+v) p

platinumCard    = Card "Platinum"   9 (valueCard 5 0) Value

goldCard        = Card "Gold"       6 (valueCard 3 0) Value

silverCardAction :: Card -> Player -> State Game Player
silverCardAction c p    = doSilver merchantPlayed
  where merchantPlayed  = merchantCard `elem` (p ^.played)
        doSilver True   = valueCard 3 0 c p
        doSilver False  = valueCard 2 0 c p

silverCard      = Card "Silver"     3 silverCardAction Value

copperCard      = Card "Copper"     0 (valueCard 1 0) Value

treasureCards   = [platinumCard, goldCard, silverCard, copperCard]

colonyCard      = Card "Colony"     11 (valueCard 0 10) Value

provinceCard    = Card "Province"   8 (valueCard 0 6) Value

duchyCard       = Card "Province"   5 (valueCard 0 3) Value

estateCard      = Card "Estate"     2 (valueCard 0 1) Value

curseCard       = Card "Curse"      0 (valueCard 0 (-1)) Value

victoryCards    = [curseCard, estateCard, duchyCard, gardensCard, provinceCard]

basicCardAction :: Int -> Int -> Int -> Int -> Int -> Card -> Player -> State Game Player
basicCardAction draw a b m v c p = if hasActionsLeft p
    then do
      p' <- deal draw p
      let player = over hand (delete c) $ over played (c:) $ over actions (+a) $ over buys (+b) $ over money (+m) $ over victory (+v) p'
      updatePlayer player
    else return p

marketCard      = Card "Market"     5 (basicCardAction 1 0 1 1 0) Action

moatCard        = Card "Moat"       2 (basicCardAction 2 (-1) 0 0 0) Action

smithyCard      = Card "Smithy"     4 (basicCardAction 3 (-1) 0 0 0) Action

villageCard     = Card "Village"    3 (basicCardAction 1 1 0 0 0) Action

festivalCard    = Card "Festival"   5 (basicCardAction 0 1 1 2 0) Action

laboratoryCard  = Card "Laboratory" 5 (basicCardAction 2 0 0 0 0) Action

cellarCardAction :: Card -> Player -> State Game Player
cellarCardAction c p = if hasActionsLeft p
    then do
      p' <- (p ^. strategy . discardStrategy) (0, length (p ^. hand)) (over played (c:) (over actions (+ (-1)) (over hand (delete c) p)))
      deal (length (p ^. hand) - length (p' ^. hand)) p'
    else return p

cellarCard      = Card "Cellar"     2 cellarCardAction Action

chapelCardAction :: Card -> Player -> State Game Player
chapelCardAction c p = if hasActionsLeft p
    then (p ^. strategy . trashStrategy) (0, 4) (over played (c:) (over actions (+ (-1)) (over hand (delete c) p)))
    else return p

chapelCard     = Card "Chapel"      2 chapelCardAction Action

harbingerCardAction :: Card -> Player -> State Game Player
harbingerCardAction c p = if hasActionsLeft p
    then do
      p' <- deal 1 p
      (p ^. strategy . retrieveStrategy) (0, 1) (over played (c:) (over hand (delete c) p'))
    else return p

harbingerCard   = Card "Harbinger"  3 harbingerCardAction Action

merchantCardAction :: Card -> Player -> State Game Player
merchantCardAction c p = basicCardAction 1 0 0 (money silverPlayed) 0 c p
  where money True    = 2
        money False   = 1
        silverPlayed  = silverCard `elem` (p ^. played ++ p ^. hand)

merchantCard    = Card "Merchant"   3 merchantCardAction Action

vassalCardAction :: Card -> Player -> State Game Player
vassalCardAction c p = if hasActionsLeft p
    then do
      gs <- get
      p' <- basicCardAction 0 (-1) 0 2 0 c p
      let (enoughDeck, newDiscard)
              | not (null (p' ^. deck)) = (p' ^. deck, p' ^. discard)
              | otherwise               = ( (p' ^. deck) ++ shuffle' (p' ^. discard) (length (p' ^. discard)) (gs ^. random), [])
      let topOfDeck Nothing             = p'
      let topOfDeck (Just c)            = if (c^. cardType) == Value
          then updatePlayer (over discard (c:) (set deck (tail enoughDeck) p'))
          else do
            p'' <- updatePlayer (over actions (+1) (set discard newDiscard (set hand (delete c enoughDeck) p')))
            (c ^. action) c p''
      topOfDeck $ find (const True) enoughDeck
    else return p

vassalCard      = Card "Vassal"     3 vassalCardAction Action

defendsAgainstAttack :: Card -> Player -> Bool
defendsAgainstAttack _ p = moatCard `elem` (p ^. hand)

discardVictory :: Player -> State Game Player
discardVictory p = if defendsAgainstAttack bureaucratCard p
    then return p
    else updatePlayer $ found $ find (`elem` victoryCards) (p ^. hand)
  where found Nothing   = p
        found (Just c)  = over hand (delete c) $ over discard (c:) p

bureaucratCardAction :: Card -> Player -> State Game Player
bureaucratCardAction c p = if hasActionsLeft p
    then do
      gs <- get
      mapM_ discardVictory (delete p (gs ^. players))
      updatePlayer $ over deck (silverCard:) $ over played (c:) $ over actions (+ (-1)) p
    else return p

bureaucratCard  = Card "Bureaucrat" 4 bureaucratCardAction Action

gardensCardAction :: Card -> Player -> State Game Player
gardensCardAction c p = do
  let points = length ( (p ^. hand) ++ (p ^. discard) ++ (p ^. played) ++ (p ^. deck) ) `div` 10
  updatePlayer $ over victory (+ points) $ over played (c:) p

gardensCard     = Card "Gardens"    4 gardensCardAction Value

militiaDiscard :: Player -> State Game Player
militiaDiscard p = if defendsAgainstAttack militiaCard p
    then return p
    else (p ^. strategy . discardStrategy) ( length (p ^. hand) - 3, length (p ^. hand) - 3 ) p

militiaCardAction :: Card -> Player -> State Game Player
militiaCardAction c p = if hasActionsLeft p
    then do
      gs <- get
      mapM_ militiaDiscard (delete p (gs ^. players))
      updatePlayer $ over money (+2) $ over played (c:) $ over actions (+ (-1)) p
    else return p

militiaCard     = Card "Militia"    4 militiaCardAction Action

moneylenderCardAction :: Card -> Player -> State Game Player
moneylenderCardAction c p = if hasActionsLeft p
    then return $ copper $ find (== copperCard) (p ^. hand)
    else return p
  where copper Nothing    = over discard (c:) p
        copper (Just cop) = over hand (delete cop) $ over money (+3) $ over played (c:) $ over actions (+ (-1)) p

moneylenderCard = Card "Moneylender"  4 moneylenderCardAction Action

poacherCardAction :: Card -> Player -> State Game Player
poacherCardAction c p = if hasActionsLeft p
    then do
      p' <- basicCardAction 1 0 0 1 0 c p
      emptyDecks <- numEmptyDecks
      (p' ^. strategy . discardStrategy) (emptyDecks, emptyDecks) p'
    else return p

poacherCard     = Card "Poacher"      4 poacherCardAction Action

remodelCardAction :: Card -> Player -> State Game Player
remodelCardAction c p = if hasActionsLeft p
    then do
      p' <- (p ^. strategy . trashStrategy) (0, 1) p
      let diff = (p ^. hand) \\ (p' ^. hand)
      if length diff == 1
        then do
          let p'' = over played (c:) p'
          updatePlayer p''
          (p'' ^. strategy . gainCardStrategy) (head diff ^. cost + 2) p''
        else return p'
    else return p

remodelCard     = Card "Remodel"      4 remodelCardAction Action

throneRoomCardAction :: Card -> Player -> State Game Player
throneRoomCardAction c p = if hasActionsLeft p
    then do
      mc <- (p ^. strategy . throneRoomStrategy) p
      playCard mc
    else return p
  where playCard Nothing      = return p
        playCard (Just card)  = do
          p' <- updatePlayer $ over actions (+1) $ over played (c:) $ over hand (delete c) $ over hand (card:) p
          p'' <- (card ^. action) card p'
          p''' <- updatePlayer $ over played (delete card) p''
          (card ^. action) card p'''

throneRoomCard  = Card "Throne Room"  4 throneRoomCardAction Action

banditDiscard :: Player -> State Game Player
banditDiscard p = if defendsAgainstAttack militiaCard p
    then return p
    else do
      let (toptwo, therest) = splitAt 2 (p ^. deck)
      let totrash           = take 1 $ intersect toptwo (delete copperCard (reverse treasureCards))
      let todiscard         = toptwo \\ totrash
      updatePlayer $ set deck therest $ over discard (todiscard++) p

banditCardAction :: Card -> Player -> State Game Player
banditCardAction c p = if hasActionsLeft p
    then do
      gs <- get
      mapM_ banditDiscard (delete p (gs ^. players))
      updatePlayer $ over discard (goldCard:) $ over played (c:) $ over actions (+ (-1)) p
    else return p

banditCard      = Card "Bandit"       5 banditCardAction Action

councilRoomDraw :: Player -> State Game Player
councilRoomDraw p = do
    p' <- deal 1 p
    updatePlayer p'

councilRoomCardAction :: Card -> Player -> State Game Player
councilRoomCardAction c p = if hasActionsLeft p
    then do
      gs <- get
      mapM_ councilRoomDraw (delete p (gs ^. players))
      basicCardAction 4 (-1) 0 0 0 c p
    else return p

councilRoomCard = Card "Council Room" 5 councilRoomCardAction Action

gainCurse :: Player -> State Game Player
gainCurse p = if defendsAgainstAttack witchCard p
    then return p
    else updatePlayer $ over discard (curseCard:) p

witchCardAction :: Card -> Player -> State Game Player
witchCardAction c p = if hasActionsLeft p
    then do
      gs <- get
      mapM_ gainCurse (delete p (gs ^. players))
      basicCardAction 2 (-1) 0 0 0 c p
    else return p

witchCard       = Card "Witch"        5 witchCardAction Action

mineCardAction :: Card -> Player -> State Game Player
mineCardAction c p = if hasActionsLeft p
    then do
      mc <- firstCardInPlay $ intersect (p ^. hand) treasureCards
      mine mc
    else return p
  where mine Nothing            = return p
        mine (Just c)           = mine' c
        mine' card
          | card == copperCard  = exchange copperCard silverCard
          | card == silverCard  = exchange silverCard goldCard
          | card == goldCard    = exchange goldCard platinumCard
          | otherwise           = return p
        exchange c1 c2    = do
          gs <- get
          put $ over decks (Map.mapWithKey (decreaseCards c2)) gs
          updatePlayer $ over hand (delete c1) $ over hand (c2:) $ over actions (+ (-1)) p

mineCard          = Card "Mine"       5 mineCardAction Action

discardOrPlay :: Card -> Player -> State Game Player
discardOrPlay c p = do
  keep <- (p ^. strategy . libraryStrategy) c
  if (c ^. cardType) == Value || keep
    then return p
    else updatePlayer $ over discard (c:) $ over hand (delete c) p

drawTo :: Int -> Player -> State Game Player
drawTo num p = do
  let todraw = num - length (p ^. hand)
  if todraw == 0
    then return p
    else do
      p' <- deal todraw p
      let (newhand, oldhand) = splitAt todraw (p' ^. hand)
      p'' <- foldrM discardOrPlay p' newhand
      drawTo num p''

libraryCardAction :: Card -> Player -> State Game Player
libraryCardAction c p = if hasActionsLeft p
    then drawTo 7 $ over actions (+ (-1)) p
    else return p

libraryCard     = Card "Library"      5 libraryCardAction Action
