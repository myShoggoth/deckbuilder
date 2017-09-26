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
    , sentryCard
    , libraryCard
    , artisanCard
    , workshopCard
    , treasureCards
    , victoryCards
    , kingdomCards2ndEdition
    , firstGameKingdomCards
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

-- | For value cards, pass money and victory point values.
valueCard :: Int -> Int -> Card -> Player -> State Game Player
valueCard m v c p = updatePlayer $ over hand (delete c) $ over played (c:) $ over money (+m) $ over victory (+v) p

platinumCard    = Card "Platinum"   9 (valueCard 5 0) Value

goldCard        = Card "Gold"       6 (valueCard 3 0) Value

-- | Silver cards need extra logic to make Merchant work in all cases.
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

duchyCard       = Card "Duchy"      5 (valueCard 0 3) Value

estateCard      = Card "Estate"     2 (valueCard 0 1) Value

curseCard       = Card "Curse"      0 (valueCard 0 (-1)) Value

-- | Cards that affect victory values.
victoryCards    = [curseCard, estateCard, duchyCard, gardensCard, provinceCard]

-- | For basic card values: draw cards, +actions, +buys, +money, +victory
basicCardAction :: Int -> Int -> Int -> Int -> Int -> Card -> Player -> State Game Player
basicCardAction draw a b m v c p = do
  p' <- updatePlayer $ over hand (delete c) $ over played (c:) $ over actions (+a) $ over buys (+b) $ over money (+m) $ over victory (+v) p
  deal draw p'

marketCard      = Card "Market"     5 (basicCardAction 1 0 1 1 0) Action

moatCard        = Card "Moat"       2 (basicCardAction 2 (-1) 0 0 0) Action

smithyCard      = Card "Smithy"     4 (basicCardAction 3 (-1) 0 0 0) Action

villageCard     = Card "Village"    3 (basicCardAction 1 1 0 0 0) Action

festivalCard    = Card "Festival"   5 (basicCardAction 0 1 1 2 0) Action

laboratoryCard  = Card "Laboratory" 5 (basicCardAction 2 0 0 0 0) Action

cellarCardAction :: Card -> Player -> State Game Player
cellarCardAction c p = do
  p' <- (p ^. strategy . discardStrategy) (0, length (p ^. hand)) (over played (c:) (over actions (+ (-1)) (over hand (delete c) p)))
  deal (length (p ^. hand) - length (p' ^. hand)) p'

cellarCard      = Card "Cellar"     2 cellarCardAction Action

chapelCardAction :: Card -> Player -> State Game Player
chapelCardAction c p = (p ^. strategy . trashStrategy) (0, 4) (over played (c:) (over actions (+ (-1)) (over hand (delete c) p)))

chapelCard     = Card "Chapel"      2 chapelCardAction Action

harbingerCardAction :: Card -> Player -> State Game Player
harbingerCardAction c p = do
  p' <- deal 1 p
  (p ^. strategy . retrieveStrategy) (0, 1) (over played (c:) (over hand (delete c) p'))

harbingerCard   = Card "Harbinger"  3 harbingerCardAction Action

merchantCardAction :: Card -> Player -> State Game Player
merchantCardAction c p = basicCardAction 1 0 0 (money silverPlayed) 0 c p
  where money True    = 2
        money False   = 1
        silverPlayed  = silverCard `elem` (p ^. played ++ p ^. hand)

merchantCard    = Card "Merchant"   3 merchantCardAction Action

vassalCardAction :: Card -> Player -> State Game Player
vassalCardAction c p = do
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
bureaucratCardAction c p = do
  gs <- get
  mapM_ discardVictory (delete p (gs ^. players))
  updatePlayer $ over deck (silverCard:) $ over played (c:) $ over actions (+ (-1)) p

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
militiaCardAction c p = do
  gs <- get
  mapM_ militiaDiscard (delete p (gs ^. players))
  updatePlayer $ over money (+2) $ over played (c:) $ over actions (+ (-1)) p

militiaCard     = Card "Militia"    4 militiaCardAction Action

moneylenderCardAction :: Card -> Player -> State Game Player
moneylenderCardAction c p = return $ copper $ find (== copperCard) (p ^. hand)
  where copper Nothing    = over discard (c:) p
        copper (Just cop) = over hand (delete cop) $ over money (+3) $ over played (c:) $ over actions (+ (-1)) p

moneylenderCard = Card "Moneylender"  4 moneylenderCardAction Action

poacherCardAction :: Card -> Player -> State Game Player
poacherCardAction c p = do
  p' <- basicCardAction 1 0 0 1 0 c p
  emptyDecks <- numEmptyDecks
  (p' ^. strategy . discardStrategy) (emptyDecks, emptyDecks) p'

poacherCard     = Card "Poacher"      4 poacherCardAction Action

remodelCardAction :: Card -> Player -> State Game Player
remodelCardAction c p = do
  p' <- (p ^. strategy . trashStrategy) (0, 1) p
  let diff = (p ^. hand) \\ (p' ^. hand)
  if length diff == 1
    then do
      let p'' = over played (c:) p'
      updatePlayer p''
      (p'' ^. strategy . gainCardStrategy) (head diff ^. cost + 2) p''
    else return p'

remodelCard     = Card "Remodel"      4 remodelCardAction Action

throneRoomCardAction :: Card -> Player -> State Game Player
throneRoomCardAction c p = do
  mc <- (p ^. strategy . throneRoomStrategy) p
  playCard mc
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
banditCardAction c p = do
  gs <- get
  mapM_ banditDiscard (delete p (gs ^. players))
  updatePlayer $ over discard (goldCard:) $ over played (c:) $ over actions (+ (-1)) p

banditCard      = Card "Bandit"       5 banditCardAction Action

councilRoomDraw :: Player -> State Game Player
councilRoomDraw p = do
  p' <- deal 1 p
  updatePlayer p'

councilRoomCardAction :: Card -> Player -> State Game Player
councilRoomCardAction c p = do
  gs <- get
  mapM_ councilRoomDraw (delete p (gs ^. players))
  basicCardAction 4 (-1) 0 0 0 c p

councilRoomCard = Card "Council Room" 5 councilRoomCardAction Action

gainCurse :: Player -> State Game Player
gainCurse p = if defendsAgainstAttack witchCard p
  then return p
  else updatePlayer $ over discard (curseCard:) p

witchCardAction :: Card -> Player -> State Game Player
witchCardAction c p = do
  gs <- get
  mapM_ gainCurse (delete p (gs ^. players))
  basicCardAction 2 (-1) 0 0 0 c p

witchCard       = Card "Witch"        5 witchCardAction Action

mineCardAction :: Card -> Player -> State Game Player
mineCardAction c p = do
  mc <- firstCardInPlay $ intersect (p ^. hand) treasureCards
  mine mc
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
libraryCardAction c p = drawTo 7 $ over actions (+ (-1)) p

libraryCard     = Card "Library"      5 libraryCardAction Action

trashDiscardOrReorder :: [Card] -> Player -> State Game Player
trashDiscardOrReorder c p = do
  (trash, disc, keep) <- (p ^. strategy . sentryStrategy) c p
  gs <- get
  let (Just p') = find (== p) (gs ^. players)
  updatePlayer $ over discard (disc ++) $ over deck (keep ++) p'

sentryCardAction :: Card -> Player -> State Game Player
sentryCardAction c p = do
  p' <- basicCardAction 1 0 0 0 0 c p
  p'' <- deal 2 p'
  let (newcards, oldhand) = splitAt 2 (p'' ^. hand)
  p''' <- trashDiscardOrReorder newcards p''
  updatePlayer $ set hand oldhand p'''

sentryCard    = Card "Sentry"       5 sentryCardAction Action

artisanCardAction :: Card -> Player -> State Game Player
artisanCardAction c p = do
  p' <- updatePlayer $ over played (c:) $ over actions (+ (-1)) p
  p'' <- (p' ^. strategy . gainCardStrategy) 5 p'
  let newcard = head (p'' ^. discard)
  p''' <- updatePlayer $ over hand (newcard:) $ over discard (delete newcard) p''
  (p''' ^. strategy . handToDeckStrategy) 1 p'''

artisanCard   = Card "Artisan"      6 artisanCardAction Action

workshopCardAction :: Card -> Player -> State Game Player
workshopCardAction c p = do
  p' <- updatePlayer $ over played (c:) $ over actions (+ (-1)) p
  (p' ^. strategy . gainCardStrategy) 4 p'

workshopCard  = Card "Workshop"     3 workshopCardAction Action

-- | The kingdom cards from Dominion 2nd edition.
kingdomCards2ndEdition = [
    marketCard
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
  , sentryCard
  , libraryCard
  , artisanCard
  ]

-- | The ten kingdom cards recommended for a player's first game.
firstGameKingdomCards = [
    cellarCard
  , marketCard
  , merchantCard
  , militiaCard
  , mineCard
  , moatCard
  , remodelCard
  , smithyCard
  , villageCard
  , workshopCard
  ]
