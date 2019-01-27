module DeckBuilding.Dominion.Cards.Base
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
    , baseSetActionTerminators
    ) where

import           DeckBuilding.Dominion.Cards.Utils
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils

import           Control.Lens
import           Control.Monad.RWS
import qualified Data.DList                        as DL
import           Data.Foldable                     (foldrM)
import           Data.List                         (delete, find, group,
                                                    groupBy, intersect, sort,
                                                    sortBy, (\\))
import qualified Data.Map                          as Map
import           System.Random.Shuffle

-- Cards and their actions

goldCard        = Card "Gold"       6 (valueCard 3 0) Value

-- | Silver cards need extra logic to make Merchant work in all cases.
silverCardAction :: Card -> Int -> DominionState Int
silverCardAction c p    = do
    player <- findPlayer p
    let monies = if (merchantCard `elem` (player ^. played)) && not (silverCard `elem` (player ^. played))
                    then 3
                    else 2
    valueCard monies 0 c p

silverCard      = Card "Silver"     3 silverCardAction Value

copperCard      = Card "Copper"     0 (valueCard 1 0) Value

treasureCards   = [goldCard, silverCard, copperCard]

provinceCard    = Card "Province"   8 (valueCard 0 6) Value

duchyCard       = Card "Duchy"      5 (valueCard 0 3) Value

estateCard      = Card "Estate"     2 (valueCard 0 1) Value

curseCard       = Card "Curse"      0 (valueCard 0 (-1)) Value

-- | Cards that affect victory values.
victoryCards    = [curseCard, estateCard, duchyCard, gardensCard, provinceCard]

marketCard      = Card "Market"     5 (basicCardAction 1 0 1 1 0) Action

moatCard        = Card "Moat"       2 (basicCardAction 2 (-1) 0 0 0) Action

smithyCard      = Card "Smithy"     4 (basicCardAction 3 (-1) 0 0 0) Action

villageCard     = Card "Village"    3 (basicCardAction 1 1 0 0 0) Action

festivalCard    = Card "Festival"   5 (basicCardAction 0 1 1 2 0) Action

laboratoryCard  = Card "Laboratory" 5 (basicCardAction 2 0 0 0 0) Action

cellarCardAction :: Card -> Int -> DominionState Int
cellarCardAction c p = do
  player <- findPlayer p
  discarded <- (player ^. strategy . discardStrategy) (0, length (player ^. hand)) p
  tell $ DL.singleton $ Discard discarded
  basicCardAction (length discarded) (-1) 0 0 0 c p

cellarCard      = Card "Cellar"     2 cellarCardAction Action

chapelCardAction :: Card -> Int -> DominionState Int
chapelCardAction c p = do
  player <- findPlayer p
  _ <- (player ^. strategy . trashStrategy) (0, 4) p
  basicCardAction 0 (-1) 0 0 0 c p

chapelCard     = Card "Chapel"      2 chapelCardAction Action

harbingerCardAction :: Card -> Int -> DominionState Int
harbingerCardAction c p = do
  player <- findPlayer p
  deal 1 p
  (player ^. strategy . retrieveStrategy) (0, 1) p
  basicCardAction 0 0 0 0 0 c p

harbingerCard   = Card "Harbinger"  3 harbingerCardAction Action

merchantCardAction :: Card -> Int -> DominionState Int
merchantCardAction c p = do
  player <- findPlayer p
  let silverPlayed = silverCard `elem` (player ^. played)
  basicCardAction 1 0 0 (monies silverPlayed) 0 c p
  where monies True  = 1
        monies False = 0

merchantCard    = Card "Merchant"   3 merchantCardAction Action

vassalCardAction :: Card -> Int -> DominionState Int
vassalCardAction c p = do
  player <- findPlayer p
  r <- use random
  basicCardAction 0 (-1) 0 2 0 c p
  let (enoughDeck, newDiscard)
        | not (null (player ^. deck)) = (player ^. deck, player ^. discard)
        | otherwise                   = ( (player ^. deck) ++ shuffle' (player ^. discard) (length (player ^. discard)) r, [])
  let topOfDeck Nothing                 = p
  let topOfDeck (Just c)                = if (c ^. cardType) == Value
      then do
        (players . ix p . discard) %= (c:)
        (players . ix p . deck) .= (tail enoughDeck)
        return p
      else do
        (players . ix p . actions) += 1
        (players . ix p . discard) .= newDiscard
        (players . ix p . hand) .= (delete c enoughDeck)
        (c ^. action) c p
  topOfDeck $ find (const True) enoughDeck

vassalCard      = Card "Vassal"     3 vassalCardAction Action

defendsAgainstAttack :: Card -> DominionPlayer -> Bool
defendsAgainstAttack _ p = moatCard `elem` (p ^. hand)

discardVictory :: Int -> Int -> DominionState (Maybe Card)
discardVictory e p | p == e = return Nothing
discardVictory _ p = do
  player <- findPlayer p
  if defendsAgainstAttack bureaucratCard player
    then return Nothing
    else do
      case find (`elem` victoryCards) (player ^. hand) of
        Nothing -> return Nothing
        Just c  -> do
          (players . ix p . hand) %= (delete c)
          (players . ix p . discard) %= (c:)
          return $ Just c

bureaucratCardAction :: Card -> Int -> DominionState Int
bureaucratCardAction c p = do
  (players . ix p . deck) %= (silverCard:)
  players <- use players
  mapM_ (discardVictory p) [0.. (length players) - 1]
  decks %= (Map.mapWithKey (decreaseCards silverCard))
  basicCardAction 0 (-1) 0 0 0 c p

bureaucratCard  = Card "Bureaucrat" 4 bureaucratCardAction Action

gardensCardAction :: Card -> Int -> DominionState Int
gardensCardAction c p = do
  player <- findPlayer p
  let points = length ( (player ^. hand) ++ (player ^. discard) ++ (player ^. played) ++ (player ^. deck) ) `div` 10
  valueCard 0 points c p

gardensCard     = Card "Gardens"    4 gardensCardAction Value

militiaDiscard :: Int -> Int -> DominionState [Card]
militiaDiscard e p | p == e = return []
militiaDiscard _ p = do
  player <- findPlayer p
  if defendsAgainstAttack militiaCard player
    then return []
    else do
      (player ^. strategy . discardStrategy) ( length (player ^. hand) - 3, length (player ^. hand) - 3 ) p

militiaCardAction :: Card -> Int -> DominionState Int
militiaCardAction c p = do
  players <- use players
  mapM_ (militiaDiscard p) [0.. (length players) - 1]
  basicCardAction 0 (-1) 0 2 0 c p

militiaCard     = Card "Militia"    4 militiaCardAction Action

moneylenderCardAction :: Card -> Int -> DominionState Int
moneylenderCardAction c p = do
  player <- findPlayer p
  if copperCard `elem` (player ^. hand)
    then do
      (players . ix p . hand) %= (delete copperCard)
      trash %= (copperCard:)
      basicCardAction 0 (-1) 0 3 0 c p
    else return p

moneylenderCard = Card "Moneylender"  4 moneylenderCardAction Action

poacherCardAction :: Card -> Int -> DominionState Int
poacherCardAction c p = do
  player <- findPlayer p
  basicCardAction 1 0 0 1 0 c p
  emptyDecks <- numEmptyDecks
  (player ^. strategy . discardStrategy) (emptyDecks, emptyDecks) p
  return p

poacherCard     = Card "Poacher"      4 poacherCardAction Action

remodelCardAction :: Card -> Int -> DominionState Int
remodelCardAction c p = do
  player <- findPlayer p
  diff <- (player ^. strategy . trashStrategy) (0, 1) p
  if length diff == 1
    then do
      newCard <- (player ^. strategy . gainCardStrategy) (head diff ^. cost + 2) p
      case newCard of
        Nothing -> do
          basicCardAction 0 0 0 0 0 c p
          (players . ix p . hand) %= (diff++)
          trsh <- use trash
          trash .= (trsh \\ diff)
        Just card -> do
          basicCardAction 0 (-1) 0 0 0 c p
          tell $ DL.singleton $ Remodel (head diff) card
      return newCard
    else do
      basicCardAction 0 0 0 0 0 c p
      return Nothing
  return p

remodelCard     = Card "Remodel"      4 remodelCardAction Action

throneRoomCardAction :: Card -> Int -> DominionState Int
throneRoomCardAction c p = do
  player <- findPlayer p
  mc <- (player ^. strategy . throneRoomStrategy) p
  case mc of
    Nothing     -> basicCardAction 0 0 0 0 0 c p
    (Just card) -> do
      basicCardAction 0 1 0 0 0 c p
      (players . ix p . hand) %= (card:)
      (card ^. action) card p
      (players . ix p . played) %= (delete card)
      (card ^. action) card p
      tell $ DL.singleton $ ThroneRoom card
      return p

throneRoomCard  = Card "Throne Room"  4 throneRoomCardAction Action

banditDiscard :: Int -> Int -> DominionState ()
banditDiscard e p | p == e = return ()
banditDiscard _ p = do
  player <- findPlayer p
  if defendsAgainstAttack banditCard player
    then return ()
    else do
      toptwo <- deal 2 p
      let totrash   = take 1 $ intersect toptwo (delete copperCard (reverse treasureCards))
      let todiscard = toptwo \\ totrash
      trash %= (totrash ++)
      (players . ix p . discard) %= (todiscard++)
      return ()

banditCardAction :: Card -> Int -> DominionState Int
banditCardAction c p = do
  ps <- use players
  mapM_ (banditDiscard p) [0.. (length ps) - 1]
  (players . ix p . discard) %= (goldCard:)
  decks %= (Map.mapWithKey (decreaseCards goldCard))
  basicCardAction 0 (-1) 0 0 0 c p

banditCard      = Card "Bandit"       5 banditCardAction Action

councilRoomDraw :: Int -> Int -> DominionState [Card]
councilRoomDraw e p | p == e = return []
councilRoomDraw _ p = deal 1 p

councilRoomCardAction :: Card -> Int -> DominionState Int
councilRoomCardAction c p = do
  ps <- use players
  mapM_ (councilRoomDraw p) [0.. (length ps) - 1]
  basicCardAction 4 (-1) 0 0 0 c p

councilRoomCard = Card "Council Room" 5 councilRoomCardAction Action

gainCurse :: Int -> Int -> DominionState Bool
gainCurse e p | p == e = return False
gainCurse _ p = do
  player <- findPlayer p
  if defendsAgainstAttack witchCard player
    then return False
    else do
      (players . ix p . discard) %= (curseCard:)
      decks %= (Map.mapWithKey (decreaseCards curseCard))
      return True

witchCardAction :: Card -> Int -> DominionState Int
witchCardAction c p = do
  ps <- use players
  mapM_ (gainCurse p) [0.. (length ps) - 1]
  basicCardAction 2 (-1) 0 0 0 c p

witchCard       = Card "Witch"        5 witchCardAction Action

exch :: Card -> Card -> Card -> Int -> DominionState Int
exch c c1 c2 p = do
  decks %= (Map.mapWithKey (decreaseCards c2))
  (players . ix p . hand) %= (delete c1)
  (players . ix p . hand) %= (c2:)
  basicCardAction 0 (-1) 0 0 0 c p

mineCardAction :: Card -> Int -> DominionState Int
mineCardAction c p = do
  player <- findPlayer p
  mc <- firstCardInPlay $ intersect (player ^. hand) treasureCards
  case mc of
    Nothing -> return p
    (Just card)
        | card == copperCard  -> exch c copperCard silverCard p
        | card == silverCard  -> exch c silverCard goldCard p
        | otherwise           -> return p

mineCard          = Card "Mine"       5 mineCardAction Action

discardOrPlay :: Card -> Int -> DominionState Int
discardOrPlay c p = do
  player <- findPlayer p
  keep <- (player ^. strategy . libraryStrategy) c
  if (c ^. cardType) == Value || keep
    then return p
    else do
      (players . ix p . discard) %= (c:)
      (players . ix p . hand) %= (delete c)
      return p

drawTo :: Int -> Int -> DominionState Int
drawTo num p = do
  player <- findPlayer p
  let todraw = num - length (player ^. hand)
  if todraw <= 0
    then return p
    else do
      newcards <- deal todraw p
      foldrM discardOrPlay p newcards
      drawTo num p

libraryCardAction :: Card -> Int -> DominionState Int
libraryCardAction c p = do
  drawTo 7 p
  basicCardAction 0 (-1) 0 0 0 c p

libraryCard     = Card "Library"      5 libraryCardAction Action

sentryCardAction :: Card -> Int -> DominionState Int
sentryCardAction c p = do
  basicCardAction 1 0 0 0 0 c p
  player <- findPlayer p
  let oldhand = player ^. hand
  newcards <- deal 2 p
  (trashem, disc, keep) <- (player ^. strategy . sentryStrategy) newcards p
  trash %= (trashem ++)
  (players . ix p . discard) %= (disc ++)
  (players . ix p . deck) %= (keep ++)
  (players . ix p . hand) .= oldhand
  return p

sentryCard    = Card "Sentry"       5 sentryCardAction Action

artisanCardAction :: Card -> Int -> DominionState Int
artisanCardAction c p = do
  player <- findPlayer p
  mc <- (player ^. strategy . gainCardStrategy) 5 p
  case mc of
    Nothing   -> return p
    Just card -> do
      basicCardAction 0 (-1) 0 0 0 c p
      decks %= (Map.mapWithKey (decreaseCards card))
      (players . ix p . hand) %= (card:)
      (players . ix p . deck) %= (delete card) -- gainCardStrategy puts it in the deck by default
      (player ^. strategy . handToDeckStrategy) 1 p
      return p

artisanCard   = Card "Artisan"      6 artisanCardAction Action

workshopCardAction :: Card -> Int -> DominionState Int
workshopCardAction c p = do
  basicCardAction 0 (-1) 0 0 0 c p
  player <- findPlayer p
  (player ^. strategy . gainCardStrategy) 4 p
  return p

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

baseSetActionTerminators = [
    chapelCard
  , moatCard
  , vassalCard
  , workshopCard
  , bureaucratCard
  , militiaCard
  , moneylenderCard
  , remodelCard
  , smithyCard
  , throneRoomCard
  , banditCard
  , councilRoomCard
  , libraryCard
  , mineCard
  , witchCard
  , artisanCard
  ]
