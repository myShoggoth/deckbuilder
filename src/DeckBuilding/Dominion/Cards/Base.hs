{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}

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

import           Control.Lens
import           Control.Monad.RWS
import qualified Data.DList                        as DL
import           Data.Foldable                     (foldrM)
import           Data.Generics.Product
import           Data.List                         (delete, find, intersect,
                                                    (\\))
import qualified Data.Map                          as Map
import           DeckBuilding.Dominion.Cards.Utils
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils
import           System.Random.Shuffle

-- Cards and their actions

goldCard :: Card
goldCard        = Card "Gold"       6 (valueCard 3) Value (simpleVictory 0)

-- | Silver cards need extra logic to make Merchant work in all cases.
silverCardAction :: Card -> Int -> DominionState Int
silverCardAction c p    = do
    player <- findPlayer p
    let monies = if (merchantCard `elem` (player ^. field @"played")) && (silverCard `notElem` (player ^. field @"played"))
                    then 3
                    else 2
    valueCard monies c p

silverCard :: Card
silverCard      = Card "Silver"     3 silverCardAction Value (simpleVictory 0)

copperCard :: Card
copperCard      = Card "Copper"     0 (valueCard 1) Value (simpleVictory 0)

treasureCards :: [Card]
treasureCards   = [goldCard, silverCard, copperCard]

provinceCard :: Card
provinceCard    = Card "Province"   8 (valueCard 0) Value (simpleVictory 6)

duchyCard :: Card
duchyCard       = Card "Duchy"      5 (valueCard 0) Value (simpleVictory 3)

estateCard :: Card
estateCard      = Card "Estate"     2 (valueCard 0) Value (simpleVictory 1)

curseCard :: Card
curseCard       = Card "Curse"      0 (valueCard 0 ) Value (simpleVictory (-1))

-- | Cards that affect victory values.
victoryCards :: [Card]
victoryCards    = [curseCard, estateCard, duchyCard, gardensCard, provinceCard]

marketCard :: Card
marketCard      = Card "Market"     5 (basicCardAction 1 0 1 1) Action (simpleVictory 0)

moatCard :: Card
moatCard        = Card "Moat"       2 (basicCardAction 2 (-1) 0 0) Action (simpleVictory 0)

smithyCard :: Card
smithyCard      = Card "Smithy"     4 (basicCardAction 3 (-1) 0 0) Action (simpleVictory 0)

villageCard :: Card
villageCard     = Card "Village"    3 (basicCardAction 1 1 0 0) Action (simpleVictory 0)

festivalCard :: Card
festivalCard    = Card "Festival"   5 (basicCardAction 0 1 1 2) Action (simpleVictory 0)

laboratoryCard :: Card
laboratoryCard  = Card "Laboratory" 5 (basicCardAction 2 0 0 0) Action (simpleVictory 0)

cellarCardAction :: Card -> Int -> DominionState Int
cellarCardAction c p = do
  player <- findPlayer p
  discarded <- (player ^. field @"strategy" . field @"discardStrategy") (0, length (player ^. field @"hand")) p
  tell $ DL.singleton $ Discard discarded
  basicCardAction (length discarded) (-1) 0 0 c p

cellarCard :: Card
cellarCard      = Card "Cellar"     2 cellarCardAction Action (simpleVictory 0)

chapelCardAction :: Card -> Int -> DominionState Int
chapelCardAction c p = do
  player <- findPlayer p
  _ <- (player ^. field @"strategy" . field @"trashStrategy") (0, 4) p
  basicCardAction 0 (-1) 0 0 c p

chapelCard :: Card
chapelCard     = Card "Chapel"      2 chapelCardAction Action (simpleVictory 0)

harbingerCardAction :: Card -> Int -> DominionState Int
harbingerCardAction c p = do
  player <- findPlayer p
  _ <- deal 1 p
  _ <- (player ^. field @"strategy" . field @"retrieveStrategy") (0, 1) p
  basicCardAction 0 0 0 0 c p

harbingerCard :: Card
harbingerCard   = Card "Harbinger"  4 harbingerCardAction Action (simpleVictory 0)

merchantCardAction :: Card -> Int -> DominionState Int
merchantCardAction c p = do
  player <- findPlayer p
  let silverPlayed = silverCard `elem` (player ^. field @"played")
  basicCardAction 1 0 0 (monies silverPlayed) c p
  where monies True  = 1
        monies False = 0

merchantCard :: Card
merchantCard    = Card "Merchant"   3 merchantCardAction Action (simpleVictory 0)

vassalCardAction :: Card -> Int -> DominionState Int
vassalCardAction c p = do
  player <- findPlayer p
  r <- use $ field @"random"
  _ <- basicCardAction 0 (-1) 0 2 c p
  let (enoughDeck, newDiscard)
        | not (null (player ^. field @"deck")) = (player ^. field @"deck", player ^. field @"discard")
        | otherwise                            = ( (player ^. field @"deck") ++ shuffle' (player ^. field @"discard") (length (player ^. field @"discard")) r, [])
  let topOfDeck Nothing                 = return p
      topOfDeck (Just c')               = if (c' ^. field @"cardType") == Value
          then do
            (field @"players" . ix p . field @"discard") %= (c':)
            (field @"players" . ix p . field @"deck") .= tail enoughDeck
            return p
          else do
            (field @"players" . ix p . field @"actions") += 1
            (field @"players" . ix p . field @"discard") .= newDiscard
            (field @"players" . ix p . field @"hand") .= delete c enoughDeck
            (c' ^. field @"action") c' p
  topOfDeck $ find (const True) enoughDeck

vassalCard :: Card
vassalCard      = Card "Vassal"     3 vassalCardAction Action (simpleVictory 0)

defendsAgainstAttack :: Card -> DominionPlayer -> Bool
defendsAgainstAttack _ p = moatCard `elem` (p ^. field @"hand")

discardVictory :: Int -> Int -> DominionState (Maybe Card)
discardVictory e p | p == e = return Nothing
discardVictory _ p = do
  player <- findPlayer p
  if defendsAgainstAttack bureaucratCard player
    then return Nothing
    else
      case find (`elem` victoryCards) (player ^. field @"hand") of
        Nothing -> return Nothing
        Just c  -> do
          (field @"players" . ix p . field @"hand") %= delete c
          (field @"players" . ix p . field @"discard") %= (c:)
          return $ Just c

bureaucratCardAction :: Card -> Int -> DominionState Int
bureaucratCardAction c p = do
  (field @"players" . ix p . field @"deck") %= (silverCard:)
  players' <- use $ field @"players"
  mapM_ (discardVictory p) [0.. length players' - 1]
  field @"decks" %= Map.mapWithKey (decreaseCards silverCard)
  basicCardAction 0 (-1) 0 0 c p

bureaucratCard :: Card
bureaucratCard  = Card "Bureaucrat" 4 bureaucratCardAction Action (simpleVictory 0)

gardensCardAction :: Card -> Int -> DominionState Int
gardensCardAction c p = do
  player <- findPlayer p
  let points = length ( (player ^. field @"hand") ++ (player ^. field @"discard") ++ (player ^. field @"played") ++ (player ^. field @"deck") ) `div` 10
  pure points

gardensCard :: Card
gardensCard     = Card "Gardens"    4 (valueCard 0) Value gardensCardAction

militiaDiscard :: Int -> Int -> DominionState [Card]
militiaDiscard e p | p == e = return []
militiaDiscard _ p = do
  player <- findPlayer p
  if defendsAgainstAttack militiaCard player
    then return []
    else
      (player ^. field @"strategy" . field @"discardStrategy") ( length (player ^. field @"hand") - 3, length (player ^. field @"hand") - 3 ) p

militiaCardAction :: Card -> Int -> DominionState Int
militiaCardAction c p = do
  players' <- use $ field @"players"
  mapM_ (militiaDiscard p) [0.. length players' - 1]
  basicCardAction 0 (-1) 0 2 c p

militiaCard :: Card
militiaCard     = Card "Militia"    4 militiaCardAction Action (simpleVictory 0)

moneylenderCardAction :: Card -> Int -> DominionState Int
moneylenderCardAction c p = do
  player <- findPlayer p
  if copperCard `elem` (player ^. field @"hand")
    then do
      (field @"players" . ix p . field @"hand") %= delete copperCard
      field @"trash" %= (copperCard :)
      basicCardAction 0 (-1) 0 3 c p
    else return p

moneylenderCard :: Card
moneylenderCard = Card "Moneylender"  4 moneylenderCardAction Action (simpleVictory 0)

poacherCardAction :: Card -> Int -> DominionState Int
poacherCardAction c p = do
  player <- findPlayer p
  _ <- basicCardAction 1 0 0 1 c p
  emptyDecks <- numEmptyDecks
  _ <- (player ^. field @"strategy" . field @"discardStrategy") (emptyDecks, emptyDecks) p
  return p

poacherCard :: Card
poacherCard     = Card "Poacher"      4 poacherCardAction Action (simpleVictory 0)

remodelCardAction :: Card -> Int -> DominionState Int
remodelCardAction c p = do
  player <- findPlayer p
  diff <- (player ^. field @"strategy" . field @"trashStrategy") (0, 1) p
  _ <- if length diff == 1
    then do
      newCard <- (player ^. field @"strategy" . field @"gainCardStrategy") (head diff ^. field @"cost" + 2) p
      case newCard of
        Nothing -> do
          _ <- basicCardAction 0 0 0 0 c p
          (field @"players" . ix p . field @"hand") %= (diff++)
          trsh <- use $ field @"trash"
          field @"trash" .= trsh \\ diff
        Just card -> do
          _ <- basicCardAction 0 (-1) 0 0 c p
          tell $ DL.singleton $ Remodel (head diff) card
      return newCard
    else do
      _ <- basicCardAction 0 0 0 0 c p
      return Nothing
  return p

remodelCard :: Card
remodelCard     = Card "Remodel"      4 remodelCardAction Action (simpleVictory 0)

throneRoomCardAction :: Card -> Int -> DominionState Int
throneRoomCardAction c p = do
  player <- findPlayer p
  mc <- (player ^. field @"strategy" . field @"throneRoomStrategy") p
  case mc of
    Nothing     -> basicCardAction 0 0 0 0 c p
    (Just card) -> do
      _ <- basicCardAction 0 1 0 0 c p
      (field @"players" . ix p . field @"hand") %= (card:)
      _ <- (card ^. field @"action") card p
      (field @"players" . ix p . field @"played") %= delete card
      _ <- (card ^. field @"action") card p
      tell $ DL.singleton $ ThroneRoom card
      return p

throneRoomCard :: Card
throneRoomCard  = Card "Throne Room"  4 throneRoomCardAction Action (simpleVictory 0)

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
      field @"trash" %= (totrash ++)
      (field @"players" . ix p . field @"discard") %= (todiscard++)
      return ()

banditCardAction :: Card -> Int -> DominionState Int
banditCardAction c p = do
  ps <- use $ field @"players"
  mapM_ (banditDiscard p) [0.. length ps - 1]
  (field @"players" . ix p . field @"discard") %= (goldCard:)
  field @"decks" %= Map.mapWithKey (decreaseCards goldCard)
  basicCardAction 0 (-1) 0 0 c p

banditCard :: Card
banditCard      = Card "Bandit"       5 banditCardAction Action (simpleVictory 0)

councilRoomDraw :: Int -> Int -> DominionState [Card]
councilRoomDraw e p | p == e = return []
councilRoomDraw _ p = deal 1 p

councilRoomCardAction :: Card -> Int -> DominionState Int
councilRoomCardAction c p = do
  ps <- use $ field @"players"
  mapM_ (councilRoomDraw p) [0.. length ps - 1]
  basicCardAction 4 (-1) 0 0 c p

councilRoomCard :: Card
councilRoomCard = Card "Council Room" 5 councilRoomCardAction Action (simpleVictory 0)

gainCurse :: Int -> Int -> DominionState Bool
gainCurse e p | p == e = return False
gainCurse _ p = do
  player <- findPlayer p
  if defendsAgainstAttack witchCard player
    then return False
    else do
      (field @"players" . ix p . field @"discard") %= (curseCard:)
      field @"decks" %= Map.mapWithKey (decreaseCards curseCard)
      return True

witchCardAction :: Card -> Int -> DominionState Int
witchCardAction c p = do
  ps <- use $ field @"players"
  mapM_ (gainCurse p) [0.. length ps - 1]
  basicCardAction 2 (-1) 0 0 c p

witchCard :: Card
witchCard       = Card "Witch"        5 witchCardAction Action (simpleVictory 0)

exch :: Card -> Card -> Card -> Int -> DominionState Int
exch c c1 c2 p = do
  field @"decks" %= Map.mapWithKey (decreaseCards c2)
  (field @"players" . ix p . field @"hand") %= delete c1
  (field @"players" . ix p . field @"hand") %= (c2:)
  basicCardAction 0 (-1) 0 0 c p

mineCardAction :: Card -> Int -> DominionState Int
mineCardAction c p = do
  player <- findPlayer p
  mc <- firstCardInPlay $ intersect (player ^. field @"hand") treasureCards
  case mc of
    Nothing -> return p
    (Just card)
        | card == copperCard  -> exch c copperCard silverCard p
        | card == silverCard  -> exch c silverCard goldCard p
        | otherwise           -> return p

mineCard :: Card
mineCard          = Card "Mine"       5 mineCardAction Action (simpleVictory 0)

discardOrPlay :: Card -> Int -> DominionState Int
discardOrPlay c p = do
  player <- findPlayer p
  keep <- (player ^. field @"strategy" . field @"libraryStrategy") c
  if (c ^. field @"cardType") == Value || keep
    then return p
    else do
      (field @"players" . ix p . field @"discard") %= (c:)
      (field @"players" . ix p . field @"hand") %= delete c
      return p

drawTo :: Int -> Int -> DominionState Int
drawTo num p = do
  player <- findPlayer p
  let todraw = num - length (player ^. field @"hand")
  if todraw <= 0
    then return p
    else do
      newcards <- deal todraw p
      _ <- foldrM discardOrPlay p newcards
      drawTo num p

libraryCardAction :: Card -> Int -> DominionState Int
libraryCardAction c p = do
  _ <- drawTo 7 p
  basicCardAction 0 (-1) 0 0 c p

libraryCard :: Card
libraryCard     = Card "Library"      5 libraryCardAction Action (simpleVictory 0)

sentryCardAction :: Card -> Int -> DominionState Int
sentryCardAction c p = do
  _ <- basicCardAction 1 0 0 0 c p
  player <- findPlayer p
  let oldhand = player ^. field @"hand"
  newcards <- deal 2 p
  (trashem, disc, keep) <- (player ^. field @"strategy" . field @"sentryStrategy") newcards p
  field @"trash" %= (trashem ++)
  (field @"players" . ix p . field @"discard") %= (disc ++)
  (field @"players" . ix p . field @"deck") %= (keep ++)
  (field @"players" . ix p . field @"hand") .= oldhand
  return p

sentryCard :: Card
sentryCard    = Card "Sentry"       5 sentryCardAction Action (simpleVictory 0)

artisanCardAction :: Card -> Int -> DominionState Int
artisanCardAction c p = do
  player <- findPlayer p
  mc <- (player ^. field @"strategy" . field @"gainCardStrategy") 5 p
  case mc of
    Nothing   -> return p
    Just card -> do
      _ <- basicCardAction 0 (-1) 0 0 c p
      field @"decks" %= Map.mapWithKey (decreaseCards card)
      (field @"players" . ix p . field @"hand") %= (card:)
      (field @"players" . ix p . field @"deck") %= delete card -- gainCardStrategy puts it in the deck by default
      _ <- (player ^. field @"strategy" . field @"handToDeckStrategy") 1 p
      return p

artisanCard :: Card
artisanCard   = Card "Artisan"      6 artisanCardAction Action (simpleVictory 0)

workshopCardAction :: Card -> Int -> DominionState Int
workshopCardAction c p = do
  _ <- basicCardAction 0 (-1) 0 0 c p
  player <- findPlayer p
  _ <- (player ^. field @"strategy" . field @"gainCardStrategy") 4 p
  return p

workshopCard :: Card
workshopCard  = Card "Workshop"     3 workshopCardAction Action (simpleVictory 0)

-- | The kingdom cards from Dominion 2nd edition.
kingdomCards2ndEdition :: [Card]
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
firstGameKingdomCards :: [Card]
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

baseSetActionTerminators :: [Card]
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
