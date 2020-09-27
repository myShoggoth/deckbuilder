{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE TypeFamilies              #-}

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

import Control.Lens ( (^.), use, (%=), (+=), (.=), Ixed(ix) )
import Control.Monad.RWS ( MonadWriter(tell) )
import qualified Data.DList as DL
import Data.Foldable (foldrM)
import Data.Generics.Product ( HasField(field) )
import Data.Generics.Labels ()
import Data.List (delete, find, intersect, (\\))
import qualified Data.Map as Map
import DeckBuilding.Dominion.Cards.Utils
    ( simpleVictory, valueCard, basicCardAction )
import DeckBuilding.Types
import DeckBuilding.Dominion.Types
    ( DominionPlayer,
      Card(Card),
      CardType(Action, Value),
      DominionState,
      DominionMove(ThroneRoom, Discard, Remodel) )
import DeckBuilding.Dominion.Utils
    ( deal, numEmptyDecks, decreaseCards, firstCardInPlay, findPlayer )
import System.Random.Shuffle ( shuffle' )

-- Cards and their actions

-- | $3
goldCard :: Card
goldCard        = Card "Gold"       6 (valueCard 3) Value (simpleVictory 0)

-- | $2
silverCard :: Card
silverCard      = Card "Silver"     3 silverCardAction Value (simpleVictory 0)
  where
    -- Silver cards need extra logic to make Merchant work in all cases.
    silverCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    silverCardAction c p    = do
        thePlayer <- findPlayer p
        let monies = if (merchantCard `elem` (thePlayer ^. field @"played")) && (silverCard `notElem` (thePlayer ^. field @"played"))
                        then 3
                        else 2
        valueCard monies c p

-- | $1
copperCard :: Card
copperCard      = Card "Copper"     0 (valueCard 1) Value (simpleVictory 0)

-- | Convience 'List' of treasure 'Card's.
treasureCards :: [Card]
treasureCards   = [goldCard, silverCard, copperCard]

-- | 6VP
provinceCard :: Card
provinceCard    = Card "Province"   8 (valueCard 0) Value (simpleVictory 6)

-- | 3VP
duchyCard :: Card
duchyCard       = Card "Duchy"      5 (valueCard 0) Value (simpleVictory 3)

-- | 1VP
estateCard :: Card
estateCard      = Card "Estate"     2 (valueCard 0) Value (simpleVictory 1)

-- | -1VP
curseCard :: Card
curseCard       = Card "Curse"      0 (valueCard 0 ) Value (simpleVictory (-1))

-- | Convience 'List' of 'Card's that affect victory values.
victoryCards :: [Card]
victoryCards    = [curseCard, estateCard, duchyCard, gardensCard, provinceCard]

-- | +1 Card
--
-- +1 Action
--
-- +1 Buy
--
-- +$1
marketCard :: Card
marketCard      = Card "Market"     5 (basicCardAction 1 0 1 1) Action (simpleVictory 0)

-- | +2 Cards
--
-- When another player plays an Attack card, you may first reveal this from your hand, to be unaffected by it.
moatCard :: Card
moatCard        = Card "Moat"       2 (basicCardAction 2 (-1) 0 0) Action (simpleVictory 0)

-- | +3 Cards
smithyCard :: Card
smithyCard      = Card "Smithy"     4 (basicCardAction 3 (-1) 0 0) Action (simpleVictory 0)

-- | +1 Card
--
-- +2 Actions
villageCard :: Card
villageCard     = Card "Village"    3 (basicCardAction 1 1 0 0) Action (simpleVictory 0)

-- | +2 Actions
--
-- +1 Buy
--
-- +$2
festivalCard :: Card
festivalCard    = Card "Festival"   5 (basicCardAction 0 1 1 2) Action (simpleVictory 0)

-- | +2 Cards
--
-- +1 Action
laboratoryCard :: Card
laboratoryCard  = Card "Laboratory" 5 (basicCardAction 2 0 0 0) Action (simpleVictory 0)

-- | +1 Action
--
-- Discard any number of cards, then draw that many.
cellarCard :: Card
cellarCard      = Card "Cellar"     2 cellarCardAction Action (simpleVictory 0)
  where
    cellarCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    cellarCardAction c p = do
      thePlayer <- findPlayer p
      discarded <- (thePlayer ^. #strategy . #discardStrategy) (0, length (thePlayer ^. #hand)) p
      tell $ DL.singleton $ Discard p discarded
      basicCardAction (length discarded) (-1) 0 0 c p

-- | Trash up to 4 cards from your hand.
chapelCard :: Card
chapelCard     = Card "Chapel"      2 chapelCardAction Action (simpleVictory 0)
  where
    chapelCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    chapelCardAction c p = do
      thePlayer <- findPlayer p
      _ <- (thePlayer ^. #strategy . #trashStrategy) (0, 4) p
      basicCardAction 0 (-1) 0 0 c p

-- | +1 Card
--
-- +1 Action
--
-- Look through your discard pile. You may put a card from it onto your deck.
harbingerCard :: Card
harbingerCard   = Card "Harbinger"  4 harbingerCardAction Action (simpleVictory 0)
  where
    harbingerCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    harbingerCardAction c p = do
      thePlayer <- findPlayer p
      _ <- deal 1 p
      _ <- (thePlayer ^. #strategy . #retrieveStrategy) (0, 1) p
      basicCardAction 0 0 0 0 c p

-- | +1 Card
--
-- +1 Action
--
-- The first time you play a Silver this turn, +$1.
merchantCard :: Card
merchantCard    = Card "Merchant"   3 merchantCardAction Action (simpleVictory 0)
  where
    merchantCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    merchantCardAction c p = do
      thePlayer <- findPlayer p
      let silverPlayed = silverCard `elem` (thePlayer ^. #played)
      basicCardAction 1 0 0 (monies silverPlayed) c p
      where monies True  = 1
            monies False = 0

-- | +$2
--
-- Discard the top card of your deck. If it's an Action card, you may play it.
vassalCard :: Card
vassalCard      = Card "Vassal"     3 vassalCardAction Action (simpleVictory 0)
  where
    vassalCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    vassalCardAction c p = do
      thePlayer <- findPlayer p
      r <- use #random
      _ <- basicCardAction 0 (-1) 0 2 c p
      let (enoughDeck, newDiscard)
            | not (null (thePlayer ^. #deck)) = (thePlayer ^. #deck, thePlayer ^. #discard)
            | otherwise                            = ( (thePlayer ^. #deck) ++ shuffle' (thePlayer ^. #discard) (length (thePlayer ^. #discard)) r, [])
      let topOfDeck Nothing                 = return p
          topOfDeck (Just c')               = if (c' ^. #cardType) == Value
              then do
                (field @"players" . ix (unPlayerNumber p) . #discard) %= (c':)
                (field @"players" . ix (unPlayerNumber p) . #deck) .= tail enoughDeck
                return p
              else do
                (field @"players" . ix (unPlayerNumber p) . #actions) += 1
                (field @"players" . ix (unPlayerNumber p) . #discard) .= newDiscard
                (field @"players" . ix (unPlayerNumber p) . #hand) .= delete c enoughDeck
                (c' ^. #action) c' p
      topOfDeck $ find (const True) enoughDeck

-- TODO: This needs to move to Utils and handle cards beyond Moat.
defendsAgainstAttack :: Card -> DominionPlayer -> Bool
defendsAgainstAttack _ p = moatCard `elem` (p ^. #hand)

-- | Gain a Silver onto your deck. Each other player reveals a Victory
-- card from their hand and puts it onto their deck (or reveals a hand
-- with no Victory cards).
bureaucratCard :: Card
bureaucratCard  = Card "Bureaucrat" 4 bureaucratCardAction Action (simpleVictory 0)
  where
    bureaucratCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    bureaucratCardAction c p = do
      (field @"players" . ix (unPlayerNumber p) . #deck) %= (silverCard:)
      players' <- use #players
      mapM_ (discardVictory p) $ PlayerNumber <$> [0.. length players' - 1]
      field @"decks" %= Map.mapWithKey (decreaseCards silverCard)
      basicCardAction 0 (-1) 0 0 c p
    discardVictory :: PlayerNumber -> PlayerNumber -> DominionState (Maybe Card)
    discardVictory e p | p == e = return Nothing
    discardVictory _ p = do
      thePlayer <- findPlayer p
      if defendsAgainstAttack bureaucratCard thePlayer
        then return Nothing
        else
          case find (`elem` victoryCards) (thePlayer ^. #hand) of
            Nothing -> return Nothing
            Just c  -> do
              (field @"players" . ix (unPlayerNumber p) . #hand) %= delete c
              (field @"players" . ix (unPlayerNumber p) . #discard) %= (c:)
              return $ Just c

-- | Worth 1VP per 10 cards you have (round down).
gardensCard :: Card
gardensCard     = Card "Gardens"    4 (valueCard 0) Value gardensCardAction
  where
    gardensCardAction :: Card -> PlayerNumber -> DominionState Int
    gardensCardAction _ p = do
      thePlayer <- findPlayer p
      let points = length ( (thePlayer ^. #hand) ++ (thePlayer ^. #discard) ++ (thePlayer ^. #played) ++ (thePlayer ^. #deck) ) `div` 10
      pure points

-- | +$2
--
-- Each other player discards down to 3 cards in hand.
militiaCard :: Card
militiaCard     = Card "Militia"    4 militiaCardAction Action (simpleVictory 0)
  where
    militiaCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    militiaCardAction c p = do
      players' <- use $ #players
      mapM_ (militiaDiscard p) $ PlayerNumber <$> [0.. length players' - 1]
      basicCardAction 0 (-1) 0 2 c p
    militiaDiscard :: PlayerNumber -> PlayerNumber -> DominionState [Card]
    militiaDiscard e p | p == e = return []
    militiaDiscard _ p = do
      thePlayer <- findPlayer p
      if defendsAgainstAttack militiaCard thePlayer
        then return []
        else
          (thePlayer ^. #strategy . #discardStrategy) ( length (thePlayer ^. #hand) - 3, length (thePlayer ^. #hand) - 3 ) p

-- | You may trash a Copper from your hand for +$3.	
moneylenderCard :: Card
moneylenderCard = Card "Moneylender"  4 moneylenderCardAction Action (simpleVictory 0)
  where
    moneylenderCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    moneylenderCardAction c p = do
      thePlayer <- findPlayer p
      if copperCard `elem` (thePlayer ^. #hand)
        then do
          (field @"players" . ix (unPlayerNumber p) . #hand) %= delete copperCard
          field @"trash" %= (copperCard :)
          basicCardAction 0 (-1) 0 3 c p
        else return p

-- | +1 Card
--
-- +1 Action
--
-- +$1
--
-- Discard a card per empty Supply pile.
poacherCard :: Card
poacherCard     = Card "Poacher"      4 poacherCardAction Action (simpleVictory 0)
  where
    poacherCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    poacherCardAction c p = do
      thePlayer <- findPlayer p
      _ <- basicCardAction 1 0 0 1 c p
      emptyDecks <- numEmptyDecks
      _ <- (thePlayer ^. #strategy . #discardStrategy) (emptyDecks, emptyDecks) p
      return p

-- | Trash a card from your hand. Gain a card costing up to $2 more than it.
remodelCard :: Card
remodelCard     = Card "Remodel"      4 remodelCardAction Action (simpleVictory 0)
  where
    remodelCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    remodelCardAction c p = do
      thePlayer <- findPlayer p
      diff <- (thePlayer ^. #strategy . #trashStrategy) (0, 1) p
      _ <- if length diff == 1
        then do
          newCard <- (thePlayer ^. #strategy . #gainCardStrategy) (head diff ^. #cost + 2) p
          case newCard of
            Nothing -> do
              _ <- basicCardAction 0 0 0 0 c p
              (field @"players" . ix (unPlayerNumber p) . #hand) %= (diff++)
              trsh <- use $ #trash
              field @"trash" .= trsh \\ diff
            Just card -> do
              _ <- basicCardAction 0 (-1) 0 0 c p
              tell $ DL.singleton $ Remodel p (head diff) card
          return newCard
        else do
          _ <- basicCardAction 0 0 0 0 c p
          return Nothing
      return p

-- | You may play an Action card from your hand twice.
throneRoomCard :: Card
throneRoomCard  = Card "Throne Room"  4 throneRoomCardAction Action (simpleVictory 0)
  where
    throneRoomCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    throneRoomCardAction c p = do
      thePlayer <- findPlayer p
      mc <- (thePlayer ^. #strategy . #throneRoomStrategy) p
      case mc of
        Nothing     -> basicCardAction 0 0 0 0 c p
        (Just card) -> do
          _ <- basicCardAction 0 1 0 0 c p
          (field @"players" . ix (unPlayerNumber p) . #hand) %= (card:)
          _ <- (card ^. #action) card p
          (field @"players" . ix (unPlayerNumber p) . #played) %= delete card
          _ <- (card ^. #action) card p
          tell $ DL.singleton $ ThroneRoom p card
          return p

-- | Gain a Gold. Each other player reveals the top 2 cards of their deck,
-- trashes a revealed Treasure other than Copper, and discards the rest.
banditCard :: Card
banditCard      = Card "Bandit"       5 banditCardAction Action (simpleVictory 0)
  where
    banditCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    banditCardAction c p = do
      ps <- use #players
      mapM_ (banditDiscard p) $ PlayerNumber <$>  [0.. length ps - 1]
      (field @"players" . ix (unPlayerNumber p) . #discard) %= (goldCard:)
      field @"decks" %= Map.mapWithKey (decreaseCards goldCard)
      basicCardAction 0 (-1) 0 0 c p
    banditDiscard :: PlayerNumber -> PlayerNumber -> DominionState ()
    banditDiscard e p | p == e = return ()
    banditDiscard _ p = do
      thePlayer <- findPlayer p
      if defendsAgainstAttack banditCard thePlayer
        then return ()
        else do
          toptwo <- deal 2 p
          let totrash   = take 1 $ intersect toptwo (delete copperCard (reverse treasureCards))
          let todiscard = toptwo \\ totrash
          field @"trash" %= (totrash ++)
          (field @"players" . ix (unPlayerNumber p) . #discard) %= (todiscard++)
          return ()

-- | +4 Cards
--
-- +1 Buy
--
-- Each other player draws a card.
councilRoomCard :: Card
councilRoomCard = Card "Council Room" 5 councilRoomCardAction Action (simpleVictory 0)
  where
    councilRoomCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    councilRoomCardAction c p = do
      ps <- use $ #players
      mapM_ (councilRoomDraw p) $ PlayerNumber <$> [0.. length ps - 1]
      basicCardAction 4 (-1) 0 0 c p
    councilRoomDraw :: PlayerNumber -> PlayerNumber -> DominionState [Card]
    councilRoomDraw e p | p == e = return []
    councilRoomDraw _ p = deal 1 p

-- | +2 Cards
--
-- Each other player gains a Curse.
witchCard :: Card
witchCard       = Card "Witch"        5 witchCardAction Action (simpleVictory 0)
  where
    witchCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    witchCardAction c p = do
      ps <- use #players
      mapM_ (gainCurse p) $ PlayerNumber <$> [0.. length ps - 1]
      basicCardAction 2 (-1) 0 0 c p
    gainCurse :: PlayerNumber -> PlayerNumber -> DominionState Bool
    gainCurse e p | p == e = return False
    gainCurse _ p = do
      thePlayer <- findPlayer p
      if defendsAgainstAttack witchCard thePlayer
        then return False
        else do
          (field @"players" . ix (unPlayerNumber p) . #discard) %= (curseCard:)
          field @"decks" %= Map.mapWithKey (decreaseCards curseCard)
          return True

-- | You may trash a Treasure card from your hand. Gain a Treasure
-- card to your hand costing up to $3 more than it.
mineCard :: Card
mineCard          = Card "Mine"       5 mineCardAction Action (simpleVictory 0)
  where
    mineCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    mineCardAction c p = do
      thePlayer <- findPlayer p
      mc <- firstCardInPlay $ intersect (thePlayer ^. #hand) treasureCards
      case mc of
        Nothing -> return p
        (Just card)
            | card == copperCard  -> exch c copperCard silverCard p
            | card == silverCard  -> exch c silverCard goldCard p
            | otherwise           -> return p
    exch :: Card -> Card -> Card -> PlayerNumber -> DominionState PlayerNumber
    exch c c1 c2 p = do
      field @"decks" %= Map.mapWithKey (decreaseCards c2)
      (field @"players" . ix (unPlayerNumber p) . #hand) %= delete c1
      (field @"players" . ix (unPlayerNumber p) . #hand) %= (c2:)
      basicCardAction 0 (-1) 0 0 c p

-- | Draw until you have 7 cards in hand, skipping any Action
-- cards you choose to; set those aside, discarding them afterwards.
libraryCard :: Card
libraryCard     = Card "Library"      5 libraryCardAction Action (simpleVictory 0)
  where
    libraryCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    libraryCardAction c p = do
      _ <- drawTo 7 p
      basicCardAction 0 (-1) 0 0 c p
    drawTo :: Int -> PlayerNumber -> DominionState PlayerNumber
    drawTo num p = do
      thePlayer <- findPlayer p
      let todraw = num - length (thePlayer ^. #hand)
      if todraw <= 0
        then return p
        else do
          newcards <- deal todraw p
          _ <- foldrM discardOrPlay p newcards
          drawTo num p
    discardOrPlay :: Card -> PlayerNumber -> DominionState PlayerNumber
    discardOrPlay c p = do
      thePlayer <- findPlayer p
      keep <- (thePlayer ^. #strategy . #libraryStrategy) c
      if (c ^. #cardType) == Value || keep
        then return p
        else do
          (field @"players" . ix (unPlayerNumber p) . #discard) %= (c:)
          (field @"players" . ix (unPlayerNumber p) . #hand) %= delete c
          return p

-- | +1 Card
--
-- +1 Action
--
-- Look at the top 2 cards of your deck. Trash and/or discard any number
-- of them. Put the rest back on top in any order.
sentryCard :: Card
sentryCard    = Card "Sentry"       5 sentryCardAction Action (simpleVictory 0)
  where
    sentryCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    sentryCardAction c p = do
      _ <- basicCardAction 1 0 0 0 c p
      thePlayer <- findPlayer p
      let oldhand = thePlayer ^. #hand
      newcards <- deal 2 p
      (trashem, disc, keep) <- (thePlayer ^. #strategy . #sentryStrategy) newcards p
      field @"trash" %= (trashem ++)
      (field @"players" . ix (unPlayerNumber p) . #discard) %= (disc ++)
      (field @"players" . ix (unPlayerNumber p) . #deck) %= (keep ++)
      (field @"players" . ix (unPlayerNumber p) . #hand) .= oldhand
      return p

-- | Gain a card to your hand costing up to $5.
-- Put a card from your hand onto your deck.
artisanCard :: Card
artisanCard   = Card "Artisan"      6 artisanCardAction Action (simpleVictory 0)
  where
    artisanCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    artisanCardAction c p = do
      thePlayer <- findPlayer p
      mc <- (thePlayer ^. #strategy . #gainCardStrategy) 5 p
      case mc of
        Nothing   -> return p
        Just card -> do
          _ <- basicCardAction 0 (-1) 0 0 c p
          field @"decks" %= Map.mapWithKey (decreaseCards card)
          (field @"players" . ix (unPlayerNumber p) . #hand) %= (card:)
          (field @"players" . ix (unPlayerNumber p) . #deck) %= delete card -- gainCardStrategy puts it in the deck by default
          _ <- (thePlayer ^. #strategy . #handToDeckStrategy) 1 p
          return p

-- | Gain a card costing up to $4.
workshopCard :: Card
workshopCard  = Card "Workshop"     3 workshopCardAction Action (simpleVictory 0)
  where
    workshopCardAction :: Card -> PlayerNumber -> DominionState PlayerNumber
    workshopCardAction c p = do
      _ <- basicCardAction 0 (-1) 0 0 c p
      thePlayer <- findPlayer p
      _ <- (thePlayer ^. #strategy . #gainCardStrategy) 4 p
      return p

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
