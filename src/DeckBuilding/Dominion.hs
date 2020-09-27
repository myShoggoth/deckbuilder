{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE OverloadedLabels          #-}

{-|
Module      : DeckBuilding.Dominion
Description : A deck-building game engine and simulator
Copyright   : (c) Andrew F. Boardman, 2017
License     : GPL-3
Maintainer  : andrew@myshoggoth.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module DeckBuilding.Dominion
    ( newPlayer
    , basicDecks
    , resetTurn
    , evaluateHand
    , makeDecks
    , randomKingdomDecks
    , configToGame
    ) where

import Control.Lens ( (^.), use, (%=), (+=), (.=), Ixed(ix) )
import Control.Monad.RWS ( MonadWriter(tell) )
import qualified Data.DList as DL
import Data.Generics.Product ( HasField(field) )
import Data.Generics.Labels ()
import Data.List (groupBy, sort, group)
import qualified Data.Map as Map
import qualified Data.Text as Text
import DeckBuilding.Dominion.Cards
    ( goldCard,
      silverCard,
      copperCard,
      provinceCard,
      duchyCard,
      estateCard )
import DeckBuilding.Types ( Game(..), PlayerNumber(PlayerNumber, unPlayerNumber) )
import DeckBuilding.Dominion.Types
    ( DominionPlayer(DominionPlayer, playerName, victory),
      Strategy,
      Card(Card),
      CardType(Value),
      DominionAIGame(..),
      DominionGame(DominionGame),
      DominionConfig,
      DominionState,
      DominionMove(Play, Turn, GameOver) )
import DeckBuilding.Dominion.Utils
    ( deal, numEmptyDecks, findPlayer, discardCard, executeMoves )
import System.Random (StdGen)
import System.Random.Shuffle (shuffle')

-- Dominion

-- Core Engine

-- | Creates a new player with a name and strategy and the default started deck.
newPlayer :: Text.Text -> Strategy -> DominionPlayer
newPlayer n = DominionPlayer n [] (replicate 7 copperCard ++ replicate 3 estateCard) [] [] 1 1 0 0 0

{- |
  Evaluates the cards in the deck. Since cards can cause more to be drawn,
  the default case is to run a card and then recursively call with the new
  hand for the player.

  If the player is out of actions we can only run Value cards (ones that don't
  require actions), and skip all cards that require actions.
-}
evaluateHand :: PlayerNumber -> DominionState PlayerNumber
evaluateHand pnum = do
  thePlayer <- findPlayer pnum
  mc <- (thePlayer ^. #strategy . #nextCard) pnum
  case mc of
    Nothing -> return pnum
    Just c -> do
      evaluateCard c pnum thePlayer
      evaluateHand pnum

-- | Determine whether or not to play a card. 'Value' cards are played
-- automatically, 'Action' cards can only be played if the player has
-- remaining action points.
evaluateCard :: Card -> PlayerNumber -> DominionPlayer -> DominionState ()
evaluateCard c@(Card _ _ _ Value _) pnum _ = evaluateCard' c pnum
evaluateCard c pnum (DominionPlayer _ _ _ _ _ 0 _ _ _ _ _) = discardCard c pnum
evaluateCard c pnum _ = evaluateCard' c pnum

-- | We know the 'Card' will be played, so 'tell' to the 'Writer' that
-- we're playing the card, then call the 'action' function.
evaluateCard' :: Card -> PlayerNumber -> DominionState ()
evaluateCard' c pnum = do
  tell $ DL.singleton $ Play pnum c
  _ <- (c ^. #action) c pnum
  return ()

-- | Returns the list of players in total points order, highest first.
sortByPoints :: DominionState [DominionPlayer]
sortByPoints = do
  players' <- use #players
  return $ sort players'

-- | Given a set of potential kingdom cards, pick a random ten to play with.
randomKingdomDecks :: [Card] -> StdGen -> [Card]
randomKingdomDecks cs g = take 10 $ shuffle' cs (length cs) g

-- | Turns a list of cards into a Map of type (Card, Number in deck)
makeDecks :: [Card] -> Map.Map Card Int
makeDecks cs = Map.fromList $ (, 10) <$> cs

-- | Basic decks that are in all games, numbers based on the total players.
basicDecks :: Int -> Map.Map Card Int
basicDecks numPlayers
    | numPlayers == 2 = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 8), (duchyCard, 8), (provinceCard, 8) ]
    | otherwise       = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 12), (duchyCard, 12), (provinceCard, 12) ]

-- | Move played cards to discard pile, reset actions, buys, money, victory.
resetTurn :: PlayerNumber -> DominionState PlayerNumber
resetTurn p = do
  thePlayer <- findPlayer p
  (field @"players" . ix (unPlayerNumber p) . #discard) %= ( ((thePlayer ^. #hand) ++ (thePlayer ^. #played) ) ++)
  (field @"players" . ix (unPlayerNumber p) . #played) .= []
  (field @"players" . ix (unPlayerNumber p) . #hand) .= []
  (field @"players" . ix (unPlayerNumber p) . #actions) .= 1
  (field @"players" . ix (unPlayerNumber p) . #buys) .= 1
  (field @"players" . ix (unPlayerNumber p) . #money) .= 0
  (field @"players" . ix (unPlayerNumber p) . #victory) .= 0
  (field @"players" . ix (unPlayerNumber p) . #turns) += 1
  return p

configToGame :: DominionConfig -> StdGen -> DominionGame
configToGame c = DominionGame
                  (map (\p -> uncurry newPlayer p) (c ^. #playerDefs))
                  (basicDecks (length (c ^. #playerDefs)) `Map.union` makeDecks (c ^. #kingdomCards))
                  []

mkDominionAIGame :: PlayerNumber -> DominionState DominionAIGame
mkDominionAIGame pnum = do
  thePlayer <- findPlayer pnum
  decks' <- use $ #decks
  trash' <- use $ #trash
  pure DominionAIGame
    { playerNum = pnum
    , hand = thePlayer ^. #hand
    , played = thePlayer ^. #played
    , actions = thePlayer ^. #actions
    , buys = thePlayer ^. #buys
    , money = thePlayer ^. #money
    , turns = thePlayer ^. #turns
    , cards = buildCardMap thePlayer
    , trash = trash'
    , decks = decks'
    }
  where
    buildCardMap :: DominionPlayer -> Map.Map Card Int
    buildCardMap p = Map.fromList $ map (\x -> (head x, length x)) $ group $ sort allCards
      where
        allCards = (p ^. #hand) <> (p ^. #played) <> (p ^. #discard) <> (p ^. #deck)

instance Game DominionConfig (DL.DList DominionMove) DominionGame where
  start       = pure ()

  finished    = do
    decks' <- use #decks
    emptyDecks <- numEmptyDecks
    return $ (decks' Map.! provinceCard == 0) || emptyDecks >= 3

  runTurn p   = do
    thePlayer <- findPlayer p
    tell $ DL.singleton $ Turn p (thePlayer ^. #turns) thePlayer
    _ <- evaluateHand p
    aig <- mkDominionAIGame p
    _ <- executeMoves aig (thePlayer ^. #strategy . #buyStrategy $ aig)
    _ <- resetTurn p >>= deal 5

    finished

  result      = do
      turnOrder' <- turnOrder
      mapM_ tallyPoints turnOrder'
      players' <- sortByPoints
      tell $ DL.singleton $ GameOver $ map (\p -> (p ^. #playerName, p ^. #victory)) players'
      let grouped = groupBy (\p1 p2 -> (p1 ^. #victory) == (p2 ^. #victory) && (p1 ^. #turns) == (p2 ^. #turns)) players'
      return $ result' ((length . head) grouped) players'
    where result' 1 l = Left $ (playerName $ head l, victory $ head l)
          result' n _ = Right n

  turnOrder  = do
    players' <- use #players
    return $ PlayerNumber <$> [0 .. (length players' - 1)]

  tallyPoints p = do
    thePlayer <- findPlayer p
    (field @"players" . ix (unPlayerNumber p) . #hand) .= ((thePlayer ^. #deck) ++ (thePlayer ^. #discard) ++ (thePlayer ^. #hand) ++ (thePlayer ^. #played))
    player' <- findPlayer p
    mapM_ victoryPts (player' ^. #hand)
      where victoryPts :: Card -> DominionState Int
            victoryPts c@(Card _ _ _ _ s) = s c p
