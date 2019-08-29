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

import           Control.Lens
import           Control.Monad.RWS
import qualified Data.DList                  as DL
import           Data.Generics.Product
import           Data.List                   (groupBy, sort)
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text
import           DeckBuilding.Dominion.Cards
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils
import           DeckBuilding.Types
import           System.Random               (StdGen)
import           System.Random.Shuffle       (shuffle')

-- Dominion

-- Core Engine

-- | Creates a new player with a name and strategy and the default started deck.
newPlayer :: Text.Text -> Strategy -> DominionPlayer
-- newPlayer n | trace ("newPlayer: " ++ show n) False = undefined
newPlayer n = DominionPlayer n [] (replicate 7 copperCard ++ replicate 3 estateCard) [] [] 1 1 0 0 0

{- |
  Evaluates the cards in the deck. Since cards can cause more to be drawn,
  the default case is to run a card and then recursively call with the new
  hand for the player.

  If the player is out of actions we can only run Value cards (ones that don't
  require actions), and skip all cards that require actions.
-}
evaluateHand :: Int -> DominionState Int
evaluateHand pnum = do
  player <- findPlayer pnum
  mc <- (player ^. field @"strategy" . field @"nextCard") pnum
  case mc of
    Nothing -> return pnum
    Just c -> do
      yay <- evaluateCard c pnum player
      if yay
         then evaluateHand pnum
         else return pnum

evaluateCard :: Card -> Int -> DominionPlayer -> DominionState Bool
evaluateCard c@(Card _ _ _ Value _) pnum player = evaluateCard' c pnum
evaluateCard c pnum player@(DominionPlayer _ _ _ _ _ 0 _ _ _ _ _) = return False
evaluateCard c pnum player = evaluateCard' c pnum

evaluateCard' :: Card -> Int -> DominionState Bool
evaluateCard' c pnum = do
  tell $ DL.singleton $ Play c
  _ <- (c ^. field @"action") c pnum
  return True

-- | Returns the list of players in total points order, highest first.
sortByPoints :: DominionState [DominionPlayer]
sortByPoints = do
  players' <- use $ field @"players"
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
resetTurn :: Int -> DominionState Int
resetTurn p = do
  player <- findPlayer p
  (field @"players" . ix p . field @"discard") %= ( ((player ^. field @"hand") ++ (player ^. field @"played") ) ++)
  (field @"players" . ix p . field @"played") .= []
  (field @"players" . ix p . field @"hand") .= []
  (field @"players" . ix p . field @"actions") .= 1
  (field @"players" . ix p . field @"buys") .= 1
  (field @"players" . ix p . field @"money") .= 0
  (field @"players" . ix p . field @"victory") .= 0
  (field @"players" . ix p . field @"turns") += 1
  return p

configToGame :: DominionConfig -> StdGen -> DominionGame
configToGame c = DominionGame
                  (map (\p -> newPlayer (fst p) (snd p)) (c ^. field @"playerDefs"))
                  (basicDecks (length (c ^. field @"playerDefs")) `Map.union` makeDecks (c ^. field @"kingdomCards"))
                  []

instance Game DominionConfig (DL.DList DominionMove) DominionGame where
  finished    = do
    decks' <- use $ field @"decks"
    emptyDecks <- numEmptyDecks
    return $ (decks' Map.! provinceCard == 0) || emptyDecks >= 3

  runTurn p   = do
    player <- findPlayer p
    tell $ DL.singleton $ Turn (player ^. field @"turns") player
    _ <- evaluateHand p
    _ <- (player ^. field @"strategy" . field @"buyStrategy") p
    _ <- resetTurn p
    _ <- deal 5 p
    finished

  result      = do
      turnOrder' <- turnOrder
      mapM_ tallyPoints turnOrder'
      players' <- sortByPoints
      tell $ DL.singleton $ GameOver $ map (\p -> (p ^. field @"playerName", p ^. field @"victory")) players'
      let grouped = groupBy (\p1 p2 -> (p1 ^. field @"victory") == (p2 ^. field @"victory") && (p1 ^. field @"turns") == (p2 ^. field @"turns")) players'
      return $ result' ((length . head) grouped) players'
    where result' 1 l = Left $ playerName $ head l
          result' n _ = Right n

  turnOrder  = do
    players' <- use $ field @"players"
    return [0 .. (length players' - 1)]

  tallyPoints p = do
    player <- findPlayer p
    (field @"players" . ix p . field @"hand") .= ((player ^. field @"deck") ++ (player ^. field @"discard") ++ (player ^. field @"hand") ++ (player ^. field @"played"))
    player' <- findPlayer p
    mapM_ victoryPts (player' ^. field @"hand")
      where victoryPts :: Card -> DominionState Int
            victoryPts c@(Card _ _ _ _ s) = s c p
