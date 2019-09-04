{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

{-|
Module      : DeckBuilding.Legendary
Description : A deck-building game engine and simulator
Copyright   : (c) Andrew F. Boardman, 2017
License     : GPL-3
Maintainer  : andrew@myshoggoth.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@. Totes.
-}
module DeckBuilding.Legendary
  () where

import           Control.Lens
import           Control.Monad.RWS
import qualified Data.DList                   as DL
import qualified Data.List                    as L
import           Data.Generics.Product
import           DeckBuilding.Legendary.Types
import           DeckBuilding.Legendary.Utils
import           DeckBuilding.Types

{-
doTurn -
  draw villain -- only this part is legendary specific -- basically a game state update
    run villain action if any
    push villains to next city square if needed
    villain escape action if applicable and there is one
    mastermind escape action if applicable and there is one
  loop of
    play card
    buys
    attacks
  deal cards for new turn
  reset player data for new turn
  is game over?
gameResult
-}

{- |
  Evaluates the cards in the deck. Since cards can cause more to be drawn,
  the default case is to run a card and then recursively call with the new
  hand for the player.
-}
evaluateHand :: Int -> [Card] -> LegendaryState Int
evaluateHand pnum []     = return pnum
evaluateHand pnum (x:_) = do
  tell $ DL.singleton $ Play x
  (x ^. field @"action") x pnum >>= evaluateHand pnum

-- | Move played cards to discard pile, reset actions, buys, money, victory.
resetTurn :: Int -> LegendaryState Int
resetTurn p = do
  player <- findPlayer p
  (field @"players" . ix p . field @"discard") %= ( ((player ^. field @"hand") ++ (player ^. field @"played") ) ++)
  (field @"players" . ix p . field @"played") .= []
  (field @"players" . ix p . field @"hand") .= []
  (field @"players" . ix p . field @"money") .= 0
  (field @"players" . ix p . field @"victory") .= 0
  (field @"players" . ix p . field @"turns") += 1
  return p

-- | Returns the list of players in total points order, highest first.
sortByPoints :: LegendaryState [LegendaryPlayer]
sortByPoints = do
  players' <- use $ field @"players"
  return $ L.sort players'

instance Game LegendaryConfig (DL.DList LegendaryMove) LegendaryGame where
  finished    = do
    gs <- get
    mmEvilWins' <- gs ^. field @"mastermind" . field @"mmEvilWins"
    sEvilWins' <- gs ^. field @"scheme" . field @"sEvilWins"
    villainDeck' <- use $ field @"villainDeck"
    return $ mmEvilWins' && sEvilWins' && L.null villainDeck'

  runTurn p = do
    player <- findPlayer p
    tell $ DL.singleton $ Turn (player ^. field @"turns") player
    _ <- (player ^. field @"strategy" . field @"orderHand") p
      >>= evaluateHand p
      >>= (player ^. field @"strategy" . field @"buyStrategy")
      >>= (player ^. field @"strategy" . field @"attackStrategy")
      >>= deal 6
    _ <- resetTurn p
    finished

  result      = do
      turnOrder' <- turnOrder
      mapM_ tallyPoints turnOrder'
      players' <- sortByPoints
      let grouped = L.groupBy (\p1 p2 -> (p1 ^. field @"victory") == (p2 ^. field @"victory") && (p1 ^. field @"turns") == (p2 ^. field @"turns")) players'
      return $ result' ((length . head) grouped) players'
    where result' 1 l = Left $ (playerName $ head l, victory $ head l)
          result' n _ = Right n

  turnOrder  = do
    players' <- use $ field @"players"
    return [0 .. length players' - 1]

  tallyPoints p = do
    player <- findPlayer p
    (field @"players" . ix p . field @"hand") .= ((player ^. field @"deck") ++ (player ^. field @"discard") ++ (player ^. field @"hand") ++ (player ^. field @"played"))
    _ <- evaluateHand p (player ^. field @"hand")
    return ()
