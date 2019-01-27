{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

import           DeckBuilding.Legendary.Types
import           DeckBuilding.Types
import           DeckBuilding.Legendary.Utils

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

  If The player is out of actions we can only run Value cards (ones that don't
  require actions), and skip all cards that require actions.
-}
evaluateHand' :: Int -> [Card] -> LegendaryState Int
evaluateHand' pnum []     = return pnum
evaluateHand' pnum (x:_) = do
  tell $ DL.singleton $ Play x
  _ <- (x ^. action) x pnum
  player <- findPlayer pnum
  evaluateHand' pnum (player ^. hand)

-- | Runs the cards in the deck by offloading the work to evaluateHand'
evaluateHand :: Int -> LegendaryState Int
evaluateHand p = do
  player <- findPlayer p
  evaluateHand' p (player ^. hand)

-- | Move played cards to discard pile, reset actions, buys, money, victory.
resetTurn :: Int -> LegendaryState Int
resetTurn p = do
  player <- findPlayer p
  (players . ix p . discard) %= ( ((player ^. hand) ++ (player ^. played) ) ++)
  (players . ix p . played) .= []
  (players . ix p . hand) .= []
  (players . ix p . money) .= 0
  (players . ix p . victory) .= 0
  (players . ix p . turns) += 1
  return p

-- | Returns the list of players in total points order, highest first.
sortByPoints :: LegendaryState [LegendaryPlayer]
sortByPoints = do
  players' <- use players
  return $ L.sort players'

-- newtype OrphanLegendaryGame = OrphanLegendaryGame LegendaryGame

instance Game LegendaryConfig (DL.DList LegendaryMove) LegendaryGame where
  finished    = do
    gs <- get
    mmEvilWins' <- (gs ^. mastermind . mmEvilWins)
    sEvilWins' <- gs ^. scheme . sEvilWins
    villainDeck' <- use villainDeck
    return $ mmEvilWins' && sEvilWins' && (L.null villainDeck')

  runTurn p = do
    player <- findPlayer p
    tell $ DL.singleton $ Turn (player ^. turns) player
    _ <- (player ^. strategy . orderHand) p
    _ <- evaluateHand p
    _ <- (player ^. strategy ^. buyStrategy) p
    _ <- (player ^. strategy ^. attackStrategy) p
    _ <- deal 6 p
    _ <- resetTurn p
    finished

  result      = do
      np <- numPlayers
      mapM_ tallyPoints [0.. np - 1]
      players' <- sortByPoints
      let grouped = L.groupBy (\p1 p2 -> (p1 ^. victory) == (p2 ^. victory) && (p1 ^. turns) == (p2 ^. turns)) players'
      return $ result' ((length . head) grouped) players'
    where result' 1 l = Left $ _playerName $ head l
          result' n _ = Right n

  numPlayers  = do
    players' <- use players
    return $ length players'

  tallyPoints p = do
    player <- findPlayer p
    (players . ix p . hand) .= ((player ^. deck) ++ (player ^. discard) ++ (player ^. hand) ++ (player ^. played))
    _ <- evaluateHand p
    return ()
