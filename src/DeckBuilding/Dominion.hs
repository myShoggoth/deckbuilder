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
    , mkDominionAIGame
    , runGames
    , runDominionTurns
    , runPlayerTurns
    , runPlayerTurn
    ) where

import Control.Monad.State (evalState, runState)
import Control.Lens ( (^.), use, (%=), (+=), (.=), Ixed(ix) )
import Data.Generics.Product ( HasField(field) )
import Data.Generics.Labels ()
import qualified Data.Map as Map
import qualified Data.Text as Text
import DeckBuilding.Dominion.Cards
    ( goldCard,
      silverCard,
      copperCard,
      provinceCard,
      duchyCard,
      estateCard,
      curseCard )
import DeckBuilding.Types ( Game(..), PlayerNumber(PlayerNumber, unPlayerNumber) )
import DeckBuilding.Dominion.Types
    ( DominionPlayer(DominionPlayer, turns),
      Strategy,
      Card(Card),
      CardType(Value),
      DominionGame(DominionGame),
      DominionConfig(playerDefs, kingdomCards),
      DominionState,
      DominionTurn(DominionTurn),
      DominionPlayerTurn(DominionPlayerTurn),
      DominionAction,
      DominionDraw(DominionDraw),
      DominionBoard(DominionBoard) )
import DeckBuilding.Dominion.Utils
    ( deal, numEmptyDecks, findPlayer, discardCard, mkDominionAIGame, executeBuys, cardPlayed )
import System.Random (StdGen, RandomGen(split))
import System.Random.Shuffle (shuffle')
import Control.Monad.List ( forM_ )
import Control.Parallel.Strategies ( rseq, rpar, runEval )
import Data.Text.Prettyprint.Doc ( Doc, pretty )
import DeckBuilding.Dominion.Pretty ()

-- Dominion

-- Core Engine

-- | Creates a new player with a name and strategy and the default started deck.
newPlayer :: Text.Text -> Strategy -> DominionPlayer
newPlayer n = DominionPlayer n [] (replicate 7 copperCard ++ replicate 3 estateCard) [] [] 1 1 0 0 1

{- |
  Evaluates the cards in the deck. Since cards can cause more to be drawn,
  the default case is to run a card and then recursively call with the new
  hand for the player.

  If the player is out of actions we can only run Value cards (ones that don't
  require actions), and skip all cards that require actions.
-}
evaluateHand :: PlayerNumber -> DominionState [DominionAction]
evaluateHand pnum = do
  thePlayer <- findPlayer pnum
  mc <- (thePlayer ^. #strategy . #nextCard) pnum
  case mc of
    Nothing -> return []
    Just c -> do
      ma <- evaluateCard c pnum thePlayer
      case ma of
        Nothing -> evaluateHand pnum
        Just a -> do
          as <- evaluateHand pnum
          pure $ a : as

-- | Determine whether or not to play a card. 'Value' cards are played
-- automatically, 'Action' cards can only be played if the player has
-- remaining action points.
evaluateCard :: Card -> PlayerNumber -> DominionPlayer -> DominionState (Maybe DominionAction)
evaluateCard c@(Card _ _ _ Value _) pnum _ = evaluateCard' c pnum
evaluateCard c pnum (DominionPlayer _ _ _ _ _ 0 _ _ _ _ _) = do
  discardCard c pnum
  pure Nothing
evaluateCard c pnum _ = evaluateCard' c pnum

evaluateCard' :: Card -> PlayerNumber -> DominionState (Maybe DominionAction)
evaluateCard' c pnum = do
  mdm <- (c ^. #action) pnum
  cardPlayed c pnum
  return mdm

-- | Given a set of potential kingdom cards, pick a random ten to play with.
randomKingdomDecks :: [Card] -> StdGen -> [Card]
randomKingdomDecks cs g = take 10 $ shuffle' cs (length cs) g

-- | Turns a list of cards into a Map of type (Card, Number in deck)
makeDecks :: [Card] -> Map.Map Card Int
makeDecks cs = Map.fromList $ (, 10) <$> cs

-- | Basic decks that are in all games, numbers based on the total players.
-- TODO: Only add curses if there are witches in game.
basicDecks :: Int -> Map.Map Card Int
basicDecks numPlayers
    | numPlayers == 2 = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 8), (duchyCard, 8), (provinceCard, 8), (curseCard, 10) ]
    | otherwise       = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 12), (duchyCard, 12), (provinceCard, 12), (curseCard, 10 * (numPlayers - 1)) ]

-- | Move played cards to discard pile, reset actions, buys, money, victory.
resetTurn :: PlayerNumber -> DominionState ()
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

configToGame :: DominionConfig -> StdGen -> DominionBoard
configToGame c = DominionBoard
    (map (uncurry newPlayer) (c ^. #playerDefs))
    (basicDecks (length (c ^. #playerDefs)) `Map.union` makeDecks (c ^. #kingdomCards))
    []

runGames :: Int -> DominionConfig -> StdGen -> [Doc ann]
runGames 0 _ _ = []
runGames n conf g = do
  let (g1, g2) = split g
  runEval $ do
    x <- rpar $ pretty $ runGame conf g1
    _ <- rseq x
    return $ x : runGames (n - 1) conf g2

runGame :: DominionConfig -> StdGen -> DominionGame
runGame conf g =
    DominionGame
      (playerDefs conf)
      (kingdomCards conf)
      g
      turns
      (Just results)
  where
    (turns, endGameState) = runState runTurns $ configToGame conf g
    results = evalState tallyPoints endGameState

runTurns :: DominionState [DominionTurn]
runTurns = do
  start
  players <- turnOrder
  runDominionTurns players

runDominionTurns :: [PlayerNumber] -> DominionState [DominionTurn]
runDominionTurns [] = pure []
runDominionTurns xs = do
  fin <- finished
  if fin
    then pure []
    else do
      trns <- runPlayerTurns xs
      let dt = DominionTurn trns
      dts <- runDominionTurns xs
      pure $ dt : dts

runPlayerTurns :: [PlayerNumber] -> DominionState [DominionPlayerTurn]
runPlayerTurns [] = pure []
runPlayerTurns (x:xs) = do
  fin <- finished
  if fin
    then pure []
    else do
      turn <- runPlayerTurn x
      following <- runPlayerTurns xs
      pure $ turn : following

runPlayerTurn :: PlayerNumber -> DominionState DominionPlayerTurn
runPlayerTurn p = do
  actns <- evaluateHand p
  aig <- mkDominionAIGame p
  thePlayer <- findPlayer p
  let bys = thePlayer ^. #strategy . #buyStrategy $ aig
  executeBuys bys aig
  resetTurn p
  dealt <- deal 5 p
  pure $ DominionPlayerTurn
          p
          (thePlayer ^. #turns)
          bys
          actns
          (DominionDraw dealt)

instance Game DominionBoard where
  start       = do
    to <- turnOrder
    forM_ to $ \p -> deal 5 p

  finished    = do
    decks' <- use #decks
    emptyDecks <- numEmptyDecks
    pure $ (decks' Map.! provinceCard == 0) || emptyDecks >= 3

  tallyPoints = do
      turnOrder' <- turnOrder
      points <- mapM tallyPlayerPoints turnOrder'
      names <- mapM getPlayerName turnOrder'
      pure $ zip names points
    where
      tallyPlayerPoints p = do
        (field @"players" . ix (unPlayerNumber p) . #victory) .= 0
        thePlayer <- findPlayer p
        (field @"players" . ix (unPlayerNumber p) . #hand) .= ((thePlayer ^. #deck) ++ (thePlayer ^. #discard) ++ (thePlayer ^. #hand) ++ (thePlayer ^. #played))
        player' <- findPlayer p
        mapM_ (victoryPts p) (player' ^. #hand)
        finalPlayer <- findPlayer p
        pure $ finalPlayer ^. #victory
      victoryPts :: PlayerNumber -> Card -> DominionState Int
      victoryPts p (Card _ _ _ _ s) = s p
      getPlayerName :: PlayerNumber -> DominionState Text.Text
      getPlayerName p = do
        thePlayer <- findPlayer p
        return $ thePlayer ^. #playerName

  turnOrder  = do
    players' <- use #players
    return $ PlayerNumber <$> [0 .. (length players' - 1)]
