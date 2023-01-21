{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
      curseCard,
      moatCard, victoryCards,
      treasuryCard
    )
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
      DominionAction (TreasuryDuration, OutpostDuration),
      DominionDraw(DominionDraw),
      DominionBoard(DominionBoard) )
import DeckBuilding.Dominion.Utils
    ( deal, numEmptyDecks, findPlayer, discardCard, mkDominionAIGame, executeBuys, cardPlayed, removeFromCards )
import System.Random (StdGen, RandomGen(split))
import System.Random.Shuffle (shuffle')
import Control.Monad.List ( forM_ )
import Control.Parallel.Strategies ( rseq, rpar, runEval )
import Prettyprinter ( Doc, pretty )
import DeckBuilding.Dominion.Pretty ()
import Data.Maybe (catMaybes)
import Data.List (intersect)
import Control.Conditional (when)
import GHC.RTS.Flags (ProfFlags(descrSelector))

-- Dominion

-- Core Engine

-- | Creates a new player with a name and strategy and the default starter deck.
newPlayer :: Text.Text -> Strategy -> DominionPlayer
newPlayer n = DominionPlayer n [] (replicate 7 copperCard ++ replicate 3 estateCard) [] [] 1 1 0 0 1 [] [] [] 0 0 False

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
evaluateCard c pnum (DominionPlayer _ _ _ _ _ 0 _ _ _ _ _ _ _ _ _ _ _) = do
  discardCard c pnum
  pure Nothing
evaluateCard c pnum _ = evaluateCard' c pnum

evaluateCard' :: Card -> PlayerNumber -> DominionState (Maybe DominionAction)
evaluateCard' c pnum = do
  mda <- (c ^. #action) pnum
  cardPlayed c pnum
  return mda

-- | Given a set of potential kingdom cards, pick a random ten to play with.
randomKingdomDecks :: [Card] -> StdGen -> [Card]
randomKingdomDecks cs g = take 10 $ shuffle' cs (length cs) g

-- | Turns a list of cards into a Map of type (Card, Number in deck)
makeDecks :: [Card] -> Map.Map Card Int
makeDecks cs = Map.fromList $ (, 10) <$> cs

-- | Basic decks that are in all games, numbers based on the total players.
basicDecks :: Int -> Map.Map Card Int
basicDecks numPlayers
    | numPlayers == 2 = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 8), (duchyCard, 8), (provinceCard, 8), (curseCard, 10) ]
    | otherwise       = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 12), (duchyCard, 12), (provinceCard, 12), (curseCard, 10 * (numPlayers - 1)) ]

-- | Move played cards to discard pile, reset actions, buys, money, victory.
resetTurn :: PlayerNumber -> DominionState [DominionAction]
resetTurn p = do
  thePlayer <- findPlayer p
  decs <- treasuryDecision p
  playerReset p
  -- If they played an Outpost, run a small bonus turn
  bonusActions <- if thePlayer ^. #outpost
    then do
      dealt <- deal 3 p
      durations <- mapM (\f -> f p) (thePlayer ^. #duration)
      #players . ix (unPlayerNumber p) . #duration .= []

      actns <- evaluateHand p

      aig <- mkDominionAIGame p
      let bys = thePlayer ^. #strategy . #buyStrategy $ aig
      #lastBuys %= Map.mapWithKey (zeroBuys p)
      executeBuys bys aig
      decs' <- treasuryDecision p
      playerReset p
      #players . ix (unPlayerNumber p) . #outpost .= False
      pure $ OutpostDuration (DominionDraw dealt) bys : (catMaybes durations ++ actns ++ decs')
    else pure []

  (#players . ix (unPlayerNumber p) . #turns) += 1
  pure $ decs ++ bonusActions

  where
    treasuryDecision :: PlayerNumber -> DominionState [DominionAction]
    treasuryDecision p = do
      thePlayer <- findPlayer p
      lbs <- use #lastBuys

      if treasuryCard `elem` thePlayer ^. #discard && null (victoryCards `intersect` (lbs Map.! p))
        then do
          aig <- mkDominionAIGame p
          if thePlayer ^. #strategy . #treasuryStrategy $ aig
            then do
              da <- moveTreasuryCard p
              theRest <- treasuryDecision p
              pure $ da : theRest
            else pure []
        else pure []
    moveTreasuryCard :: PlayerNumber -> DominionState DominionAction
    moveTreasuryCard p = do
      thePlayer <- findPlayer p
      #players . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) [treasuryCard]
      #players . ix (unPlayerNumber p) . #deck %= (treasuryCard:)
      pure TreasuryDuration
    playerReset :: PlayerNumber -> DominionState ()
    playerReset p = do
      thePlayer <- findPlayer p
      (#players . ix (unPlayerNumber p) . #discard) %= ( ((thePlayer ^. #hand) ++ (thePlayer ^. #played) ) ++)
      (#players . ix (unPlayerNumber p) . #played) .= []
      (#players . ix (unPlayerNumber p) . #hand) .= []
      (#players . ix (unPlayerNumber p) . #actions) .= 1
      (#players . ix (unPlayerNumber p) . #buys) .= 1
      (#players . ix (unPlayerNumber p) . #money) .= 0
      (#players . ix (unPlayerNumber p) . #victory) .= 0

-- | Instantiate a new 'DominionBoard' based on a 'DominionConfig' and a random seed.
configToGame :: DominionConfig -> StdGen -> DominionBoard
configToGame c = DominionBoard
    (map (uncurry newPlayer) (c ^. #playerDefs))
    allDecks
    []
    zeroedDecks
    Map.empty
    [moatCard]
    curseCard
  where
    allDecks = basicDecks (length (c ^. #playerDefs)) `Map.union` makeDecks (c ^. #kingdomCards)
    zeroedDecks = fmap (const 0) allDecks

-- | Run n games of Dominion based on a config and a seed.
-- Outputs a prettyprinted document with the results.
--
-- Try to parallelize the running of the games as much as possible.
runGames :: Int -> DominionConfig -> StdGen -> [Doc ann]
runGames 0 _ _ = []
runGames n conf g = do
  let (g1, g2) = split g
  runEval $ do
    x <- rpar $ pretty $ runGame conf g1
    _ <- rseq x
    return $ x : runGames (n - 1) conf g2

-- | Run a single game of Dominion based on a 'DominionConfig'
-- and a random seed.
runGame :: DominionConfig -> StdGen -> DominionGame
runGame conf g =
    DominionGame
      (playerDefs conf)
      (kingdomCards conf)
      g
      turns
      results
  where
    (turns, endGameState) = runState runTurns $ configToGame conf g
    results = evalState tallyPoints endGameState

-- | Start the game, determine player order, and run the turns.
runTurns :: DominionState [DominionTurn]
runTurns = do
  start
  players <- turnOrder
  runDominionTurns players

-- | Given the turn order, generate the 'DominionTurn's, which
-- each contain a turn per player.
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

-- | Given the turn order, generate the 'DominionPlayerTurn's
-- within a 'DominionTurn'.
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

-- | Run the 'DominionPlayerTurn' for a specified player.
runPlayerTurn :: PlayerNumber -> DominionState DominionPlayerTurn
runPlayerTurn p = do
  thePlayer <- findPlayer p
  -- Duration cards register their second turn's action as a function,
  -- which we run at the beginning of the next turn (and then empty
  -- the list).
  durations <- mapM (\f -> f p) (thePlayer ^. #duration)
  #players . ix (unPlayerNumber p) . #duration .= []

  actns <- evaluateHand p

  aig <- mkDominionAIGame p
  let bys = thePlayer ^. #strategy . #buyStrategy $ aig
  #lastBuys %= Map.mapWithKey (zeroBuys p)
  executeBuys bys aig

  das <- resetTurn p
  dealt <- deal 5 p

  pure $ DominionPlayerTurn
          p
          (thePlayer ^. #turns)
          bys
          (catMaybes durations ++ actns ++ das)
          (DominionDraw dealt)

zeroBuys :: Eq a1 => a1 -> a1 -> [a2] -> [a2]
zeroBuys p p1 xs = if p == p1 then [] else xs

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
        (#players . ix (unPlayerNumber p) . #victory) .= 0
        thePlayer <- findPlayer p
        (#players . ix (unPlayerNumber p) . #hand) .= ((thePlayer ^. #deck) ++ (thePlayer ^. #discard) ++ (thePlayer ^. #hand) ++ (thePlayer ^. #played) ++ (thePlayer ^. #island))
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
