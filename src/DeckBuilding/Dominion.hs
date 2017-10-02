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
    ( runGames
    , runGame
    , newPlayer
    , doTurn
    , basicDecks
    , resetTurn
    , evaluateHand
    , makeDecks
    , randomKingdomDecks
    ) where

import           Control.Arrow                          ((&&&))
import           Control.Lens
import           Control.Monad.State
import           Data.Foldable                          (foldrM)
import           Data.List                              (delete, find, group,
                                                         groupBy, intersect,
                                                         sort, sortBy)
import qualified Data.Map                               as Map
import           Data.Ord                               (comparing)
import           System.Random                          (StdGen, mkStdGen,
                                                         newStdGen, randoms)
import           System.Random.Shuffle                  (shuffle')

import           DeckBuilding.Dominion.Cards
import           DeckBuilding.Dominion.Strategies.Basic
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils

import           Debug.Trace


-- Dominion

-- Core Engine

-- | Creates a new player with a name and strategy and the default started deck.
newPlayer :: String -> Strategy -> Player
newPlayer n = Player n [] (replicate 7 copperCard ++ replicate 3 estateCard) [] [] 1 1 0 0 0

{- |
  Evaluates the cards in the deck. Since cards can cause more to be drawn,
  the default case is to run a card and then recursively call with the new
  hand for the player.

  If The player is out of actions we can only run Value cards (ones that don't
  require actions), and skip all cards that require actions.
-}
evaluateHand' :: Player -> [Card] -> State Game Player
evaluateHand' p []     = return p
evaluateHand' p@(Player _ _ _ _ _ 0 _ _ _ _ _) (x@(Card _ _ _ Value):xs)  = do
  p' <- (x ^. action) x p
  evaluateHand' p' xs
evaluateHand' p@(Player _ _ _ _ _ 0 _ _ _ _ _) (_:xs)  = evaluateHand' p xs
evaluateHand' p (x:xs) = do
  p' <- (x ^. action) x p
  evaluateHand' p' (p' ^. hand)

-- | Runs the cards in the deck by offloading the work to evaluateHand'
evaluateHand :: Player -> State Game Player
evaluateHand p = evaluateHand' p (p ^. hand)

-- | Runs all the cards in the player's deck to determine the total number of
--   victory points.
tallyAllPoints :: Player -> State Game Player
tallyAllPoints p = evaluateHand $ Player (p ^. playerName) [] [] ((p ^. deck) ++ (p ^. discard) ++ (p ^. hand) ++ (p ^. played)) [] 1 1 0 0 (p ^. turns) (p ^. strategy)

-- | Returns the list of players in total points order, highest first.
sortByPoints :: State Game [Player]
sortByPoints = do
  gs <- get
  return $ sort (gs ^. players)

gameResult' :: [Player] -> Result
gameResult' players = result ((length . head) grouped) players
  where grouped = groupBy (\p1 p2 -> (p1 ^. victory) == (p2 ^. victory) && (p1 ^. turns) == (p2 ^. turns)) players
        result 1 l = Left $ _playerName $ head l
        result n _ = Right n

-- | Returns the Result of the game.
gameResult :: State Game Result
gameResult = do
  players <- sortByPoints
  return $ gameResult' players

-- | Given a set of potential kingdom cards, pick a random ten to play with.
randomKingdomDecks :: [Card] -> StdGen -> [Card]
randomKingdomDecks cs g = take 10 $ shuffle' cs (length cs) g

-- | Turns a list of cards into a Map of type (Card, Number in deck)
makeDecks :: [Card] -> Map.Map Card Int
makeDecks cs = Map.fromList $ map (\c -> (c, 10)) cs

-- | Basic decks that are in all games, numbers based on the total players.
basicDecks :: Int -> Map.Map Card Int
basicDecks numPlayers
    | numPlayers == 2 = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 8), (duchyCard, 8), (provinceCard, 8) ]
    | otherwise       = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 12), (duchyCard, 12), (provinceCard, 12) ]

-- | Move played cards to discard pile, reset actions, buys, money, victory.
resetTurn :: Player -> State Game Player
resetTurn p = updatePlayer $ Player (p ^. playerName) (p ^. deck) (p ^. discard ++ p ^. played) (p ^. hand) [] 1 1 0 0 (p ^. turns + 1) (p ^. strategy)

{-|
  The core of the engine, on each turn we:

  1. Call the strategy to order the hand to determine which cards to run first.
  2. Evaluate the cards in the hand.
  3. Call the strategy to buy cards.
  4. Deal a new hand.
  5. Reset the player for the next turn.
  6. Determine if the game is now over.
-}
doTurn :: Player -> State Game Bool
doTurn p = do
  p' <- (p ^. strategy . orderHand) p
  p'' <- evaluateHand p'
  p''' <- (p'' ^. strategy . buyStrategy) p''
  p'''' <- deal 5 p'''
  _ <- resetTurn p''''
  isGameOver

-- | Run turns for each player until all players have gone or the game ends.
doTurns :: [Player] -> State Game Bool
doTurns [] = return False
doTurns (x:xs) = do
  done <- doTurn x
  if done
    then return True
    else doTurns xs

-- | Return if the game is over (all provinces are gone or there are three
--  empty decks).
isGameOver :: State Game Bool
isGameOver = do
  gs <- get
  emptyDecks <- numEmptyDecks
  return $ ((gs ^. decks) Map.! provinceCard == 0) || emptyDecks >= 3

{-|
  Run the game, do turns for each player until the game is over, then figure
  out who won.
-}
runGame' :: State Game Result
runGame' = do
  gs <- get
  done <- doTurns (gs ^. players)
  if done
    then do
      mapM_ tallyAllPoints (gs ^. players)
      sortByPoints
      gameResult
    else runGame'

-- | Run a single game with a set of players and kingdom cards.
runGame :: [Player] -> [Card] -> IO Result
runGame players kingdom = do
  g <- newStdGen
  let result = evalState runGame' $ Game players (basicDecks (length players) `Map.union` makeDecks kingdom) [] g
  return result

-- | Run n games with a set of players and kingdom cards.
runGames :: Int -> [Player] -> [Card] -> IO [(Result, Int)]
runGames num players kingdom = do
  g <- newStdGen
  let seeds = take num $ randoms g
  let gens = map mkStdGen seeds
  let gses = map (Game players (basicDecks (length players) `Map.union` makeDecks kingdom) []) gens
  let results = map (runState runGame') gses
  return $ map (head &&& length) $ group $ sort $ map fst results
