module DeckBuilding.Dominion
    ( runGames
    , runGame
    , newPlayer
    , doTurn
    , basicDecks
    , resetTurn
    , evaluateHand
    ) where

import qualified Data.Map as Map
import Data.List (delete, find, sortBy, group, sort, groupBy, intersect)
import Data.Foldable (foldrM)
import System.Random (randoms, newStdGen, mkStdGen)
import Control.Lens
import Control.Monad.State

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Cards
import DeckBuilding.Dominion.Utils
import DeckBuilding.Dominion.Strategies.Basic

-- Dominion

-- Core Engine

newPlayer :: String -> Strategy -> Player
newPlayer n s = Player n [] ((( (take 7) . repeat ) copperCard) ++ (( (take 3) . repeat) estateCard)) [] [] 1 1 0 0 s

evaluateHand :: Player -> State Game Player
evaluateHand p = foldrM (\c acc -> (c ^. action) c acc) p (p ^. hand)

tallyAllPoints :: Player -> State Game Player
tallyAllPoints p = evaluateHand $ Player (p ^. playerName) [] [] ((p ^. deck) ++ (p ^. discard) ++ (p ^. hand) ++ (p ^. played)) [] 1 1 0 0 (p ^. strategy)

sortByPoints :: State Game [Player]
sortByPoints = do
  gs <- get
  return $ reverse $ sortBy (\p1 p2 -> compare (p1 ^. victory) (p2 ^. victory)) (gs ^. players)

gameResult' :: [Player] -> Result
gameResult' players = result ((length . head) grouped) players
  where grouped = groupBy (\p1 p2 -> (p1 ^. victory) == (p2 ^. victory)) players
        result 1 l = Left $ _playerName $ head l
        result n _ = Right n

gameResult :: State Game Result
gameResult = do
  players <- sortByPoints
  return $ gameResult' players

basicDecks :: Int -> Map.Map Card Int
basicDecks numPlayers
    | numPlayers == 2 = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 8), (duchyCard, 8), (provinceCard, 8) ]
    | otherwise       = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 12), (duchyCard, 12), (provinceCard, 12) ]

resetTurn :: Player -> State Game Player
resetTurn p = updatePlayer $ Player (p ^. playerName) (p ^. deck) (p ^. discard ++ p ^. played) (p ^. hand) [] 1 1 0 0 (p ^. strategy)

doTurn :: Player -> State Game Bool
doTurn p = do
  p' <- evaluateHand p
  p'' <- (p' ^. strategy . buyStrategy) p'
  p''' <- deal 5 p''
  _ <- resetTurn p'''
  isGameOver

doTurns :: [Player] -> State Game Bool
doTurns [] = return False
doTurns (x:xs) = do
  done <- doTurn x
  if done
    then return True
    else doTurns xs

isGameOver :: State Game Bool
isGameOver = do
  gs <- get
  emptyDecks <- numEmptyDecks
  return $ ((gs ^. decks) Map.! provinceCard == 0) || emptyDecks >= 3

runGame' :: State Game Result
runGame' = do
  gs <- get
  done <- doTurns (gs ^. players)
  if done
    then do
      mapM tallyAllPoints (gs ^. players)
      sortByPoints
      gameResult
    else runGame'

runGame :: [Player] -> IO (Result)
runGame players = do
  g <- newStdGen
  let result = fst $ runState runGame' $ Game players (basicDecks (length players)) g
  return $ result

runGames :: Int -> [Player] -> IO ([(Result, Int)])
runGames num players = do
  g <- newStdGen
  let seeds = take num $ randoms g
  let gens = map mkStdGen seeds
  let gses = map (Game players (basicDecks (length players))) gens
  let results = map (runState runGame') gses
  return $ map (\l -> (head l, length l)) $ group $ sort $ map fst results
