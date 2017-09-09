module DeckBuilding.Dominion
    ( changeTurn
    , deal
    , runGames
    , runGame
    , newPlayer
    , doTurn
    , basicDecks
    , resetTurn
    , evaluateHand
    ) where

import qualified Data.Map as Map
import Data.List (delete, find, sortBy, group, sort, groupBy, intersect)
import System.Random

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Cards
import DeckBuilding.Dominion.Utils
import DeckBuilding.Dominion.Strategies.Basic

-- Dominion

-- Core Engine

newPlayer :: String -> Player
newPlayer n = Player n [] ((( (take 7) . repeat ) copperCard) ++ (( (take 3) . repeat) estateCard)) [] [] 1 1 0 0

evaluateHand :: Player -> GameState -> GameState
evaluateHand p gs = changeTurn p' newGs
  where newGs     = foldr (\c -> (_action c) c p) gs (_hand p'')
        Just p'   = find (== p) (_players newGs)
        Just p''  = find (== p) (_players gs)

tallyAllPoints :: Player -> GameState -> GameState
tallyAllPoints p gs = evaluateHand player $ changeTurn player gs
  where player  = Player (_playerName p') [] [] ((_deck p') ++ (_discard p') ++ (_hand p') ++ (_played p')) [] 1 1 0 0
        Just p' = find (== p) (_players gs)

sortByPoints :: GameState -> GameState
sortByPoints gs = GameState (reverse (sortBy (\p1 p2 -> compare (_victory p1) (_victory p2)) (_players gs))) (_decks gs) (_random gs)

gameResult' :: [Player] -> Result
gameResult' players = result ((length . head) grouped) players
  where grouped = groupBy (\p1 p2 -> (_victory p1) == (_victory p2)) players
        result 1 l = Left $ _playerName $ head l
        result n _ = Right n

gameResult :: GameState -> Result
gameResult gs = gameResult' (_players sorted)
  where sorted = sortByPoints (foldr tallyAllPoints gs (_players gs))

basicDecks :: Int -> Map.Map Card Int
basicDecks numPlayers
    | numPlayers == 2 = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 8), (duchyCard, 8), (provinceCard, 8) ]
    | otherwise       = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 12), (duchyCard, 12), (provinceCard, 12) ]

resetTurn :: Player -> GameState -> GameState
resetTurn p gs = changeTurn player gs
  where Just p' = find (== p) (_players gs)
        player  = Player (_playerName p') (_deck p') (_discard p' ++ _played p') (_hand p') [] 1 1 0 0

doTurn :: Bool -> Player -> GameState -> GameState
doTurn True  p gs = gs
doTurn False p gs = ( (resetTurn p) . (deal 5 p) . (bigMoneyBuy p) . (evaluateHand p) ) gs

isGameOver :: GameState -> Bool
isGameOver gs = ((_decks gs) Map.! provinceCard == 0) || numEmptyDecks >= 3
  where numEmptyDecks = length $ Map.filter (== 0) (_decks gs)

runGame' :: Bool -> GameState -> GameState
runGame' True  gs = sortByPoints (foldr tallyAllPoints gs (_players gs))
runGame' False gs = runGame' (isGameOver gs') gs'
  where gs' = foldr (doTurn (isGameOver gs)) gs (_players gs)

runGame :: [Player] -> IO (Result)
runGame players = do
  g <- newStdGen
  return $ gameResult $ runGame' False $ GameState players (basicDecks (length players)) g

runGames :: Int -> [Player] -> IO ([(Result, Int)])
runGames num players = do
  g <- newStdGen
  let seeds = take num $ randoms g
  let gens = map mkStdGen seeds
  let gses = map (GameState players (basicDecks (length players))) gens
  return $ map (\l -> (head l, length l)) $ group $ sort $ map (gameResult . (runGame' False)) gses
