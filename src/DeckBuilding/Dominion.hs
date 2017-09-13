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
import System.Random (randoms, newStdGen, mkStdGen)
import Control.Lens

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Cards
import DeckBuilding.Dominion.Utils
import DeckBuilding.Dominion.Strategies.Basic

-- Dominion

-- Core Engine

newPlayer :: String -> Player
newPlayer n = Player n [] ((( (take 7) . repeat ) copperCard) ++ (( (take 3) . repeat) estateCard)) [] [] 1 1 0 0

evaluateHand :: Player -> GameState -> GameState
evaluateHand p gs = changeTurn p'' gs'
  where gs'       = foldr (\c -> (c ^. action) c p) gs (p' ^. hand)
        Just p'   = find (== p) (gs ^. players)
        Just p''  = find (== p) (gs' ^. players)

tallyAllPoints :: Player -> GameState -> GameState
tallyAllPoints p gs = evaluateHand player $ changeTurn player gs
  where player  = Player (p' ^. playerName) [] [] ((p ^. deck) ++ (p' ^. discard) ++ (p' ^. hand) ++ (p' ^. played)) [] 1 1 0 0
        Just p' = find (== p) (gs ^. players)

sortByPoints :: GameState -> GameState
sortByPoints gs = GameState (reverse (sortBy (\p1 p2 -> compare (p1 ^. victory) (p2 ^. victory)) (gs ^. players))) (gs ^. decks) (gs ^. random)

gameResult' :: [Player] -> Result
gameResult' players = result ((length . head) grouped) players
  where grouped = groupBy (\p1 p2 -> (p1 ^. victory) == (p2 ^. victory)) players
        result 1 l = Left $ _playerName $ head l
        result n _ = Right n

gameResult :: GameState -> Result
gameResult gs = gameResult' (sorted ^. players)
  where sorted = sortByPoints (foldr tallyAllPoints gs (gs ^. players))

basicDecks :: Int -> Map.Map Card Int
basicDecks numPlayers
    | numPlayers == 2 = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 8), (duchyCard, 8), (provinceCard, 8) ]
    | otherwise       = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 12), (duchyCard, 12), (provinceCard, 12) ]

resetTurn :: Player -> GameState -> GameState
resetTurn p gs = changeTurn player gs
  where Just p' = find (== p) (gs ^. players)
        player  = Player (p' ^. playerName) (p' ^. deck) (p' ^. discard ++ p' ^. played) (p' ^. hand) [] 1 1 0 0

doTurn :: Bool -> Player -> GameState -> GameState
doTurn True  p gs = gs
doTurn False p gs = ( (resetTurn p) . (deal 5 p) . (bigMoneyBuy p) . (evaluateHand p) ) gs

isGameOver :: GameState -> Bool
isGameOver gs = ((gs ^. decks) Map.! provinceCard == 0) || numEmptyDecks >= 3
  where numEmptyDecks = length $ Map.filter (== 0) (gs ^. decks)

runGame' :: Bool -> GameState -> GameState
runGame' True  gs = sortByPoints (foldr tallyAllPoints gs (gs ^. players))
runGame' False gs = runGame' (isGameOver gs') gs'
  where gs' = foldr (doTurn (isGameOver gs)) gs (gs ^. players)

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
