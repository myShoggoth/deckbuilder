module Main where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion
import DeckBuilding.Dominion.Utils
import DeckBuilding.Dominion.Strategies.Basic
import DeckBuilding.Dominion.Cards

import System.Random
import Control.Lens

showStrategies :: [Player] -> IO ()
showStrategies [] = return ()
showStrategies (x:xs) = do
  print $ show (x ^. playerName) ++ " is using strategy: " ++ show (x ^. strategy . strategyName)
  showStrategies xs

-- | Basic usage of the library, pick some kingdom cards and run a few
--  thousand games to test the strategies against each other.
main :: IO ()
main = do
  g <- newStdGen
  let kingdom = randomKingdomDecks kingdomCards2ndEdition g
  print $ "Kingdom: " ++ show kingdom
  let players = [newPlayer "Big Money" bigMoneyStrategy, newPlayer "Big Smithy" bigSmithyStrategy, newPlayer "Village/Smithy Engine 4" villageSmithyEngine4]
  result <- runDominionGames 5000 players kingdom
  showStrategies players
  print $ show result
