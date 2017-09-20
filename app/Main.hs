module Main where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion
import DeckBuilding.Dominion.Utils
import DeckBuilding.Dominion.Strategies.Basic

main :: IO ()
main = do
  result <- runGames 1000 [newPlayer "Player 1" bigMoneyStrategy, newPlayer "Player 2" bigSmithyStrategy]
  print $ show $ result
