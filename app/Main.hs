module Main where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion
import DeckBuilding.Dominion.Utils

main :: IO ([(Result, Int)])
main = runGames 1000 [newPlayer "Player 1", newPlayer "Player 2"]
