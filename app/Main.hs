module Main where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion
import DeckBuilding.Dominion.Utils
import DeckBuilding.Dominion.Strategies.Basic

main :: IO ([(Result, Int)])
main = runGames 1000 [newPlayer "Player 1" bigMoneyStrategy, newPlayer "Player 2" bigSmithyStrategy]
