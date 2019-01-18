{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion
import DeckBuilding.Dominion.Utils
import DeckBuilding.Dominion.Strategies.Basic
import DeckBuilding.Dominion.Cards

import           System.Random
import           Control.Lens
import qualified Data.DList                             as DL
import Control.Monad.IO.Class (liftIO)

import System.Console.CmdArgs

data DeckBuilder = DeckBuilder
  { times :: Int
  }
  deriving (Data, Typeable, Show, Eq)

deckBuilder = DeckBuilder
  { times = def &= help "Number of games to run."
  } &=
  verbosity &=
  help "stack run -- deckbuilder-exe 1" &=
  summary "DeckBuilder version 0.1.0.2, (C) Andrew Boardman" &=
  details [ "DeckBuilder runs a simulation of deck building games."
          , "You can create new strategies and run them against each other."
          , "See the README.md for instructions!"
          ]


genGens :: Int -> StdGen -> [StdGen]
genGens 0 _ = []
genGens n g = do
  let (g1, g2) = split g
  g1 : genGens (n - 1) g2

-- | Basic usage of the library, pick some kingdom cards and run a few
--  thousand games to test the strategies against each other.
main :: IO ()
main = do
  g <- newStdGen
  args <- cmdArgs deckBuilder

  let n = times args
      (g1, g2) = split g
      gens = genGens n g
      conf = DominionConfig
              [ ("Big Money", bigMoneyStrategy)
              , ("Big Smithy", bigSmithyStrategy)
              , ("Village/Smithy Engine 4", villageSmithyEngine4)
              ]
              (randomKingdomDecks kingdomCards2ndEdition g1)
              n
              gens
      (result, output) = runDominionGames conf

  mapM_ (putStrLn . show) $ DL.toList $ DL.concat output
  print $ show result
