module Main where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion
import DeckBuilding.Dominion.Utils
import DeckBuilding.Dominion.Strategies.Basic
import DeckBuilding.Dominion.Cards

import           System.Random
import           Control.Lens
import qualified Data.DList                             as DL

-- | Basic usage of the library, pick some kingdom cards and run a few
--  thousand games to test the strategies against each other.
main :: IO ()
main = do
  g <- newStdGen
  let (g1, g2) = split g
  let conf = DominionConfig
              [ ("Big Money", bigMoneyStrategy)
              , ("Big Smithy", bigSmithyStrategy)
              , ("Village/Smithy Engine 4", villageSmithyEngine4)
              ]
              (randomKingdomDecks kingdomCards2ndEdition g1)
              1
              [g2]
  let (result, output) = runDominionGames conf
  mapM_ (putStrLn . show) $ DL.toList $ DL.concat output
  print $ show result
