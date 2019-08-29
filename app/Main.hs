{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

import           DeckBuilding.Dominion
import           DeckBuilding.Dominion.Cards
import           DeckBuilding.Dominion.DominionTree
import           DeckBuilding.Dominion.Strategies.Basic
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Types
import           DeckBuilding

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import qualified Data.DList                             as DL
import qualified Data.List                              as List
import qualified Data.Text.IO                           as Text
import           Data.Text.Lazy                         (toStrict, pack)
import           Data.Text                              (Text(..))
import qualified Data.Text                              as Text (concat)
import           System.Random
import           Data.Generics.Product                  hiding (list)
import           Control.Lens
import           Control.Arrow                          ((&&&))
import           Control.Monad.RWS
import           Control.Monad.Extra

import           System.Console.CmdArgs

data DeckBuilder = DeckBuilder
  { times :: Int
  }
  deriving (Data, Typeable, Show, Eq)

deckBuilder :: DeckBuilder
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

-- | Run n games with a set of players and kingdom cards.
runDominionGames :: LayoutOptions -> DominionConfig -> IO [(Result, Int)]
runDominionGames layoutOptions c = do
  results :: [Result] <- forM gses $ \g -> do
    let (result, output) = evalRWS (runGame False :: DominionState Result) c g
        dt = buildDominionTree (DL.toList output, result)
    loud <- isLoud
    ifM isLoud ( Text.putStrLn $ (renderStrict . layoutPretty layoutOptions . pretty) dt ) (pure ())
    pure result
  pure $ map (head &&& length) $ List.group $ List.sort results
  where gses = map (configToGame c) (seeds c)

-- | Basic usage of the library, pick some kingdom cards and run a few
--  thousand games to test the strategies against each other.
main :: IO ()
main = do
  g <- newStdGen
  args' <- cmdArgs deckBuilder

  let n = times args'
      (g1, g2) = split g
      gens = genGens n g2
      conf = DominionConfig
              [ ("Big Money", bigMoneyStrategy)
              , ("Big Smithy", bigSmithyStrategy)
              , ("Village/Smithy Engine 4", villageSmithyEngine4)
              ]
--              firstGameKingdomCards
              (randomKingdomDecks kingdomCards2ndEdition g1)
              n
              gens
  result <- runDominionGames layoutOptions conf
  (Text.putStrLn . renderStrict . layoutPretty layoutOptions . pretty) result
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 80 1 }

instance Pretty DominionTree where
    pretty (DominionTree turns results) = vsep [ "Turns", align $ pretty turns, "Results", align $ pretty results ]

instance Pretty GameTurn where
    pretty (GameTurn n turns) = align $ vsep [ "Turn" <+> pretty n, pretty turns ]

instance Pretty PlayerTurn where
    pretty (PlayerTurn pname played bought) = align $ vsep [ "Player" <+> pretty pname, pretty played, pretty bought ]

instance Pretty CardPlay where
    pretty (Standard c) = sep [ "Played", pretty $ cardName c ]
    pretty (PlayThroneRoom c) = sep [ "ThroneRoomed a ", pretty $ cardName c]
    pretty (PlayRemodel c c') = sep [ "Remodelled a", pretty $ cardName c, "into a", pretty $ cardName c' ]
    pretty (PlayCellar c) = sep [ "Cellared", list $ pretty . cardName <$> c ]

instance Pretty BoughtCard where
    pretty (BoughtCard c) = sep [ "Bought", pretty $ cardName c ]

instance Pretty Result where
    pretty (Left s)   = pretty $ s <> " won!"
    pretty (Right n)  = pretty $ "Tie between " <> show n <> " players."

instance Pretty DominionMove where
    pretty (Turn n p)     = pretty $ "Turn " <> show n <> " for player " <> show (playerName p) <> ": "
                              <> "Hand = " <> show (hand p)
    pretty (Play c)       = pretty $ "Playing " <> show c
    pretty (Deal n xs)    = pretty $ "Dealing " <> show n <> " card(s): " <> show xs
    pretty (Discard xs)   = pretty $ "Discarding: " <> show xs
    pretty (ThroneRoom c) = pretty $ "Using Thrown Room on " <> show c
    pretty (Remodel c c') = pretty $ "Remodelling " <> show c <> " into " <> show c'
    pretty (Buy c)        = pretty $ "Buying " <> show c
    pretty (Retrieve xs)  = pretty $ "Retrieving " <> show xs
    pretty (Trash xs)     = pretty $ "Trashing " <> show xs
    pretty (GameOver xs)  = pretty $ "Game Over!\n" <> "Results: " <> show xs
