{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

import DeckBuilding.Dominion ( configToGame, randomKingdomDecks )
import DeckBuilding.Dominion.Cards ( kingdomCards2ndEdition )
import DeckBuilding.Dominion.DominionTree ( buildDominionTree )
import DeckBuilding.Dominion.Strategies.Basic
    ( bigMoneyStrategy, bigSmithyStrategy, villageSmithyEngine4 )
import DeckBuilding.Dominion.Types
    ( DominionConfig(DominionConfig, seeds),
      DominionMove(..),
      DominionPlayer(playerName, hand),
      Card(cardName),
      DominionState,
      PlayerTurn(PlayerTurn),
      CardPlay(..),
      BoughtCard(..),
      DominionTree(DominionTree),
      GameTurn(GameTurn),
      BanditDecision(discarded, trashed))
import DeckBuilding.Types ( Result, PlayerNumber(unPlayerNumber) )
import DeckBuilding ( runGame )
import Data.Text.Prettyprint.Doc
    ( (<+>),
      align,
      layoutPretty,
      list,
      sep,
      vsep,
      LayoutOptions(..),
      PageWidth(AvailablePerLine),
      Pretty(pretty) )
import Data.Text.Prettyprint.Doc.Render.Text ( renderStrict )
import qualified Data.DList                             as DL
import qualified Data.List                              as List
import qualified Data.Text.IO                           as Text
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Map (mapWithKey)
import System.Random ( newStdGen, RandomGen(split), StdGen )
import Control.Arrow ((&&&))
import Control.Monad.RWS ( evalRWS )
import Control.Monad.Extra ( forM, ifM )
import System.Console.CmdArgs
    ( Data,
      Typeable,
      (&=),
      cmdArgs,
      details,
      help,
      summary,
      verbosity,
      isLoud,
      Default(def) )

data DeckBuilder = DeckBuilder
  { times :: Int
  }
  deriving (Data, Typeable, Show, Eq)

deckBuilder :: DeckBuilder
deckBuilder = DeckBuilder
  { times = def &= help "Number of games to run."
  } &=
  verbosity &=
  help "stack run -- deckbuilder-exe --times 1" &=
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
  results' :: [Result] <- forM gses $ \g -> do
    let (res, output) = evalRWS (runGame False :: DominionState Result) c g
        dt = buildDominionTree (DL.toList output, res)
    ifM isLoud ( Text.putStrLn $ (renderStrict . layoutPretty layoutOptions . pretty) dt ) (pure ())
    ifM isLoud (putStrLn $ show res <> " ") (pure ())
    pure res
  pure $ map (head &&& length) $ List.group $ List.sort results'
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
  res <- runDominionGames layoutOptions conf
  (Text.putStrLn . renderStrict . layoutPretty layoutOptions . pretty) res
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 80 1 }

instance Pretty DominionTree where
    pretty (DominionTree turns' results') = vsep [ "Turns", align $ pretty turns', "Results", align $ pretty results' ]

instance Pretty GameTurn where
    pretty (GameTurn n turns') = align $ vsep [ "Turn" <+> pretty n, pretty turns' ]

instance Pretty PlayerTurn where
    pretty (PlayerTurn pnum played' bought) = align $ vsep [ "Player" <+> pretty (unPlayerNumber pnum), pretty played', pretty bought ]

instance Pretty BoughtCard where
    pretty (BoughtCard c) = sep [ "Bought", pretty $ cardName c ]

instance Pretty Result where
    pretty (Left (s, _))   = pretty $ s
    pretty (Right n)  = pretty $ show n <> " players tied"

instance Pretty DominionMove where
    pretty (Turn _ n p) = pretty $ "Turn " <> show n <> " for player " <> show (playerName p) <> ": "
                                <> "Hand = " <> show (hand p)
    pretty (Deal _ n xs) = pretty $ "Dealing " <> show n <> " card(s): " <> show xs
    pretty (Buy _ c) = pretty $ "Buying " <> show c
    pretty (Cellar _ xs) = pretty $ "Cellaring card(s): " <> show xs
    pretty (PlayBasic _ c _ _ _ _) = pretty $ "Playing " <> show c
    pretty (PlayValue _ c _) = pretty $ "Playing " <> show c
    pretty (ThroneRoom _ c m1 m2) = pretty $ "Throne Rooming " <> show c <> "\n"
                                                              <> "  " <> show m1 <> "\n"
                                                              <> "  " <> show m2
    pretty (Remodel _ c1 c2) = pretty $ "Remodeling " <> show c1 <> " into " <> show c2
    pretty (Vassal _ c) = pretty $ "Vassals, playing " <> show c
    pretty (Militia _ responses) = pretty $ "Militia!\n"
                                         <> responses2String responses
      where
        responses2String :: Map.Map PlayerNumber (Either Card [Card]) -> String
        responses2String responses = unlines $ map response2String $ Map.toList responses
        response2String :: (PlayerNumber, Either Card [Card]) -> String
        response2String (p, ec) = "  Player " <> show p <>
                                  (case ec of
                                    Left c -> " showed " <> show c
                                    Right mc -> " discarded " <> show mc)
                                  <> "\n"
    pretty (MoneyLender _ _) = pretty $ ("Moneylendered a copperCard" :: Text.Text)
    pretty (Poacher _ xs) = pretty $ "Poacher, discarded " <> show xs
    pretty (Chapel _ xs) = pretty $ "Chapelled " <> show xs
    pretty (Harbinger _ mc) = pretty $ "Harbingered " <> show mc
    pretty (Bureaucrat _ shown) = pretty $ "Bureaucrat:\n"
                                        <> shown2String shown
      where
        shown2String :: Map.Map PlayerNumber (Maybe Card) -> String
        shown2String shown = unlines $ map show2String $ Map.toList shown
        show2String :: (PlayerNumber, Maybe Card) -> String
        show2String (p, mc) = " Player " <> show p <> " shows " <> show mc <> "\n"
    pretty (Bandit _ responses) = pretty $ "Bandit:\n"
                                        <> responses2String responses
      where
        responses2String :: Map.Map PlayerNumber (Either Card BanditDecision) -> String
        responses2String responses = unlines $ map response2String $ Map.toList responses
        response2String :: (PlayerNumber, Either Card BanditDecision) -> String
        response2String (p, bd) = "  Player " <> show p <>
                                  (case bd of
                                    Left c -> " showed " <> show c
                                    Right decision -> " discarded " <> show (discarded decision) <> " and trashed " <> show (trashed decision))
                                  <> "\n"
    pretty (CouncilRoom _ xs draws) = pretty $ "CouncilRoom for " <> show xs <> "\n"
                                            <> draws2String draws
      where
        draws2String :: Map.Map PlayerNumber (Maybe Card) -> String
        draws2String draws = unlines $ map draw2String $ Map.toList draws
        draw2String :: (PlayerNumber, Maybe Card) -> String
        draw2String (p, mc) = " Player " <> show p <> " draws " <> show mc <> "\n"

    pretty (Witch _ xs responses) = pretty $ "Witch drew " <> show xs <> "\n"
                                          <> responses2String responses
      where
        responses2String :: Map.Map PlayerNumber (Either Card (Maybe Card)) -> String
        responses2String responses = unlines $ map response2String $ Map.toList responses
        response2String :: (PlayerNumber, Either Card (Maybe Card)) -> String
        response2String (p, ec) = "  Player " <> show p <>
                                  (case ec of
                                    Left c -> " showed " <> show c
                                    Right mc -> " gained " <> show mc)

    pretty (Mine _ c1 c2) = pretty $ "Mined " <> show c1 <> " into " <> show c2
    pretty (Library _ keeps discards) = pretty $ "Libraried and kept " <> show keeps <> ", discarded " <> show discards
    pretty (Sentry _ trashed discarded keeps) = pretty $ "Sentried to trash " <> show trashed <> ", discarded " <> show discarded <> ", and kept " <> show keeps
    pretty (Artisan _ c1 c2) = pretty $ "Artisan to gain " <> show c1 <> " and put " <> show c2 <> " back on the deck"
    pretty (Workshop _ c) = pretty $ "Workshopped a " <> show c
    pretty (GameOver xs) = pretty $ "Game Over!\n" <> "Results: " <> show xs
