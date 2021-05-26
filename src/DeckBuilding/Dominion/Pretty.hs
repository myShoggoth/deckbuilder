{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module DeckBuilding.Dominion.Pretty where

import Data.Text.Prettyprint.Doc
    ( (<+>),
      align,
      hsep,
      vsep,
      indent,
      Pretty(pretty),
      viaShow )
import DeckBuilding.Dominion.Types
    ( BanditDecision(BanditDecision),
      DominionDraw(..),
      DominionAction(Workshop, Copper, Silver, Gold, Curse, Estate,
                     Duchy, Province, Gardens, Artisan, Bandit, Chapel, Cellar,
                     Festival, Harbinger, Remodel, Laboratory, Library, Market,
                     Merchant, Mine, Moat, MoneyLender, Poacher, Sentry, Smithy,
                     ThroneRoom, Vassal, Village),
      DominionBuy(..),
      DominionPlayerTurn(DominionPlayerTurn),
      DominionTurn(..),
      Card(cardName),
      Strategy(strategyName),
      DominionGame(DominionGame) )
import DeckBuilding.Types ( PlayerNumber )
import qualified Data.Text as Text
import System.Random (StdGen)
import qualified Data.Map as Map

instance Pretty DominionGame where
    pretty (DominionGame pls kndms sed trns) =
        vsep [indent 2 $ "Dominion Game " <> pretty sed
             , indent 4 "Players", indent 6 $ align $ vsep $ map pretty pls
             , indent 4 "Kingdom", indent 6 $ align $ hsep $ map pretty kndms
             , indent 4 "Turns", indent 6 $ align $ vsep $ map pretty trns
             ]

instance Pretty StdGen where
    pretty g = pretty $ show g

instance Pretty Strategy where
    pretty s = pretty $ strategyName s

instance {-# OVERLAPPING #-} Pretty (Text.Text, Strategy) where
    pretty (name, strat) = pretty name <> " (" <> pretty (strategyName strat) <> ")"

instance Pretty Card where
    pretty c = pretty $ cardName c

instance Pretty DominionTurn where
    pretty (DominionTurn xs) = vsep $ map pretty xs

instance Pretty DominionPlayerTurn where
    pretty (DominionPlayerTurn p t b a (DominionDraw d)) =
        vsep [ "Player " <> viaShow p <> " turn " <> viaShow t
             , indent 2 "Buys", indent 4 $ align $ hsep $ map pretty b
             , indent 2 "Actions", indent 4 $ align $ vsep $ map pretty a
             , indent 2 "Draws", indent 4 $ align $ hsep $ map pretty d
             ]

instance Pretty DominionBuy where
    pretty (DominionBuy _ c) = pretty (cardName c)

instance Pretty DominionAction where
    pretty Copper = pretty ("Copper" :: Text.Text)
    pretty Silver = pretty ("Silver" :: Text.Text)
    pretty Gold = pretty ("Gold" :: Text.Text)
    pretty Curse = pretty ("Curse" :: Text.Text)
    pretty Estate = pretty ("Estate" :: Text.Text)
    pretty Duchy = pretty ("Duchy" :: Text.Text)
    pretty Province = pretty ("Province" :: Text.Text)
    pretty Gardens = pretty ("Gardens" :: Text.Text)
    pretty (Artisan gc ctd) = "Artisan gains " <> viaShow (cardName gc) <> ", " <> viaShow (cardName ctd) <> " put on deck"
    pretty (Bandit resps) = "Bandit" <+> align (vsep (map pretty $ Map.toList resps))
    pretty (Chapel xs) = "Chapel trashes " <+> hsep (map pretty xs)
    pretty (Cellar xs (DominionDraw dds)) = "Cellar discards" <+> hsep (map pretty xs) <+> "and draws" <+> hsep (map pretty dds)
    pretty Festival = pretty ("Festival" :: Text.Text)
    pretty (Harbinger (DominionDraw xs) _) = "Harbinger draws" <+> hsep (map pretty xs) <+> "and pulls" <> viaShow xs <> " from the discards and puts it on their deck"
    pretty (Remodel c1 c2) = "Remodels " <> viaShow c1 <> " into " <> viaShow c2
    pretty (Laboratory (DominionDraw xs)) = "Laboratory draws" <+> hsep (map pretty xs)
    pretty (Library xs ys) = "Library draws " <+> hsep (map pretty xs) <+> "discards" <+> hsep (map pretty ys)
    pretty (Market (DominionDraw xs)) = "Market draws" <+> hsep (map pretty xs)
    pretty (Merchant (DominionDraw xs)) = "Merchant draws" <+> hsep (map pretty xs)
    pretty (Mine c1 c2) = "Mine " <> viaShow c1 <> " into " <> viaShow c2
    pretty (Moat (DominionDraw xs)) = "Moat draws" <+> hsep (map pretty xs)
    pretty MoneyLender = pretty ("MoneyLender" :: Text.Text)
    pretty (Poacher (DominionDraw xs) ys) = "Poacher draws" <+> hsep (map pretty xs) <+> "discards" <+> hsep (map pretty ys)
    pretty (Sentry (DominionDraw ws) xs ys zs) = "Sentry draws" <+> hsep (map pretty ws) <+> "trashes" <+> hsep (map pretty xs) <+> "discards" <+> hsep (map pretty ys) <+> "keeps" <+> hsep (map pretty zs)
    pretty (Smithy (DominionDraw xs)) = "Smithy draws" <+> hsep (map pretty xs)
    pretty (ThroneRoom c da1 da2) = "Throne Rooms " <> viaShow c <> "for" <+> hsep (map pretty [da1, da2])
    pretty (Vassal mda) = "Vassal " <> viaShow mda
    pretty (Village (DominionDraw xs)) = "Village draws" <+> hsep (map pretty xs)
    pretty (Workshop c) = "Workshop gains " <> viaShow c
    pretty _ = pretty ("Unknown action" :: Text.Text)
{-    
      Bureaucrat (Map.Map PlayerNumber (Maybe Card)) |
      CouncilRoom DominionDraw (Map.Map PlayerNumber (Maybe Card)) |
      Militia (Map.Map PlayerNumber (Either Card [Card])) |
      Witch [Card] (Map.Map PlayerNumber (Either Card (Maybe Card))) |
-}

instance {-# OVERLAPS #-} Pretty (PlayerNumber, Either Card BanditDecision) where
    pretty (n, Left c) = "Player #" <> viaShow n <> " showed " <> viaShow (cardName c)
    pretty (n, Right (BanditDecision (Just c) dcs)) = "Player #" <> viaShow n <> " trashed " <> viaShow (cardName c) <> " and discarded: " <+> viaShow dcs
    pretty (n, Right (BanditDecision Nothing dcs)) = "Player #" <> viaShow n <> " discarded: " <+> viaShow dcs

instance Pretty DominionDraw where
    pretty (DominionDraw xs) = align $ hsep $ map pretty xs
