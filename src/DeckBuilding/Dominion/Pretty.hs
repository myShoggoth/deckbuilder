{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module DeckBuilding.Dominion.Pretty where

import Prettyprinter
    ( (<+>),
      align,
      hsep,
      vsep,
      indent,
      Pretty(pretty),
      viaShow, Doc )
import DeckBuilding.Dominion.Types
    ( BanditDecision(BanditDecision),
      DominionDraw(..),
      DominionAction(Workshop, Copper, Silver, Gold, Curse, Estate,
                     Duchy, Province, Gardens, Artisan, Bandit, Chapel, Cellar,
                     Festival, Harbinger, Remodel, Laboratory, Library, Market,
                     Merchant, Mine, Moat, MoneyLender, Poacher, Sentry, Smithy,
                     ThroneRoom, Vassal, Village, Militia, Harem, Duke, Bureaucrat, Conspirator, CouncilRoom, Courtyard, Ironworks, Lurker, ShantyTown, Witch),
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
    pretty (DominionGame pls kndms sed trns (Just res)) =
        vsep [indent 2 $ "Dominion Game " <> pretty sed
             , indent 4 "Players", indent 6 $ align $ vsep $ map pretty pls
             , indent 4 "Kingdom", indent 6 $ align $ hsep $ map pretty kndms
             , indent 4 "Turns", indent 6 $ align $ vsep $ map pretty trns
             , indent 4 "Results", indent 6 $ align $ vsep $ map pretty res
             ]
    pretty (DominionGame _ _ _ _ Nothing) = pretty ("Game ends without results, should never happen." :: Text.Text)

instance Pretty StdGen where
    pretty g = pretty $ show g

instance Pretty Strategy where
    pretty s = pretty $ strategyName s

instance {-# OVERLAPPING #-} Pretty (Text.Text, Strategy) where
    pretty (name, strat) = pretty name <> " (" <> pretty (strategyName strat) <> ")"

instance {-# OVERLAPPING #-} Pretty (Text.Text, Int) where
    pretty (name, points) = pretty name <> " scored " <> viaShow points <> " points."

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
    pretty Harem = pretty ("Harem" :: Text.Text)
    pretty Curse = pretty ("Curse" :: Text.Text)
    pretty Estate = pretty ("Estate" :: Text.Text)
    pretty Duchy = pretty ("Duchy" :: Text.Text)
    pretty Province = pretty ("Province" :: Text.Text)
    pretty Gardens = pretty ("Gardens" :: Text.Text)
    pretty Duke = pretty ("Duke" :: Text.Text)
    pretty (Artisan gc ctd) = "Artisan gains " <> viaShow (cardName gc) <> ", " <> viaShow (cardName ctd) <> " put on deck"
    pretty (Bandit resps) = "Bandit:" <+> align (vsep (map pretty $ Map.toList resps))
    pretty (Bureaucrat xs) = "Bureaucrat:" <+> align (vsep $ map bureaucratResponse $ Map.toList xs)
    pretty (Chapel xs) = "Chapel trashes" <+> hsep (map pretty xs)
    pretty (Cellar xs (DominionDraw dds)) = "Cellar discards" <+> hsep (map pretty xs) <+> "and draws" <+> hsep (map pretty dds)
    pretty (Conspirator (DominionDraw xs)) = "Conspirator draws" <+> hsep (map pretty xs)
    pretty (CouncilRoom (DominionDraw xs) others) = "Council Room draws" <+> hsep (map pretty xs) <+> "and causes others to draw:" <+> vsep (map (\(k,v) -> "Player draws " <> pretty v) $ Map.toList others)
    pretty (Courtyard (DominionDraw xs) h2d) = "Courtyard draws" <+> hsep (map pretty xs) <+> "and puts" <+> hsep (map pretty h2d) <+> "on the deck."
    pretty Festival = pretty ("Festival" :: Text.Text)
    pretty (Harbinger (DominionDraw xs) c) = "Harbinger draws" <+> hsep (map pretty xs) <+> "and pulls" <> viaShow c <> " from the discards and puts it on their deck"
    pretty (Ironworks c (DominionDraw [])) = "Ironworks gains " <> viaShow c
    pretty (Ironworks c (DominionDraw xs)) = "Ironworks gains " <> viaShow c <> " and draws" <+> hsep (map pretty xs)
    pretty (Remodel c1 c2) = "Remodels " <> viaShow c1 <> " into " <> viaShow c2
    pretty (Laboratory (DominionDraw xs)) = "Laboratory draws" <+> hsep (map pretty xs)
    pretty (Library xs ys) = "Library draws" <+> hsep (map pretty xs) <+> "discards" <+> hsep (map pretty ys)
    pretty (Lurker (Left c)) = "Lurker trashes " <> viaShow c
    pretty (Lurker (Right c)) = "Lurker gains " <> viaShow c <> " from the trash."
    pretty (Market (DominionDraw xs)) = "Market draws" <+> hsep (map pretty xs)
    pretty (Merchant (DominionDraw xs)) = "Merchant draws" <+> hsep (map pretty xs)
    pretty (Militia xs) = "Militia:" <+> align (vsep $ map militiaResponse $ Map.toList xs)
    pretty (Mine c1 c2) = "Mine " <> viaShow c1 <> " into " <> viaShow c2
    pretty (Moat (DominionDraw xs)) = "Moat draws" <+> hsep (map pretty xs)
    pretty MoneyLender = pretty ("MoneyLender" :: Text.Text)
    pretty (Poacher (DominionDraw xs) ys) = "Poacher draws" <+> hsep (map pretty xs) <+> "discards" <+> hsep (map pretty ys)
    pretty (Sentry (DominionDraw ws) xs ys zs) = "Sentry draws" <+> hsep (map pretty ws) <+> "trashes" <+> hsep (map pretty xs) <+> "discards" <+> hsep (map pretty ys) <+> "keeps" <+> hsep (map pretty zs)
    pretty (ShantyTown (DominionDraw []) hnd) = "Shanty Town reveals a hand with no actions: " <+> hsep (map pretty hnd)
    pretty (ShantyTown (DominionDraw xs) hnd) = "Shanty Town draws" <+> hsep (map pretty xs) <+> "after revealing a hand with actions:" <+> hsep (map pretty hnd)
    pretty (Smithy (DominionDraw xs)) = "Smithy draws" <+> hsep (map pretty xs)
    pretty (ThroneRoom c da1 da2) = "Throne Rooms " <> viaShow c <> " for" <+> hsep (map pretty [da1, da2])
    pretty (Vassal Nothing) = pretty ("Vassal" :: Text.Text)
    pretty (Vassal (Just c)) = "Vassal plays " <> viaShow c
    pretty (Village (DominionDraw xs)) = "Village draws" <+> hsep (map pretty xs)
    pretty (Witch (DominionDraw xs) ys) = "Witch:" <+> align (vsep $ "Draws " <> hsep (map pretty xs) : (map witchResponse $ Map.toList ys))
    pretty (Workshop c) = "Workshop gains " <> viaShow c

militiaResponse :: (PlayerNumber, Either Card [Card]) -> Doc ann
militiaResponse (k, Left c) = "Player " <> viaShow k <> " defends with " <> viaShow c
militiaResponse (k, Right xs) = "Player " <> viaShow k <> " discards" <+> hsep (map pretty xs)

witchResponse :: (PlayerNumber, Either Card (Maybe Card)) -> Doc ann
witchResponse (k, Left c) = "Player " <> viaShow k <> " defends with " <> viaShow c
witchResponse (k, Right Nothing) = "Player " <> viaShow k <> " would gain a curse, but none are left."
witchResponse (k, Right (Just c)) = "Player " <> viaShow k <> " gains " <> viaShow c

bureaucratResponse :: (PlayerNumber, Either Card (Maybe Card)) -> Doc ann
bureaucratResponse (k, Left c) = "Player " <> viaShow k <> " defends with " <> viaShow c
bureaucratResponse (k, Right Nothing) = "Player " <> viaShow k <> " discards nothing."
bureaucratResponse (k, Right (Just c)) = "Player " <> viaShow k <> " discards " <> viaShow c <> "."

instance {-# OVERLAPS #-} Pretty (PlayerNumber, Either Card BanditDecision) where
    pretty (n, Left c) = "Player #" <> viaShow n <> " showed " <> viaShow (cardName c)
    pretty (n, Right (BanditDecision (Just c) dcs)) = "Player #" <> viaShow n <> " trashed " <> viaShow (cardName c) <> " and discarded:" <+> viaShow dcs
    pretty (n, Right (BanditDecision Nothing dcs)) = "Player #" <> viaShow n <> " discarded:" <+> viaShow dcs

instance Pretty DominionDraw where
    pretty (DominionDraw xs) = align $ hsep $ map pretty xs
