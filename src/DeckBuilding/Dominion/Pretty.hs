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
                     Festival, Harbinger, Island, Remodel, Laboratory, Library, Market,
                     Merchant, Mine, Moat, MoneyLender, Poacher, Sentry, Smithy,
                     ThroneRoom, Vassal, Village, Embargo, Haven, HavenDuration,
                     NativeVillage, PearlDiver, FishingVillage, FishingVillageDuration,
                     Lighthouse, LighthouseDuration, Bazaar, Lookout, Warehouse, Caravan,
                     CaravanDuration, Navigator, ThroneRoom, Vassal, Village, Militia,
                     Harem, Duke, Bureaucrat, Conspirator, CouncilRoom, Courtyard,
                     Ironworks, Lurker, ShantyTown, Witch, Ambassador, Cutpurse,
                     PirateShip, Salvager, SeaHag, TreasureMap, Explorer),
      DominionBuy(..),
      DominionPlayerTurn(DominionPlayerTurn),
      DominionTurn(..),
      Card(cardName),
      Strategy(strategyName),
      DominionGame(DominionGame), DominionAIGame (pirateShip) )
import DeckBuilding.Types ( PlayerNumber )
import qualified Data.Text as Text
import System.Random (StdGen)
import qualified Data.Map as Map

instance Pretty DominionGame where
    pretty (DominionGame pls kndms sed trns res) =
        vsep [indent 2 $ "Dominion Game " <> pretty sed
             , indent 4 "Players", indent 6 $ align $ vsep $ map pretty pls
             , indent 4 "Kingdom", indent 6 $ align $ hsep $ map pretty kndms
             , indent 4 "Turns", indent 6 $ align $ vsep $ map pretty trns
             , indent 4 "Results", indent 6 $ align $ vsep $ map pretty res
             ]

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
    pretty (Ambassador xs ys) = "Ambassador:" <+> align (vsep $ "Returns " <> hsep (map pretty xs) <> " to the supply." : map ambassadorResponse (Map.toList ys))
    pretty (Artisan gc ctd) = "Artisan gains" <+> pretty gc <> "," <+> pretty ctd <+> "put on deck"
    pretty (Bandit resps) = "Bandit:" <+> align (vsep (map pretty $ Map.toList resps))
    pretty (Bazaar (DominionDraw xs)) = "Bazaar draws" <+> hsep (map pretty xs)
    pretty (Bureaucrat xs) = "Bureaucrat:" <+> align (vsep $ map bureaucratResponse $ Map.toList xs)
    pretty (Caravan (DominionDraw xs)) = "Caravan draws" <+> hsep (map pretty xs)
    pretty (CaravanDuration (DominionDraw xs)) = "Caravan (Duration) draws" <+> hsep (map pretty xs)
    pretty (Chapel xs) = "Chapel trashes" <+> hsep (map pretty xs)
    pretty (Cellar xs (DominionDraw dds)) = "Cellar discards" <+> hsep (map pretty xs) <+> "and draws" <+> hsep (map pretty dds)
    pretty (Conspirator (DominionDraw xs)) = "Conspirator draws" <+> hsep (map pretty xs)
    pretty (CouncilRoom (DominionDraw xs) others) = "Council Room draws" <+> hsep (map pretty xs) <+> "and causes others to draw:" <+> vsep (map (\(k,v) -> "Player draws " <> pretty v) $ Map.toList others)
    pretty (Courtyard (DominionDraw []) []) = "Courtyard cannot draw anything and has nothing to put back on the deck."
    pretty (Courtyard (DominionDraw []) h2d) = "Courtyard is unable to draw any cards, and puts" <+> hsep (map pretty h2d) <+> "on the deck."
    pretty (Courtyard (DominionDraw xs) h2d) = "Courtyard draws" <+> hsep (map pretty xs) <+> "and puts" <+> hsep (map pretty h2d) <+> "on the deck."
    pretty (Cutpurse xs) = "Cutpurse:" <+> align (vsep $ map cutpurseResponse $ Map.toList xs)
    pretty (Embargo x) = "Embargoes" <+> pretty x
    pretty (Explorer c) = "Explorer gained" <+> pretty c
    pretty Festival = pretty ("Festival" :: Text.Text)
    pretty FishingVillage = pretty ("Fishing Village" :: Text.Text)
    pretty FishingVillageDuration  = pretty ("Fishing Village (Duration)" :: Text.Text)
    pretty (Harbinger (DominionDraw xs) c) = "Harbinger draws" <+> hsep (map pretty xs) <+> "and pulls" <+> pretty c <+> "from the discards and puts it on their deck"
    pretty (Haven (DominionDraw xs) c) = "Haven draws" <+> hsep (map pretty xs) <+> "and puts aside" <+> pretty c <+> "for next turn"
    pretty (HavenDuration c) = "Haven (Duration) puts " <> pretty c <> " into the hand"
    pretty (Island mc) = "Islands" <+> pretty mc
    pretty (Ironworks c (DominionDraw [])) = "Ironworks gains" <+> pretty c
    pretty (Ironworks c (DominionDraw xs)) = "Ironworks gains" <+> pretty c <+> "and draws" <+> hsep (map pretty xs)
    pretty (Remodel c1 c2) = "Remodels " <> pretty c1 <> " into " <> pretty c2
    pretty (Laboratory (DominionDraw xs)) = "Laboratory draws" <+> hsep (map pretty xs)
    pretty (Library xs ys) = "Library draws" <+> hsep (map pretty xs) <+> "discards" <+> hsep (map pretty ys)
    pretty Lighthouse = pretty ("Lighthouse" :: Text.Text)
    pretty LighthouseDuration = pretty ("Lighthouse (Duration)" :: Text.Text)
    pretty (Lookout x y z) = "Lookout trashed " <> pretty x <> ", discarded " <> pretty y <> ", and put " <> pretty z <> " back on the deck."
    pretty (Lurker (Left c)) = "Lurker trashes " <> pretty c
    pretty (Lurker (Right c)) = "Lurker gains " <> pretty c <> " from the trash."
    pretty (Market (DominionDraw xs)) = "Market draws" <+> hsep (map pretty xs)
    pretty (Merchant (DominionDraw xs)) = "Merchant draws" <+> hsep (map pretty xs)
    pretty (Militia xs) = "Militia:" <+> align (vsep $ map militiaResponse $ Map.toList xs)
    pretty (Mine c1 c2) = "Mine " <> pretty c1 <> " into " <> pretty c2
    pretty (Moat (DominionDraw xs)) = "Moat draws" <+> hsep (map pretty xs)
    pretty MoneyLender = pretty ("MoneyLender" :: Text.Text)
    pretty (NativeVillage (Left c)) = "Native Village a" <+> pretty c
    pretty (NativeVillage (Right xs)) = "Native Village" <+> hsep (map pretty xs) <+> "into hand"
    pretty (Navigator []) = pretty ("Navigator discards all." :: Text.Text)
    pretty (Navigator xs) = "Navigator puts cards back on the deck in the order" <+> hsep (map pretty xs)
    pretty (PearlDiver (DominionDraw xs) c True) = "PearlDiver draws " <+> hsep (map pretty xs) <+> " sees " <> pretty c <> " on the bottom of the deck and moves it to the top."
    pretty (PearlDiver (DominionDraw xs) c False) = "PearlDiver draws " <+> hsep (map pretty xs) <+> " sees " <> pretty c <> " on the bottom of the deck leaves it."
    pretty (PirateShip (Left n)) = "Pirate gains " <> viaShow n <> " money from pirate mat Coin tokens."
    pretty (PirateShip (Right xs)) = "Pirate:" <+> align (vsep $ map pirateResponse $ Map.toList xs)
    pretty (Poacher (DominionDraw []) ys) = "Poacher discards" <+> hsep (map pretty ys)
    pretty (Poacher (DominionDraw xs) []) = "Poacher draws" <+> hsep (map pretty xs)
    pretty (Poacher (DominionDraw xs) ys) = "Poacher draws" <+> hsep (map pretty xs) <+> "discards" <+> hsep (map pretty ys)
    pretty (Salvager c) = "Salvage" <+> pretty c
    pretty (SeaHag xs) = "Sea Hag:" <+> align (vsep $ map seaHagResponse $ Map.toList xs)
    pretty (Sentry (DominionDraw ws) xs ys zs) = "Sentry draws" <+> hsep (map pretty ws) <+> "trashes" <+> hsep (map pretty xs) <+> "discards" <+> hsep (map pretty ys) <+> "keeps" <+> hsep (map pretty zs)
    pretty (ShantyTown (DominionDraw []) hnd) = "Shanty Town reveals a hand with no actions:" <+> hsep (map pretty hnd)
    pretty (ShantyTown (DominionDraw xs) hnd) = "Shanty Town draws" <+> hsep (map pretty xs) <+> "after revealing a hand with actions:" <+> hsep (map pretty hnd)
    pretty (Smithy (DominionDraw xs)) = "Smithy draws" <+> hsep (map pretty xs)
    pretty (ThroneRoom c da1 da2) = "Throne Rooms " <> pretty c <> " for" <+> align (vsep $ map pretty [da1, da2])
    pretty (TreasureMap xs) = "Treasure Map trashed two Treasure Maps for" <+> hsep (map pretty xs)
    pretty (Vassal Nothing) = pretty ("Vassal" :: Text.Text)
    pretty (Vassal (Just c)) = "Vassal plays " <> pretty c
    pretty (Village (DominionDraw xs)) = "Village draws" <+> hsep (map pretty xs)
    pretty (Warehouse (DominionDraw xs) ys) = "Warehouse draws" <+> hsep (map pretty xs) <+> "and discards" <+> hsep (map pretty ys)
    pretty (Witch (DominionDraw xs) ys) = "Witch:" <+> align (vsep $ "Draws " <> hsep (map pretty xs) : map witchResponse (Map.toList ys))
    pretty (Workshop c) = "Workshop gains " <> pretty c

militiaResponse :: (PlayerNumber, Either Card [Card]) -> Doc ann
militiaResponse (k, Left c) = "Player " <> viaShow k <> " defends with " <> pretty c
militiaResponse (k, Right xs) = "Player " <> viaShow k <> " discards" <+> hsep (map pretty xs)

witchResponse :: (PlayerNumber, Either Card (Maybe Card)) -> Doc ann
witchResponse (k, Left c) = "Player " <> viaShow k <> " defends with " <> pretty c
witchResponse (k, Right Nothing) = "Player " <> viaShow k <> " would gain a curse, but none are left."
witchResponse (k, Right (Just c)) = "Player " <> viaShow k <> " gains " <> pretty c

bureaucratResponse :: (PlayerNumber, Either Card (Maybe Card)) -> Doc ann
bureaucratResponse (k, Left c) = "Player " <> viaShow k <> " defends with " <> pretty c
bureaucratResponse (k, Right Nothing) = "Player " <> viaShow k <> " discards nothing."
bureaucratResponse (k, Right (Just c)) = "Player " <> viaShow k <> " discards " <> pretty c <> "."

ambassadorResponse :: (PlayerNumber, Either Card (Maybe Card)) -> Doc ann
ambassadorResponse (k, Left c) = "Player " <> viaShow k <> " defends with " <> pretty c
ambassadorResponse (k, Right Nothing) = "Player " <> viaShow k <> " gains nothing."
ambassadorResponse (k, Right (Just c)) = "Player " <> viaShow k <> " gains " <> pretty c <> "."

cutpurseResponse :: (PlayerNumber, Either Card (Maybe Card)) -> Doc ann
cutpurseResponse (k, Left c) = "Player " <> viaShow k <> " defends with " <> pretty c
cutpurseResponse (k, Right Nothing) = "Player " <> viaShow k <> " discards nothing."
cutpurseResponse (k, Right (Just c)) = "Player " <> viaShow k <> " discards " <> pretty c <> "."

pirateResponse :: (PlayerNumber, Either Card (Maybe Card)) -> Doc ann
pirateResponse (k, Left c) = "Player " <> viaShow k <> " defends with" <+> pretty c
pirateResponse (k, Right Nothing) = "Player " <> viaShow k <> " does not trash a treasure card."
pirateResponse (k, Right (Just c)) = "Player " <> viaShow k <> " trashes" <+> pretty c

seaHagResponse :: (PlayerNumber, Either Card (Maybe Card, Maybe Card)) -> Doc ann
seaHagResponse (k, Left c) = "Player " <> viaShow k <> " defends with" <+> pretty c
seaHagResponse (k, Right (Nothing, Nothing)) = "Player " <> viaShow k <> " has nothing to discard, and there are no more curse cards."
seaHagResponse (k, Right (Nothing, Just c)) = "Player " <> viaShow k <> " has nothing to discard, and gains" <+> pretty c
seaHagResponse (k, Right (Just c, Nothing)) = "Player " <> viaShow k <> " discards" <+> pretty c <+> "and there are no more curse cards."
seaHagResponse (k, Right (Just c, Just c1)) = "Player " <> viaShow k <> " discards" <+> pretty c <+> "and gains" <+> pretty c1

instance {-# OVERLAPS #-} Pretty (PlayerNumber, Either Card BanditDecision) where
    pretty (n, Left c) = "Player #" <> viaShow n <> " showed " <> pretty c
    pretty (n, Right (BanditDecision (Just c) dcs)) = "Player #" <> viaShow n <> " trashed " <> pretty c <> " and discarded:" <+> hsep (map pretty dcs)
    pretty (n, Right (BanditDecision Nothing dcs)) = "Player #" <> viaShow n <> " discarded:" <+> hsep (map pretty dcs)

instance Pretty DominionDraw where
    pretty (DominionDraw xs) = align $ hsep $ map pretty xs
