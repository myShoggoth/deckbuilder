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
                     Duchy, Province, Gardens, Artisan, Astrolabe, Bandit, Chapel, Cellar,
                     Festival, Harbinger, Island, Remodel, Laboratory, Library, Market,
                     Merchant, Mine, Moat, MoneyLender, Poacher, Sentry, Smithy,
                     ThroneRoom, Vassal, Village, Embargo, Haven, HavenDuration,
                     NativeVillage, PearlDiver, FishingVillage, FishingVillageDuration,
                     Lighthouse, LighthouseDuration, Bazaar, Lookout, Warehouse, Caravan,
                     CaravanDuration, Navigator, ThroneRoom, Vassal, Village, Militia,
                     Harem, Duke, Bureaucrat, Conspirator, CouncilRoom, Courtyard,
                     Ironworks, Lurker, ShantyTown, Witch, Ambassador, Cutpurse,
                     PirateShip, Salvager, SeaHag, TreasureMap, Explorer, GhostShip,
                     MerchantShip, MerchantShipDuration, Wharf, WharfDuration, Treasury,
                     TacticianDuration, Tactician, Outpost, TreasuryDuration, OutpostDuration,
                     Swindler, Steward, Masquerade, Pawn, WishingWell, Baron, Smuggler,
                     AstrolabeDuration, TidePools, TidePoolsDuration, SeaChart, Blockade,
                     Monkey, MonkeyDuration, Corsair, Sailor, SeaWitch, Bridge, Diplomat, Upgrade,
                     Nobles, Patrol),
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
    pretty (DominionPlayerTurn p t b a (DominionDraw d) g) =
        vsep [ "Player " <> viaShow p <> " turn " <> viaShow t
             , indent 2 "Buys", indent 4 $ align $ hsep $ map pretty b
             , indent 2 "Actions", indent 4 $ align $ vsep $ map pretty a
             , indent 2 "Draws", indent 4 $ align $ hsep $ map pretty d
             , indent 2 "Ganied", indent 4 $ align $ hsep $ map pretty g
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
    pretty Astrolabe = pretty ("Astrolabe" :: Text.Text)
    pretty AstrolabeDuration = pretty ("Astrolabe (Duration)" :: Text.Text)
    pretty (Bandit resps) = "Bandit:" <+> align (vsep (map pretty $ Map.toList resps))
    pretty (Baron True) = "Baron discarded an estate for four money"
    pretty (Baron False) = "Baron did not discard an estate"
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
    pretty (GhostShip (DominionDraw xs) ys) = "Ghost Ship:" <+> align (vsep $ "Draws" <+> hsep (map pretty xs) : map ghostShipResponse (Map.toList ys)) 
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
    pretty (Masquerade (DominionDraw xs) passed mtrashed) = "Masquerade draws" <+> hsep (map pretty xs) <+> "Passed cards" <+> hsep (map pretty passed) <+> "Trashed " <> pretty mtrashed
    pretty (Merchant (DominionDraw xs)) = "Merchant draws" <+> hsep (map pretty xs)
    pretty MerchantShip = pretty ("Merchant Ship" :: Text.Text)
    pretty MerchantShipDuration = pretty ("Merchant Ship (Duration)" :: Text.Text)
    pretty (Militia xs) = "Militia:" <+> align (vsep $ map militiaResponse $ Map.toList xs)
    pretty (Mine c1 c2) = "Mine " <> pretty c1 <> " into " <> pretty c2
    pretty (Moat (DominionDraw xs)) = "Moat draws" <+> hsep (map pretty xs)
    pretty MoneyLender = pretty ("MoneyLender" :: Text.Text)
    pretty (NativeVillage (Left c)) = "Native Village a" <+> pretty c
    pretty (NativeVillage (Right xs)) = "Native Village" <+> hsep (map pretty xs) <+> "into hand"
    pretty (Navigator []) = pretty ("Navigator discards all." :: Text.Text)
    pretty (Navigator xs) = "Navigator puts cards back on the deck in the order" <+> hsep (map pretty xs)
    pretty Outpost = pretty ("Outpost" :: Text.Text)
    pretty (OutpostDuration (DominionDraw xs) ys) = "Outpost (Duration) draws" <+> hsep (map pretty xs) <+> "and buys" <+> hsep (map pretty ys) <+> "for the bonus turn."
    pretty (Pawn (DominionDraw xs)) = "Pawn drew" <+> hsep (map pretty xs)
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
    pretty (Smuggler c) = "Smuggler gains " <> pretty c
    pretty (Steward (DominionDraw xs) 0 []) = "Steward draws" <+> hsep (map pretty xs)
    pretty (Steward (DominionDraw []) n []) = "Steward gains " <> pretty n <> " money"
    pretty (Steward (DominionDraw []) 0 xs) = "Steward trashes" <+> hsep (map pretty xs)
    pretty (Steward _ _ _) = error "Steward card chose multiple options?"
    pretty (Swindler responses) = "Swindler:" <+> align (vsep (map (\(k, (mc, mc2)) -> "Player " <> viaShow k <> " trashes " <> pretty mc <> " and gains " <> pretty mc2 <> "to discard.") (Map.toList responses)))
    pretty (Tactician xs) = "Tactician discards" <+> hsep (map pretty xs)
    pretty (TacticianDuration (DominionDraw xs)) = "Tactician (Duration) draws" <+> hsep (map pretty xs)
    pretty (ThroneRoom c da1 da2) = "Throne Rooms " <> pretty c <> " for" <+> align (vsep $ map pretty [da1, da2])
    pretty (TreasureMap xs) = "Treasure Map trashed two Treasure Maps for" <+> hsep (map pretty xs)
    pretty (Treasury (DominionDraw xs)) = "Treasury draws" <+> hsep (map pretty xs)
    pretty TreasuryDuration = "Treasury card put on top of the deck."
    pretty (Vassal Nothing) = pretty ("Vassal" :: Text.Text)
    pretty (Vassal (Just c)) = "Vassal plays " <> pretty c
    pretty (Village (DominionDraw xs)) = "Village draws" <+> hsep (map pretty xs)
    pretty (Warehouse (DominionDraw xs) ys) = "Warehouse draws" <+> hsep (map pretty xs) <+> "and discards" <+> hsep (map pretty ys)
    pretty (Wharf (DominionDraw xs)) = "Wharf draws" <+> hsep (map pretty xs)
    pretty (WharfDuration (DominionDraw xs)) = "Wharf (Duration) draws" <+> hsep (map pretty xs)
    pretty (WishingWell (DominionDraw xs) mc True) = "Wishing well draws" <+> hsep (map pretty xs) <+> "and correctly guessed the top of deck is " <> pretty mc
    pretty (WishingWell (DominionDraw xs) mc False) = "Wishing well draws" <+> hsep (map pretty xs) <+> "and dd not guess" <+> pretty mc
    pretty (Witch (DominionDraw xs) ys) = "Witch:" <+> align (vsep $ "Draws " <> hsep (map pretty xs) : map witchResponse (Map.toList ys))
    pretty (Workshop c) = "Workshop gains " <> pretty c
    pretty (TidePools (DominionDraw cards)) = "Tide Pools draws" <+> hsep (map pretty cards)
    pretty (TidePoolsDuration discarded) = "Tide Pools (Duration) discards" <+> hsep (map pretty discarded)
    pretty (SeaChart (DominionDraw cards) Nothing) = "Sea Chart draws" <+> pretty cards
    pretty (SeaChart (DominionDraw cards) (Just revealed)) = "Sea Chart draws" <+> pretty cards <+> "and reveals" <+> pretty revealed
    pretty (Blockade c) = "Blockade places a token on" <+> pretty c
    pretty (Monkey (DominionDraw xs)) = "Monkey draws" <+> hsep (map pretty xs)
    pretty (MonkeyDuration (DominionDraw xs)) = "Monkey (Duration) draws" <+> hsep (map pretty xs)
    pretty (Corsair xs) = "Corsair:" <+> align (vsep $ map corsairResponse $ Map.toList xs)
    pretty (Sailor True) = "Sailor gains +$2 for gaining a card this turn"
    pretty (Sailor False) = "Sailor does not gain +$2"
    pretty (SeaWitch xs) = "Sea Witch:" <+> align (vsep $ map seaWitchResponse $ Map.toList xs)
      where
        seaWitchResponse (k, Left c) = "Player " <> viaShow k <> " defends with" <+> pretty c
        seaWitchResponse (k, Right Nothing) = "Player " <> viaShow k <> " gains no curse."
        seaWitchResponse (k, Right (Just c)) = "Player " <> viaShow k <> " gains " <> pretty c
    pretty Bridge = pretty ("Bridge" :: Text.Text)
    pretty (Nobles (DominionDraw xs) actions) = 
      if actions > 0
        then "Nobles chose +2 Actions"
        else "Nobles drew" <+> hsep (map pretty xs)
    pretty (Patrol (DominionDraw xs) victories others) = 
      "Patrol drew" <+> hsep (map pretty xs) <+> 
      "revealed victories:" <+> hsep (map pretty victories) <+>
      "put back:" <+> hsep (map pretty others)
    pretty (Diplomat draw handSize) = "Diplomat draws " <> pretty draw <> " and has " <> viaShow handSize <> " cards in hand."
    pretty (Upgrade draw trashed gained) = "Upgrade draws " <> pretty draw <> ", trashes " <> pretty trashed <> ", and gains " <> pretty gained

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

ghostShipResponse :: (PlayerNumber, Either Card [Card]) -> Doc ann
ghostShipResponse (k, Left c) = "Player " <> viaShow k <> " defends with" <+> pretty c
ghostShipResponse (k, Right xs) = "Player " <> viaShow k <> " puts" <+> hsep (map pretty xs) <+> "on their deck"

corsairResponse :: (PlayerNumber, Either Card (Maybe Card)) -> Doc ann
corsairResponse (k, Left c) = "Player " <> viaShow k <> " defends with" <+> pretty c
corsairResponse (k, Right Nothing) = "Player " <> viaShow k <> " does not trash a treasure card."
corsairResponse (k, Right (Just c)) = "Player " <> viaShow k <> " trashes" <+> pretty c

seaWitchResponse :: (PlayerNumber, Either Card (Maybe Card, Maybe Card)) -> Doc ann
seaWitchResponse (k, Left c) = "Player " <> viaShow k <> " defends with" <+> pretty c
seaWitchResponse (k, Right (Nothing, Nothing)) = "Player " <> viaShow k <> " has nothing to discard, and there are no more curse cards."
seaWitchResponse (k, Right (Nothing, Just c)) = "Player " <> viaShow k <> " has nothing to discard, and gains" <+> pretty c
seaWitchResponse (k, Right (Just c, Nothing)) = "Player " <> viaShow k <> " discards" <+> pretty c <+> "and there are no more curse cards."
seaWitchResponse (k, Right (Just c, Just c1)) = "Player " <> viaShow k <> " discards" <+> pretty c <+> "and gains" <+> pretty c1

instance {-# OVERLAPS #-} Pretty (PlayerNumber, Either Card BanditDecision) where
    pretty (n, Left c) = "Player #" <> viaShow n <> " showed " <> pretty c
    pretty (n, Right (BanditDecision (Just c) dcs)) = "Player #" <> viaShow n <> " trashed " <> pretty c <> " and discarded:" <+> hsep (map pretty dcs)
    pretty (n, Right (BanditDecision Nothing dcs)) = "Player #" <> viaShow n <> " discarded:" <+> hsep (map pretty dcs)

instance Pretty DominionDraw where
    pretty (DominionDraw xs) = align $ hsep $ map pretty xs
