{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module DeckBuilding.Dominion.Cards.Seaside
(
    ambassadorCard,
    astrolabeCard,
    bazaarCard,
    caravanCard,
    cutpurseCard,
    embargoCard,
    explorerCard,
    fishingVillageCard,
    ghostShipCard,
    havenCard,
    islandCard,
    lighthouseCard,
    lookoutCard,
    merchantShipCard,
    nativeVillageCard,
    outpostCard,
    pearlDiverCard,
    pirateShipCard,
    salvagerCard,
    seaHagCard,
    seaChartCard,
    smugglersCard,
    tacticianCard,
    tidePoolsCard,
    treasureMapCard,
    treasuryCard,
    warehouseCard,
    wharfCard
) where

import DeckBuilding.Dominion.Types (Card (Card), DominionState, DominionAction (Ambassador, Island, Embargo, Haven, HavenDuration, NativeVillage, PearlDiver, FishingVillage, FishingVillageDuration, Lighthouse, LighthouseDuration, Bazaar, Lookout, Warehouse, Caravan, CaravanDuration, Cutpurse, Navigator, PirateShip, Salvager, SeaHag, TreasureMap, Explorer, GhostShip, MerchantShip, MerchantShipDuration, Wharf, WharfDuration, Treasury, Tactician, TacticianDuration, Outpost, Smuggler, Astrolabe, AstrolabeDuration, TidePools, TidePoolsDuration, SeaChart), CardType (Action, Duration), DominionDraw (DominionDraw), DominionPlayer (nativeVillage), CardPlay (PlayCellar), Strategy (handToDeckStrategy))
import DeckBuilding.Types (PlayerNumber(unPlayerNumber, PlayerNumber), turnOrder)
import Control.Lens ( (^.), use, (%=), Ixed(ix), (.=), (+=), (-=), (^?), _2, _Just, _Right, (^..) )
import DeckBuilding.Dominion.Cards.Utils (simpleVictory, basicCardAction, discardCards, trashCards, gainCardsToDeck, gainCardsToHand, handToDeck, gainCardsToDiscard)
import DeckBuilding.Dominion.Utils
    ( findPlayer, removeFromCards, mkDominionAIGame, increaseCards, decreaseCards, deal )
import Data.Generics.Product (HasField(field))
import qualified Data.Map as Map
import DeckBuilding.Dominion.Cards.Base (defendsAgainstAttack, copperCard, curseCard, gainCurse, goldCard, provinceCard, silverCard)
import Control.Monad (when)
import Safe (lastMay, headMay)
import Data.List ((\\), intersect)
import Data.Maybe (isJust, mapMaybe)
import Control.Conditional (unless)

-- | Reveal a card from your hand. Return up to 2 copies
-- of it from your hand to the Supply. Then each other player
-- gains a copy of it.
ambassadorCard :: Card
ambassadorCard = Card "Ambassador" 3 ambassadorCardAction Action (simpleVictory 0)
    where
        ambassadorCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        ambassadorCardAction p = do
            thePlayer <- findPlayer p
            aig <- mkDominionAIGame p
            _ <- basicCardAction 0 (-1) 0 0 p
            let macs = (thePlayer ^. #strategy . #ambassadorStrategy) aig
            case macs of
                [] -> pure Nothing
                cs@(x:xs) ->
                    if length cs > 2
                        then error "Ambassador strategy returns more than two cards."
                        else if length cs > 1 && x /= head xs
                            then error "Ambassador strategy returns two different cards, should be identical."
                            else do
                                ds <- use #decks
                                players' <- use #players
                                #decks %= Map.mapWithKey (increaseCards x (length cs))
                                #players . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) cs
                                playerResponses <- mapM (ambassadorGain p x) $ PlayerNumber <$> [0.. length players' - 1]
                                return $ Just $ Ambassador cs $ Map.fromList playerResponses
        ambassadorGain :: PlayerNumber -> Card -> PlayerNumber -> DominionState (PlayerNumber, Either Card (Maybe Card))
        ambassadorGain e _ p | e == p = return (e, Right Nothing)
        ambassadorGain _ c p = do
            thePlayer <- findPlayer p
            case defendsAgainstAttack ambassadorCard thePlayer of
                Just defender -> return (p, Left defender)
                Nothing -> do
                    aig <- mkDominionAIGame p
                    ds <- use #decks
                    if ds Map.! c <= 0
                        then return (p, Right Nothing)
                        else do
                            gainCardsToDiscard p [c]
                            return (p, Right $ Just c)

-- | Now and at the start of your next turn
-- +$1
-- +1 Buy
astrolabeCard :: Card
astrolabeCard = Card "Astrolab" 3 astrolabeCardAction Duration (simpleVictory 0)
    where
        astrolabeCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        astrolabeCardAction p = do
            basicCardAction 0 (-1) 1 1 p
            #players . ix (unPlayerNumber p) . #duration %= ((astrolabeCard, astrolabeCardDuration):)
            pure $ Just $ Astrolabe
        astrolabeCardDuration :: PlayerNumber -> DominionState (Maybe DominionAction)
        astrolabeCardDuration p = do
            basicCardAction 0 (-1) 1 1 p
            #players . ix (unPlayerNumber p) . #played %= (astrolabeCard:)
            pure $ Just $ AstrolabeDuration

-- | +1 Card
-- +2 Actions
-- +$1.
bazaarCard :: Card
bazaarCard = Card "Bazaar" 5 bazaarCardAction Action (simpleVictory 0)
    where
        bazaarCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        bazaarCardAction p = do
            drawn <- basicCardAction 1 1 0 1 p
            pure $ Just $ Bazaar drawn

-- | +1 Card
-- +1 Action
--
-- At the start of your next turn, +1 Card.
caravanCard :: Card
caravanCard = Card "Caravan" 4 caravanCardAction Duration (simpleVictory 0)
    where
        caravanCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        caravanCardAction p = do
            drawn <- basicCardAction 1 0 0 0 p
            #players . ix (unPlayerNumber p) . #duration %= ((caravanCard, caravanCardDuration):)
            pure $ Just $ Caravan drawn
        caravanCardDuration :: PlayerNumber -> DominionState (Maybe DominionAction)
        caravanCardDuration p = do
            drawn <- basicCardAction 1 0 0 0 p
            #players . ix (unPlayerNumber p) . #played %= (caravanCard:)
            pure $ Just $ CaravanDuration drawn

-- | +$2
--
-- Each other player discards a Copper (or reveals a hand with no Copper).
cutpurseCard :: Card
cutpurseCard = Card "Cutpurse" 4 cutpurseCardAction Action (simpleVictory 0)
    where
        cutpurseCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        cutpurseCardAction p = do
            players' <- use #players
            _ <- basicCardAction 0 (-1) 0 2 p
            playerResponses <- mapM (cutpurseDiscard p) $ PlayerNumber <$> [0.. length players' - 1]
            pure $ Just $ Cutpurse $ Map.fromList playerResponses
        cutpurseDiscard :: PlayerNumber -> PlayerNumber -> DominionState (PlayerNumber, Either Card (Maybe Card))
        cutpurseDiscard e p | e == p = return (e, Right Nothing)
        cutpurseDiscard _ p = do
            thePlayer <- findPlayer p
            case defendsAgainstAttack cutpurseCard thePlayer of
                Just defender -> return (p, Left defender)
                Nothing -> do
                    aig <- mkDominionAIGame p
                    if copperCard `elem` thePlayer ^. #hand
                        then do
                            #players . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) [copperCard]
                            #players . ix (unPlayerNumber p) . #discard %= (copperCard:)
                            return (p, Right $ Just copperCard)
                        else
                            return (p, Right Nothing)

-- | +$2
--
-- Trash this card. Put an Embargo token on top of a Supply pile.
-- -----
-- When a player buys a card, he gains a Curse card per Embargo token on that pile.
embargoCard :: Card
embargoCard = Card "Embargo" 2 embargoCardAction Action (simpleVictory 0)
    where
        embargoCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        embargoCardAction p = do
            thePlayer <- findPlayer p
            aig <- mkDominionAIGame p
            _ <- basicCardAction 0 (-1) 0 0 p
            let supplyCard = (thePlayer ^. #strategy . #embargoStrategy) aig
            #embargoes %= Map.mapWithKey (increaseCards supplyCard 1)
            #players . ix (unPlayerNumber p) . #money += 2
            #players . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) [embargoCard]
            #trash %= (embargoCard:)
            return $ Just $ Embargo supplyCard

-- | You may reveal a Province from your hand. If you do, gain a Gold to your hand.
-- If you don’t, gain a Silver to your hand.
explorerCard :: Card
explorerCard = Card "Explorer" 5 explorerCardAction Action (simpleVictory 0)
    where
        explorerCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        explorerCardAction p = do
            thePlayer <- findPlayer p
            _ <- basicCardAction 0 (-1) 0 0 p
            crds <- if provinceCard `elem` (thePlayer ^. #hand ++ thePlayer ^. #played)
                then gainCardsToHand p [goldCard]
                else gainCardsToHand p [silverCard]
            return $ Just $ Explorer $ head crds

-- | +2 Actions
-- +$1
--
-- At the start of your next turn: +1 Action and +$1.
fishingVillageCard :: Card
fishingVillageCard = Card "Fishing Village" 3 fishingVillageCardAction Duration (simpleVictory 0)
    where
        fishingVillageCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        fishingVillageCardAction p = do
            _ <- basicCardAction 0 1 0 1 p
            #players . ix (unPlayerNumber p) . #duration %= ((fishingVillageCard, fishingVillageCardDuration):)
            pure $ Just FishingVillage
        fishingVillageCardDuration :: PlayerNumber -> DominionState (Maybe DominionAction)
        fishingVillageCardDuration p = do
            _ <- basicCardAction 0 1 0 1 p
            #players . ix (unPlayerNumber p) . #played %= (fishingVillageCard:)
            pure $ Just FishingVillageDuration

-- | +2 Cards
--
-- Each other player with 4 or more cards in hand puts cards from their
-- hand onto their deck until they have 3 cards in hand.
ghostShipCard :: Card
ghostShipCard = Card "Ghost Ship" 5 ghostShipCardAction Action (simpleVictory 0)
    where
        ghostShipCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        ghostShipCardAction p = do
            drawn <- basicCardAction 2 (-1) 0 0 p
            players' <- use #players
            playerResponses <- mapM (ghostShipToDeck p) $ PlayerNumber <$> [0.. length players' - 1]
            pure $ Just $ GhostShip drawn $ Map.fromList playerResponses
        ghostShipToDeck :: PlayerNumber -> PlayerNumber -> DominionState (PlayerNumber, Either Card [Card])
        ghostShipToDeck e p | e == p = return (e, Right [])
        ghostShipToDeck _ p = do
            thePlayer <- findPlayer p
            case defendsAgainstAttack ghostShipCard thePlayer of
                Just defender -> return (p, Left defender)
                Nothing -> do
                    aig <- mkDominionAIGame p
                    -- Not technically asking for what to put on the deck
                    let toDeck = (thePlayer ^. #strategy . #handToDeckStrategy) aig $ length (thePlayer ^. #hand) - 3
                    handToDeck p toDeck
                    return (p, Right toDeck)

-- | +1 Card
-- + 1 Action
--
-- Set aside a card from your hand face down (under this). At the start
-- of your next turn, put it into your hand.
havenCard :: Card
havenCard = Card "Haven" 2 havenCardAction Duration (simpleVictory 0)
    where
        havenCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        havenCardAction p = do
            thePlayer <- findPlayer p
            aig <- mkDominionAIGame p
            drawn <- basicCardAction 1 0 0 0 p
            let havened = (thePlayer ^. #strategy . #havenStrategy) aig
            #players . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) [havenCard, havened]
            #players . ix (unPlayerNumber p) . #duration %= ((havenCard, havenCardDuration havened):)
            pure $ Just $ Haven drawn havened
        havenCardDuration :: Card -> PlayerNumber -> DominionState (Maybe DominionAction)
        havenCardDuration c p = do
            #players . ix (unPlayerNumber p) . #hand %= (c:)
            #players . ix (unPlayerNumber p) . #played %= (havenCard:)
            pure $ Just $ HavenDuration c

-- | 2VP
--
-- Put this and a card from your hand onto your Island mat.
islandCard :: Card
islandCard = Card "Island" 4 islandCardAction Duration (simpleVictory 2)
    where
        islandCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        islandCardAction p = do
            thePlayer <- findPlayer p
            aig <- mkDominionAIGame p
            _ <- basicCardAction 0 (-1) 0 0 p
            let mc = (thePlayer ^. #strategy . #islandStrategy) aig
            #players . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) [islandCard]
            #players . ix (unPlayerNumber p) . #island %= (islandCard:)
            case mc of
                Nothing -> pure Nothing
                Just c -> do
                    #players . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) [c]
                    #players . ix (unPlayerNumber p) . #island %= (c:)
                    pure $ Just $ Island mc

-- | +1 Action
-- Now and at the start of your next turn: +$1.
--
-- While this is in play, when another player plays an Attack card, it doesn’t affect you.
lighthouseCard :: Card
lighthouseCard = Card "Lighthouse" 2 lighthouseCardAction Duration (simpleVictory 0)
    where
        lighthouseCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        lighthouseCardAction p = do
            _ <- basicCardAction 0 0 0 1 p
            #players . ix (unPlayerNumber p) . #duration %= ((lighthouseCard, lighthouseCardDuration):)
            #players . ix (unPlayerNumber p) . #lighthouse += 1
            pure $ Just Lighthouse
        lighthouseCardDuration :: PlayerNumber -> DominionState (Maybe DominionAction)
        lighthouseCardDuration p = do
            _ <- basicCardAction 0 0 0 1 p
            #players . ix (unPlayerNumber p) . #lighthouse -= 1
            #players . ix (unPlayerNumber p) . #played %= (lighthouseCard:)
            pure $ Just LighthouseDuration

-- | +1 Action
--
-- Look at the top 3 cards of your deck. Trash one of them. Discard one of them. Put the other one back on to your deck.
lookoutCard :: Card
lookoutCard = Card "Lookout" 3 lookoutCardAction Action (simpleVictory 0)
    where
        lookoutCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        lookoutCardAction p = do
            thePlayer <- findPlayer p
            aig <- mkDominionAIGame p
            let oldhand = thePlayer ^. #hand
            newcards <- deal 3 p
            let (trashem, disc, keep) = (thePlayer ^. #strategy . #lookoutStrategy) aig newcards
            #trash %= (trashem:)
            #players . ix (unPlayerNumber p) . #discard %= (disc:)
            #players . ix (unPlayerNumber p) . #deck %= (keep:)
            #players . ix (unPlayerNumber p) . #hand .= oldhand
            return $ Just $ Lookout trashem disc keep

-- | Now and at the start of your next turn: +$2.
merchantShipCard :: Card
merchantShipCard = Card "Merchant Ship" 5 merchantShipCardAction Duration (simpleVictory 0)
    where
        merchantShipCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        merchantShipCardAction p = do
            _ <- basicCardAction 0 (-1) 0 2 p
            #players . ix (unPlayerNumber p) . #duration %= ((merchantShipCard, merchantShipCardDuration):)
            pure $ Just MerchantShip
        merchantShipCardDuration :: PlayerNumber -> DominionState (Maybe DominionAction)
        merchantShipCardDuration p = do
            _ <- basicCardAction 0 0 0 2 p
            #players . ix (unPlayerNumber p) . #played %= (merchantShipCard:)
            pure $ Just MerchantShipDuration

-- | + 2 Actions
--
-- Choose one: Put the top card of your deck face down on your Native
-- Village mat (you may look at those cards at any time); or put all
-- the cards from your mat into your hand.
nativeVillageCard :: Card
nativeVillageCard = Card "Native Village" 2 nativeVillageCardAction Action (simpleVictory 0)
    where
        nativeVillageCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        nativeVillageCardAction p = do
            thePlayer <- findPlayer p
            aig <- mkDominionAIGame p
            let addToMat = (thePlayer ^. #strategy . #nativeVillageStrategy) aig
            if addToMat
                then do
                    (DominionDraw drawn) <- basicCardAction 1 1 0 0 p
                    thePlayer' <- findPlayer p
                    #players . ix (unPlayerNumber p) . #nativeVillage %= (++drawn)
                    #players . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer' ^. #hand) drawn
                    pure $ Just $ NativeVillage $ Left (head drawn)
                else do
                    let villaged = thePlayer ^. #nativeVillage
                    _ <- basicCardAction 0 1 0 0 p
                    #players . ix (unPlayerNumber p) . #hand %= (++villaged)
                    #players . ix (unPlayerNumber p) . #nativeVillage .= []
                    pure $ Just $ NativeVillage $ Right villaged

-- | +$2
--
-- Look at the top 5 cards of your deck. Either discard them all, or put them back in any order.
navigatorCard :: Card
navigatorCard = Card "Navigator" 4 navigatorCardAction Action (simpleVictory 0)
    where
        navigatorCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        navigatorCardAction p = do
            (DominionDraw drawn) <- basicCardAction 5 (-1) 0 2 p
            thePlayer <- findPlayer p
            aig <- mkDominionAIGame p
            let reorder = (thePlayer ^. #strategy . #navigatorStrategy) aig drawn
            thePlayer' <- findPlayer p
            case reorder of
                [] -> #players . ix (unPlayerNumber p) . #discard %= (reorder++)
                xs -> #players . ix (unPlayerNumber p) . #deck .= xs ++ thePlayer' ^. #hand
            pure $ Just $ Navigator reorder

-- | If this is the first time you played an Outpost this turn, and the previous
-- turn wasn’t yours, then take an extra turn after this one, and you only draw
-- 3 cards for your next hand.
outpostCard :: Card
outpostCard = Card "Outpost" 5 outpostCardAction Action (simpleVictory 0)
    where
        outpostCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        outpostCardAction p = do
            #players . ix (unPlayerNumber p) . #outpost .= True
            pure $ Just Outpost

-- | +1 Card
-- +1 Action
--
-- Look at the bottom card of your deck. You may put it on top.
pearlDiverCard :: Card
pearlDiverCard = Card "Pearl Diver" 2 pearlDiverCardAction Action (simpleVictory 0)
    where
        pearlDiverCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        pearlDiverCardAction p = do
            drawn <- basicCardAction 1 0 0 0 p
            thePlayer <- findPlayer p
            aig <- mkDominionAIGame p
            let mc = lastMay $ thePlayer ^. #deck
            -- TODO: There's a corner case here where the basicCardAction draw above
            -- gets the last card in the deck that isn't handled, should force a
            -- reshuffling of the discards into the deck again. Need a clean way to
            -- handle this.
            case mc of
                Nothing -> pure Nothing
                Just c -> do
                    let moveToTop = (thePlayer ^. #strategy . #pearlDiverStrategy) aig c
                    when moveToTop $
                        #players . ix (unPlayerNumber p) . #deck .= c : init (thePlayer ^. #deck)
                    pure $ Just $ PearlDiver drawn c moveToTop

-- | Choose one: +$1 per Coin token on your Pirate Ship mat; or each other player
-- reveals the top 2 cards of their deck, trashes one of those Treasures that you
-- choose, and discards the rest, and then if anyone trashed a Treasure you add a
-- Coin token to your Pirate Ship mat.
pirateShipCard :: Card
pirateShipCard = Card "Pirate Ship" 4 pirateShipCardAction Action (simpleVictory 0)
    where
        pirateShipCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        pirateShipCardAction p = do
            thePlayer <- findPlayer p
            aig <- mkDominionAIGame p
            _ <- basicCardAction 0 (-1) 0 0 p
            if thePlayer ^. #strategy . #pirateShipStrategy $ aig
                then do
                    players' <- use #players
                    playerResponses <- mapM (pirateShipDiscard p) $ PlayerNumber <$> [0.. length players' - 1]
                    unless (null (playerResponses ^.. traverse . _2 . _Right . _Just)) $
                        #players . ix (unPlayerNumber p) . #pirateShip += 1
                    pure $ Just $ PirateShip $ Right $ Map.fromList playerResponses
                else pure $ Just $ PirateShip $ Left $ thePlayer ^. #pirateShip
        pirateShipDiscard :: PlayerNumber -> PlayerNumber -> DominionState (PlayerNumber, Either Card (Maybe Card))
        pirateShipDiscard e p | e == p = return (e, Right Nothing)
        pirateShipDiscard e p = do
            thePlayer <- findPlayer e
            case defendsAgainstAttack pirateShipCard thePlayer of
                Just defender -> return (p, Left defender)
                Nothing -> do
                    aig <- mkDominionAIGame p
                    topTwo <- deal 2 p
                    let mc = (thePlayer ^. #strategy . #pirateShipDecisionStrategy) aig topTwo
                    case mc of
                        Nothing -> do
                            #players . ix (unPlayerNumber p) . #discard %= (topTwo++)
                            return (p, Right Nothing)
                        Just c -> do
                            #trash %= (c:)
                            #players . ix (unPlayerNumber p) . #discard %= ((topTwo \\ [c])++)
                            return (p, Right $ Just c)

-- | +1 Buy
--
-- Trash a card from your hand. +$1 per $1 it costs.
salvagerCard :: Card
salvagerCard = Card "Salvager" 4 salvagerCardAction Action (simpleVictory 0)
    where
        salvagerCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        salvagerCardAction p = do
            thePlayer <- findPlayer p
            aig <- mkDominionAIGame p
            let mc = thePlayer ^. #strategy . #salvagerStrategy $ aig
            case mc of
                Nothing -> pure Nothing
                Just c -> do
                    _ <- basicCardAction 0 (-1) 1 (c ^. #cost) p
                    trashCards p [c]
                    pure $ Just $ Salvager c

-- | Gain a copy of a card costing up to 6 that the player to your right gained on their last turn.
smugglersCard :: Card
smugglersCard = Card "Smugglers" 3 smugglersCardAction Action (simpleVictory 0)
    where
        smugglersCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        smugglersCardAction p = do
            players' <- use #players
            let numPlayers = length players'
            thePlayer <- findPlayer p
            pRight <- findPlayer $ playerToRight players' p
            let gained = pRight ^. #gained
            aig <- mkDominionAIGame p
            let mc = (thePlayer ^. #strategy . #smugglerStrategy) aig gained
            case mc of
                Nothing -> pure Nothing
                Just c -> do
                    gainCardsToDiscard p [c]
                    pure $ Just $ Smuggler c
        playerToRight :: [DominionPlayer] -> PlayerNumber -> PlayerNumber
        playerToRight ps (PlayerNumber p) = if p == 0
            then PlayerNumber ((length ps) - 1)
            else PlayerNumber (p - 1)

-- | If you have at least one card in hand, discard your hand, and at the start of your next turn, +5 Cards, +1 Action, and +1 Buy.
tacticianCard :: Card
tacticianCard = Card "Tactician" 5 tacticianCardAction Duration (simpleVictory 0)
    where
        tacticianCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        tacticianCardAction p = do
            _ <- basicCardAction 0 (-1) 0 0 p
            thePlayer <- findPlayer p
            if not (null (thePlayer ^. #hand))
                then do
                    let hand = thePlayer ^. #hand
                    discardCards p hand
                    #players . ix (unPlayerNumber p) . #duration %= ((tacticianCard, tacticianCardActionDuration):)
                    pure $ Just $ Tactician hand
                else pure Nothing
        tacticianCardActionDuration :: PlayerNumber -> DominionState (Maybe DominionAction)
        tacticianCardActionDuration p = do
            drawn <- basicCardAction 5 1 1 0 p
            #players . ix (unPlayerNumber p) . #played %= (tacticianCard:)
            pure $ Just $ TacticianDuration drawn

-- | Trash this and a Treasure Map from your hand. If you trashed two Treasure Maps, gain 4 Golds onto your deck.
treasureMapCard :: Card
treasureMapCard = Card "Treasure Map" 4 treasureMapCardAction Duration (simpleVictory 0)
    -- This is not actually a "Duration" card, but it is the simplest way to avoid having it put into the
    -- player's 'played' pile. If we don't end up playing it, though, we need to do that here.
    where
        treasureMapCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        treasureMapCardAction p = do
            thePlayer <- findPlayer p
            if length ( (thePlayer ^. #hand) `intersect` [treasureMapCard, treasureMapCard] ) >= 2
                then do
                    _ <- basicCardAction 0 (-1) 0 0 p
                    trashCards p [treasureMapCard, treasureMapCard]
                    gained <- gainCardsToDeck p [goldCard, goldCard, goldCard, goldCard]
                    return $ Just $ TreasureMap gained
                else return Nothing

-- | +1 Card
-- +1 Action
-- +$1
--
-- When you discard this from play, if you didn’t buy a Victory card this turn, you may put this onto your deck.
treasuryCard :: Card
treasuryCard = Card "Treasury" 5 treasuryCardAction Action (simpleVictory 0)
    where
        treasuryCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        treasuryCardAction p = do
            drawn <- basicCardAction 1 0 0 1 p
            pure $ Just $ Treasury drawn

-- | Each other player discards the top card of their deck, then gains a Curse onto their deck.
seaHagCard :: Card
seaHagCard = Card "Sea Hag" 4 seaHagCardAction Action (simpleVictory 0)
    where
        seaHagCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        seaHagCardAction p = do
            _ <- basicCardAction 0 (-1) 0 0 p
            players' <- use #players
            playerResponses <- mapM (seaHagDiscard p) $ PlayerNumber <$> [0.. length players' - 1]
            pure $ Just $ SeaHag $ Map.fromList playerResponses
        seaHagDiscard :: PlayerNumber -> PlayerNumber -> DominionState (PlayerNumber, Either Card (Maybe Card, Maybe Card))
        seaHagDiscard e p | e == p = return (e, Right (Nothing, Nothing))
        seaHagDiscard _ p = do
            thePlayer <- findPlayer p
            case defendsAgainstAttack seaHagCard thePlayer of
                Just defender -> return (p, Left defender)
                Nothing -> do
                    topCard <- deal 1 p
                    discardCards p topCard
                    mc <- gainCurse p
                    return (p, Right (headMay topCard, mc))

-- | +1 Card
-- +1 Action
-- Reveal the top card of your deck. If you have a copy of it in play, put it into your hand.
seaChartCard :: Card
seaChartCard = Card "Sea Chart" 3 seaChartCardAction Action (simpleVictory 0)
    where
        seaChartCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        seaChartCardAction p = do
            drawn <- basicCardAction 1 0 0 0 p
            thePlayer <- findPlayer p
            let topCard = headMay (thePlayer ^. #deck)
            let inPlay = thePlayer ^. #played ++ map fst (thePlayer ^. #duration)
            case topCard of
                Just c -> do
                    if c `elem` inPlay
                        then do
                            #players . ix (unPlayerNumber p) . #hand %= (c:)
                            #players . ix (unPlayerNumber p) . #deck %= tail
                            pure $ Just $ SeaChart drawn (Just c)
                        else pure $ Just $ SeaChart drawn Nothing
                Nothing -> pure $ Just $ SeaChart drawn Nothing

-- | +3 Cards
-- +1 Action
--
-- At the start of your next turn, discard 2 cards.
tidePoolsCard :: Card
tidePoolsCard = Card "Tide Pools" 4 tidePoolsCardAction Duration (simpleVictory 0)
    where
        tidePoolsCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        tidePoolsCardAction p = do
            drawn <- basicCardAction 3 0 0 0 p
            #players . ix (unPlayerNumber p) . #duration %= ((tidePoolsCard, tidePoolsCardDuration):)
            pure $ Just $ TidePools drawn
        tidePoolsCardDuration :: PlayerNumber -> DominionState (Maybe DominionAction)
        tidePoolsCardDuration p = do
            thePlayer <- findPlayer p
            aig <- mkDominionAIGame p
            let hand = thePlayer ^. #hand
            let toDiscard = (thePlayer ^. #strategy . #discardStrategy) aig (length hand, 2)
            discardCards p toDiscard
            #players . ix (unPlayerNumber p) . #played %= (tidePoolsCard:)
            pure $ Just $ TidePoolsDuration toDiscard

-- | +3 Cards
-- +1 Action
--
-- Discard 3 cards.
warehouseCard :: Card
warehouseCard = Card "Warehouse" 3 warehouseCardAction Action (simpleVictory 0)
    where
        warehouseCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        warehouseCardAction p = do
            drawn <- basicCardAction 3 0 0 0 p
            thePlayer <- findPlayer p
            aig <- mkDominionAIGame p
            let discards = (thePlayer ^. #strategy . #discardStrategy) aig (3, 3)
            discardCards p discards
            pure $ Just $ Warehouse drawn discards

-- | Now and at the start of your next turn: +2 Cards and +1 Buy.
wharfCard :: Card
wharfCard = Card "Wharf" 5 wharfCardAction Duration (simpleVictory 0)
    where
        wharfCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        wharfCardAction p = do
            drawn <- basicCardAction 2 (-1) 1 0 p
            #players . ix (unPlayerNumber p) . #duration %= ((wharfCard, wharfCardActionDuration):)
            pure $ Just $ Wharf drawn
        wharfCardActionDuration :: PlayerNumber -> DominionState (Maybe DominionAction)
        wharfCardActionDuration p = do
            drawn <- basicCardAction 2 0 1 0 p
            #players . ix (unPlayerNumber p) . #played %= (wharfCard:)
            pure $ Just $ WharfDuration drawn
