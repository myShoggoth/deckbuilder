{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module DeckBuilding.Dominion.Cards.Seaside
(
    ambassadorCard,
    embargoCard,
    havenCard,
    islandCard
) where

import DeckBuilding.Dominion.Types (Card (Card), DominionState, DominionAction (Ambassador, Island, Embargo, Haven, HavenDuration), CardType (Action, Duration))
import DeckBuilding.Types (PlayerNumber(unPlayerNumber, PlayerNumber))
import Control.Lens ( (^.), use, (%=), Ixed(ix), (.=), (+=) )
import DeckBuilding.Dominion.Cards.Utils (simpleVictory, basicCardAction)
import DeckBuilding.Dominion.Utils
    ( findPlayer, removeFromCards, mkDominionAIGame, increaseCards, decreaseCards )
import Data.Generics.Product (HasField(field))
import qualified Data.Map as Map
import DeckBuilding.Dominion.Cards.Base (defendsAgainstAttack)

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
                                field @"decks" %= Map.mapWithKey (increaseCards x (length cs))
                                field @"players" . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) cs
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
                            field @"decks" %= Map.mapWithKey (decreaseCards c)
                            field @"players" . ix (unPlayerNumber p) . #discard %= (c:)
                            return (p, Right $ Just c)


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
            let supplyCard = (thePlayer ^. #strategy . #embargoStrategy) aig
            field @"embargoes" %= Map.mapWithKey (increaseCards supplyCard 1)
            field @"players" . ix (unPlayerNumber p) . #money += 2
            field @"players" . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) [embargoCard]
            field @"trash" %= (embargoCard:)
            return $ Just $ Embargo supplyCard

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
            field @"players" . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) [havenCard, havened]
            field @"players" . ix (unPlayerNumber p) . #duration %= (havenCardDuration havened:)
            pure $ Just $ Haven drawn havened
        havenCardDuration :: Card -> PlayerNumber -> DominionState (Maybe DominionAction)
        havenCardDuration c p = do
            field @"players" . ix (unPlayerNumber p) . #hand %= (c:)
            pure $ Just $ HavenDuration c

-- | 2VP
--
-- Put this and a card from your hand onto your Island mat.
islandCard :: Card
islandCard = Card "Island" 4 islandCardAction Action (simpleVictory 2)
    where
        islandCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
        islandCardAction p = do
            thePlayer <- findPlayer p
            aig <- mkDominionAIGame p
            let mc = (thePlayer ^. #strategy . #islandStrategy) aig
            field @"players" . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) [islandCard]
            field @"players" . ix (unPlayerNumber p) . #island %= (islandCard:)
            case mc of
                Nothing -> pure Nothing
                Just c -> do
                    field @"players" . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) [c]
                    field @"players" . ix (unPlayerNumber p) . #island %= (c:)
                    pure $ Just $ Island mc
