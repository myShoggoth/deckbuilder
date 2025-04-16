{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE BlockArguments #-}

module DeckBuilding.Dominion.Cards.Intrigue
    ( baronCard
    , bridgeCard
    , courtyardCard
    , diplomatCard
    , lurkerCard
    , pawnCard
    , masqueradeCard
    , stewardCard
    , shantyTownCard
    , swindlerCard
    , conspiratorCard
    , ironworksCard
    , dukeCard
    , haremCard
    , wishingWellCard
    , upgradeCard
    , millCard
    , miningVillageCard
    , secretPassageCard
    ) where

import Control.Lens ( (^.), use, (%=), Ixed(ix), prism', (.=) )
import Data.Generics.Product ( HasField(field) )
import Data.List (delete)
import qualified Data.Map as Map
import DeckBuilding.Dominion.Cards.Base
    ( treasureCards, duchyCard, victoryCards, estateCard )
import DeckBuilding.Dominion.Cards.Utils
    ( simpleVictory, basicCardAction, hasActionCards, handToDeck, valueCardAction, trashCards, gainCardsToDiscard, gainCardsToDeck )
import DeckBuilding.Types ( PlayerNumber(unPlayerNumber, PlayerNumber), Game (turnOrder), Game(.. ) )
import DeckBuilding.Dominion.Types
    ( Card(Card, cost),
      CardType(Value, Action, Duration), DominionState,
        DominionAction (Courtyard, Lurker, Pawn, ShantyTown, Conspirator, Ironworks, Duke, Harem, Masquerade,
          Steward, Swindler, WishingWell, Baron, Bridge, Diplomat, Upgrade, Mill, MiningVillage, SecretPassage),
        DominionDraw(DominionDraw), Strategy (trashStrategy, gainCardStrategy, discardStrategy, secretPassageStrategy), DominionBoard )
import DeckBuilding.Dominion.Utils
    ( decreaseCards, isCardInPlay, findPlayer, mkDominionAIGame, removeFromCards, deal, discardCard )
import Data.Traversable (for)
import Control.Monad (forM)
import Safe (headMay, lastMay)
import Data.Foldable (for_)
import GHC.Base (VecElem(Int16ElemRep))
import GHC.List (product)
import Control.Monad.Extra (ifM)
import Data.Functor (($>))

-- | +1 Buy
--
-- You may discard an Estate for +4 Money. If you don't, gain an Estate.
baronCard :: Card
baronCard       = Card "Baron"        4 baronCardAction Action (simpleVictory 0)
  where
    baronCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    baronCardAction p = do
      thePlayer <- findPlayer p
      let canDiscardEstate = estateCard `elem` (thePlayer ^. #hand)
      let moneys = if canDiscardEstate
                    then 4
                    else 0
      ifM (pure canDiscardEstate) (discardCard estateCard p) (gainCardsToDeck p [estateCard] $> ())
      _ <- basicCardAction 0 (-1) 1 moneys p
      pure $ Just $ Baron canDiscardEstate

-- | +3 Cards
--
-- Put a card from your hand onto your deck.
courtyardCard :: Card
courtyardCard   = Card "Courtyard"    2 courtyardCardAction Action (simpleVictory 0)
  where
    courtyardCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    courtyardCardAction p = do
      theDeal <- basicCardAction 3 (-1) 0 0 p
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let cs = (thePlayer ^. #strategy . #handToDeckStrategy) aig 1
      handToDeck p cs
      pure $ Just $ Courtyard theDeal cs

-- | +1 Action
--
-- Choose one: Trash an Action card from the Supply; or gain an Action card from the trash.
lurkerCard :: Card
lurkerCard      = Card "Lurker"   2 lurkerCardAction Action (simpleVictory 0)
  where
    lurkerCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    lurkerCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let ec = (thePlayer ^. #strategy . #lurkerStrategy) aig
      ml <- lurk ec p
      case ml of
        Nothing -> pure Nothing
        Just l -> pure $ Just $ Lurker l
    lurk :: Either Card Card -> PlayerNumber -> DominionState (Maybe (Either Card Card))
    lurk e@(Left c) _ = do
      icip <- isCardInPlay c
      if icip
        then do
          #trash %= (c:)
          #decks %= Map.mapWithKey (decreaseCards c)
          return $ Just e
        else return Nothing
    lurk e@(Right c@(Card _ _ _ Action _)) p = do
      trsh <- use #trash
      if c `elem` trsh
        then do
          #trash %= delete c
          #players . ix (unPlayerNumber p) . #discard %= (c:)
          #players . ix (unPlayerNumber p) . #gained %= (c:)
          return $ Just e
        else return Nothing
    lurk (Right _) _ = return Nothing

-- | Choose two: +1 Card, +1 Action, +1 Buy, +1 Money
--
-- The choices must be different.
pawnCard :: Card
pawnCard        = Card "Pawn"         2 pawnCardAction Action (simpleVictory 0)
  where
    pawnCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    pawnCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let (cards, actions, buys, monies) = (thePlayer ^. #strategy . #pawnStrategy) aig
      theDeal <- basicCardAction cards (actions - 1) buys monies p
      pure $ Just $ Pawn theDeal

-- | +2 Cards
--
-- Each player with any cards in hand passes one to the next such player to their left, at once. Then you may trash a card from your hand.
masqueradeCard :: Card
masqueradeCard  = Card "Masquerade"   3 masqueradeCardAction Action (simpleVictory 0)
  where
    masqueradeCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    masqueradeCardAction p = do
      -- I hate that I can't figure out how to use @turnOrder@ here.
      players' <- use #players
      let pns = PlayerNumber <$> [0 .. length players' - 1]

      -- First draw the two cards
      drawn <- basicCardAction 2 (-1) 0 0 p

      -- Next the players simultaneously pick a card to pass to the left.
      passed :: [Maybe Card] <- for pns $ \p' -> do
        pl <- findPlayer p'
        aig <- mkDominionAIGame p'
        case (pl ^. #strategy . #masqueradePassStrategy) aig of
          Nothing -> pure Nothing
          Just ca -> do
            #players . ix (unPlayerNumber p') . #hand .= removeFromCards (pl ^. #hand) [ca]
            pure $ Just ca

      -- Next pass them to the next eligible player to the left
      for_ (zip pns passed) $ \(p', pass) -> do
        case passedToMe (unPlayerNumber p') passed of
          Nothing -> pure ()
          Just ca -> #players . ix (unPlayerNumber p') . #hand %= (ca :)

      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      -- Pick 0 or 1 card(s) to trash
      let trashed = (thePlayer ^. #strategy . #trashStrategy) aig (0,1) (thePlayer ^. #hand)
      trashCards p trashed
      pure $ Just $ Masquerade drawn passed (headMay trashed)

    passedToMe :: Int -> [Maybe Card] -> Maybe Card
    passedToMe p cs = go p (p - 1) cs

    go :: Int -> Int -> [Maybe Card] -> Maybe Card
    go p p' cs =
      if p == p'
        then Nothing
        else do
          let p'' = p' `mod` length cs
          case cs !! p'' of
            Nothing -> go p (p'' + 1) cs
            Just ca -> Just ca

-- | +2 Actions
--
-- Reveal your hand. If you have no Action cards in hand, +2 Cards.
shantyTownCard :: Card
shantyTownCard  = Card "Shanty Town"  3 shantyTownCardAction Action (simpleVictory 0)
  where
    shantyTownCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    shantyTownCardAction p = do
      thePlayer <- findPlayer p
      let h = thePlayer ^. #hand
      theDeal <- if hasActionCards 1 h
        then basicCardAction 0 1 0 0 p
        else basicCardAction 2 1 0 0 p
      pure $ Just $ ShantyTown theDeal h

-- | Choose one: +2 Cards; or +2 money; or trash 2 cards from your hand.
stewardCard :: Card
stewardCard     = Card "Steward"      3 stewardCardAction Action (simpleVictory 0)
  where
    stewardCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    stewardCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let (toDraw, moreMoney, toTrash) = (thePlayer ^. #strategy . #stewardStrategy) aig
      theDeal <- basicCardAction toDraw (-1) moreMoney 0 p
      trashCards p toTrash
      pure $ Just $ Steward theDeal moreMoney toTrash

-- | +$2
--
-- Each other player trashes the top card of their deck and gains a card with the same cost that you choose.
swindlerCard :: Card
swindlerCard =    Card "Swindler"     3 swindlerCardAction Action (simpleVictory 0)
  where
    swindlerCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    swindlerCardAction p = do
      _ <- basicCardAction 0 (-1) 0 2 p
      -- I hate that I can't figure out how to use @turnOrder@ here.
      players' <- use #players
      let pns = PlayerNumber <$> [0 .. length players' - 1]
      thePlayer <- findPlayer p

      responses :: [(PlayerNumber, (Maybe Card, Maybe Card))] <- forM pns $ \p' ->
        if p' == p
          then pure (p', (Nothing, Nothing))
          else do
            -- Trash the top of their deck
            theDraw <- deal 1 p'
            aig <- mkDominionAIGame p'
            trashCards p' theDraw
            -- Choose a card for them to gain based on the cost of the trashed card
            mgain <- case headMay theDraw of
                        Nothing -> pure Nothing
                        Just ca -> do
                          let cost = ca ^. #cost
                          let mc = (thePlayer ^. #strategy . #swindlerStrategy) aig cost
                          case mc of
                            Nothing -> pure Nothing
                            Just c -> do
                              gained <- gainCardsToDiscard p' [c]
                              pure $ headMay gained
            pure (p', (headMay theDraw, mgain))
      pure $ Just $ Swindler $ Map.fromList responses

-- | +$2
--
-- If you’ve played 3 or more Actions this turn (counting this), +1 Card and +1 Action.
conspiratorCard :: Card
conspiratorCard = Card "Conspirator"  4 conspiratorCardAction Action (simpleVictory 0)
  where
    conspiratorCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    conspiratorCardAction p = do
      thePlayer <- findPlayer p
      theDeal <- if hasActionCards 2 (thePlayer ^. #played)
        then basicCardAction 1 0 0 2 p
        else basicCardAction 0 (-1) 0 2 p
      pure $ Just $ Conspirator theDeal

-- | Gain a card costing up to $4. If the gained card is an…
--
-- Action card, +1 Action
--
-- Treasure card, +$1
--
-- Victory card, +1 Card
ironworksCard :: Card
ironworksCard   = Card "Ironworks"    4 ironworksCardAction Action (simpleVictory 0)
  where
    ironworksCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    ironworksCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let mc = (thePlayer ^. #strategy . #gainCardStrategy) aig 4
      case mc of
        Nothing   -> return Nothing
        Just card
              | (card ^. #cardType) == Action || (card ^. #cardType == Duration) -> do
                gainCardsToDiscard p [card]
                pure $ Just $ Ironworks card (DominionDraw [])
              | card `elem` treasureCards             -> do
                gainCardsToDiscard p [card]
                _ <- basicCardAction 0 (-1) 0 1 p
                pure $ Just $ Ironworks card (DominionDraw [])
              | card `elem` victoryCards              -> do
                gainCardsToDiscard p [card]
                theDeal <- basicCardAction 1 (-1) 0 0 p
                pure $ Just $ Ironworks card theDeal
        Just _ -> error "Ironworks: this should never happen."

-- | Worth 1VP per Duchy you have.
dukeCard :: Card
dukeCard        = Card "Duke"         5 (valueCardAction 0 Duke) Action dukeCardValue
  where
    dukeCardValue :: PlayerNumber -> DominionState Int
    dukeCardValue p = do
      thePlayer <- findPlayer p
      let points = length $ filter (== duchyCard) ( (thePlayer ^. #hand) ++ (thePlayer ^. #discard) ++ (thePlayer ^. #played) ++ (thePlayer ^. #deck) )
      return points

-- | $2
--
-- 2VP
haremCard :: Card
haremCard       = Card "Harem"        6 (valueCardAction 2 Harem) Value (simpleVictory 2)

-- | +1 Card
-- +1 Action
--
-- Name a card, then reveal the top card of your deck. If you named it, put it into your hand.
wishingWellCard :: Card
wishingWellCard = Card "Wishing Well" 3 wishingWellCardAction Action (simpleVictory 0)
  where
    wishingWellCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    wishingWellCardAction p = do
      drawn <- basicCardAction 1 0 0 0 p
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      case drawn of
        DominionDraw [] -> pure $ Just $ WishingWell (DominionDraw []) Nothing False
        DominionDraw [c] -> pure $ Just $ WishingWell (DominionDraw [c]) Nothing False
        DominionDraw (d:topOfDeck) -> do
          let mtod = headMay topOfDeck
          let c = (thePlayer ^. #strategy . #wishingWellStrategy) aig
          let guessed = Just c == mtod
          if guessed
            then pure ()
            else handToDeck p [c]
          pure $ Just $ WishingWell (DominionDraw [d]) mtod guessed

-- | +1 Buy, +1 Money
bridgeCard :: Card
bridgeCard = Card "Bridge" 4 bridgeCardAction Action (simpleVictory 0)
  where
    bridgeCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    bridgeCardAction p = do
      thePlayer <- findPlayer p
      let handSize = length (thePlayer ^. #hand)
      _ <- basicCardAction 0 (-1) 1 1 p
      pure $ Just Bridge

-- | +2 Cards
--
-- If you have 5 or fewer cards in hand after drawing, +2 Actions.
diplomatCard :: Card
diplomatCard = Card "Diplomat" 4 diplomatCardAction Action (simpleVictory 0)
  where
    diplomatCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    diplomatCardAction p = do
      theDeal <- basicCardAction 2 (-1) 0 0 p
      thePlayer <- findPlayer p
      let handSize = length (thePlayer ^. #hand)
      if handSize <= 5
        then basicCardAction 0 2 0 0 p >>= \draw -> pure draw
        else pure $ DominionDraw []
      pure $ Just $ Diplomat theDeal handSize

-- | +1 Card, +1 Action
--
-- You may trash a card from your hand. Gain a card costing up to $2 more than it.
upgradeCard :: Card
upgradeCard = Card "Upgrade" 5 upgradeCardAction Action (simpleVictory 0)
  where
    upgradeCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    upgradeCardAction p = do
      theDeal <- basicCardAction 1 0 0 0 p
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let toTrash = (thePlayer ^. #strategy . #trashStrategy) aig (0, 1) (thePlayer ^. #hand)
      trashCards p toTrash
      case headMay toTrash of
        Nothing -> pure $ Just $ Upgrade theDeal Nothing Nothing
        Just c -> do
          let costLimit = c ^. #cost + 2
          let toGain = (thePlayer ^. #strategy . #gainCardStrategy) aig costLimit
          case toGain of
            Nothing -> pure $ Just $ Upgrade theDeal (Just c) Nothing
            Just gainedCard -> do
              gainCardsToDiscard p [gainedCard]
              pure $ Just $ Upgrade theDeal (Just c) (Just gainedCard)

-- | +1 Card, +1 Action
-- You may discard a Treasure for +2 Money.
millCard :: Card
millCard = Card "Mill" 4 millCardAction Action (simpleVictory 0)
  where
    millCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    millCardAction p = do
      thePlayer <- findPlayer p
      let treasures = filter (\c -> c ^. #cardType == Value) (thePlayer ^. #hand)
      aig <- mkDominionAIGame p
      let toDiscard = (thePlayer ^. #strategy . #discardStrategy) aig (0, 1)
      if null toDiscard
        then basicCardAction 1 0 0 0 p
        else do
          discardCard (head toDiscard) p
          basicCardAction 1 0 0 2 p
      pure $ Just $ Mill toDiscard

-- | +2 Actions
-- Gain a card costing up to $4.
miningVillageCard :: Card
miningVillageCard = Card "Mining Village" 4 miningVillageCardAction Action (simpleVictory 0)
  where
    miningVillageCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    miningVillageCardAction p = do
      _ <- basicCardAction 0 1 0 0 p
      aig <- mkDominionAIGame p
      thePlayer <- findPlayer p
      let mc = (thePlayer ^. #strategy . #gainCardStrategy) aig 4
      case mc of
        Nothing -> pure Nothing
        Just c -> do
          gainCardsToDiscard p [c]
          pure $ Just $ MiningVillage c

-- | +2 Cards
-- Look at the top 2 cards of your deck. Put one into your hand and the other on top of your deck.
secretPassageCard :: Card
secretPassageCard = Card "Secret Passage" 4 secretPassageCardAction Action (simpleVictory 0)
  where
    secretPassageCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    secretPassageCardAction p = do 
      drawn <- deal 2 p
      aig <- mkDominionAIGame p
      thePlayer <- findPlayer p
      let (toDeck, toHand) = (thePlayer ^. #strategy . #secretPassageStrategy) aig (headMay drawn) (lastMay drawn)
      case toDeck of
        Nothing -> pure Nothing
        Just c -> do
          handToDeck p [c]
          pure $ Just $ SecretPassage toHand toDeck
