{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module DeckBuilding.Dominion.Cards.Intrigue
    ( baronCard
    , courtyardCard
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
    ) where

import Control.Lens ( (^.), use, (%=), Ixed(ix), prism', (.=) )
import Data.Generics.Product ( HasField(field) )
import Data.List (delete)
import qualified Data.Map as Map
import DeckBuilding.Dominion.Cards.Base
    ( treasureCards, duchyCard, victoryCards, estateCard )
import DeckBuilding.Dominion.Cards.Utils
    ( simpleVictory, basicCardAction, hasActionCards, handToDeck, valueCardAction, trashCards, gainCardsToDiscard )
import DeckBuilding.Types ( PlayerNumber(unPlayerNumber, PlayerNumber), Game (turnOrder), Game(.. ) )
import DeckBuilding.Dominion.Types
    ( Card(Card, cost),
      CardType(Value, Action, Duration), DominionState,
        DominionAction (Courtyard, Lurker, Pawn, ShantyTown, Conspirator, Ironworks, Duke, Harem, Masquerade,
          Steward, Swindler, WishingWell, Baron),
        DominionDraw(DominionDraw), Strategy (trashStrategy, gainCardStrategy), DominionBoard )
import DeckBuilding.Dominion.Utils
    ( decreaseCards, isCardInPlay, findPlayer, mkDominionAIGame, removeFromCards, deal, discardCard )
import Data.Traversable (for)
import Control.Monad (forM)
import Safe (headMay)
import Data.Foldable (for_)
import GHC.Base (VecElem(Int16ElemRep))

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
      discardCard estateCard p
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
          (#players . ix (unPlayerNumber p) . #discard) %= (c:)
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
              | (card ^. #cardType) == Action || (card ^. #cardType == Duration) -> pure $ Just $ Ironworks card (DominionDraw [])
              | card `elem` treasureCards             -> do
                _ <- basicCardAction 0 (-1) 0 1 p
                pure $ Just $ Ironworks card (DominionDraw [])
              | card `elem` victoryCards              -> do
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
