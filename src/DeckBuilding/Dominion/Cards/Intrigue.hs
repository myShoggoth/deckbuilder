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
    , noblesCard
    , patrolCard
    , minionCard
    , torturerCard
    , tradingPostCard
    , replaceCard
    , courtierCard
    ) where

import Control.Lens ( (^.), use, (%=), Ixed(ix), prism', (.=) )
import Data.Generics.Product ( HasField(field) )
import Data.List (delete, partition, union)
import qualified Data.Map as Map
import DeckBuilding.Dominion.Cards.Base
    ( treasureCards, duchyCard, victoryCards, estateCard, curseCard, moatCard, defendsAgainstAttack, silverCard, goldCard )
import DeckBuilding.Dominion.Cards.Utils
    ( simpleVictory, basicCardAction, hasActionCards, handToDeck, valueCardAction, trashCards, gainCardsToDiscard, gainCardsToDeck, discardCards, gainCardsToHand )
import DeckBuilding.Types ( PlayerNumber(unPlayerNumber, PlayerNumber), Game (turnOrder), Game(.. ) )
import DeckBuilding.Dominion.Types
    ( Card(Card, cardName, cost, action, cardType, victoryPoints, numImplicitTypes),
      CardType(Value, Action, Duration, Victory, CurseType),
      DominionState,
        DominionAction (Courtyard, Lurker, Pawn, ShantyTown, Conspirator, Ironworks, Duke, Harem, Masquerade,
          Steward, Swindler, WishingWell, Baron, Bridge, Diplomat, Upgrade, Mill, MiningVillage, SecretPassage,
          Nobles, Patrol, Minion, Torturer, TradingPost, Replace, Courtier),
        DominionDraw(DominionDraw),
        Strategy (trashStrategy, gainCardStrategy, discardStrategy, secretPassageStrategy, courtierRevealStrategy,
          courtierBonusStrategy),
        DominionBoard, CardLocation (..), CourtierChoice (..) )
import DeckBuilding.Dominion.Utils
    ( decreaseCards, isCardInPlay, findPlayer, mkDominionAIGame, removeFromCards, deal, discardCard )
import Data.Traversable (for)
import Control.Monad (forM, when)
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
baronCard       = Card { cardName = "Baron", cost = 4, action = baronCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
courtyardCard   = Card { cardName = "Courtyard", cost = 2, action = courtyardCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
lurkerCard      = Card { cardName = "Lurker", cost = 2, action = lurkerCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
    lurk e@(Right c@(Card _ _ _ Action _ _)) p = do
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
pawnCard        = Card { cardName = "Pawn", cost = 2, action = pawnCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
masqueradeCard  = Card { cardName = "Masquerade", cost = 3, action = masqueradeCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
shantyTownCard  = Card { cardName = "Shanty Town", cost = 3, action = shantyTownCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
stewardCard     = Card { cardName = "Steward", cost = 3, action = stewardCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
swindlerCard =    Card { cardName = "Swindler", cost = 3, action = swindlerCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
-- If you've played 3 or more Actions this turn (counting this), +1 Card and +1 Action.
conspiratorCard :: Card
conspiratorCard = Card { cardName = "Conspirator", cost = 4, action = conspiratorCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
ironworksCard   = Card { cardName = "Ironworks", cost = 4, action = ironworksCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
dukeCard        = Card { cardName = "Duke", cost = 5, action = valueCardAction 0 Duke, cardType = Action, victoryPoints = dukeCardValue, numImplicitTypes = 1 }
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
haremCard       = Card { cardName = "Harem", cost = 6, action = valueCardAction 2 Harem, cardType = Value, victoryPoints = simpleVictory 2, numImplicitTypes = 2 } -- Treasure, Victory

-- | +1 Card
-- +1 Action
--
-- Name a card, then reveal the top card of your deck. If you named it, put it into your hand.
wishingWellCard :: Card
wishingWellCard = Card { cardName = "Wishing Well", cost = 3, action = wishingWellCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
bridgeCard = Card { cardName = "Bridge", cost = 4, action = bridgeCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
diplomatCard = Card { cardName = "Diplomat", cost = 4, action = diplomatCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
upgradeCard = Card { cardName = "Upgrade", cost = 5, action = upgradeCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
millCard = Card { cardName = "Mill", cost = 4, action = millCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
miningVillageCard = Card { cardName = "Mining Village", cost = 4, action = miningVillageCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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
secretPassageCard = Card { cardName = "Secret Passage", cost = 4, action = secretPassageCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
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

-- | Choose one: +3 Cards; or +2 Actions
-- Worth 2 Victory Points
noblesCard :: Card
noblesCard = Card { cardName = "Nobles", cost = 6, action = noblesCardAction, cardType = Action, victoryPoints = simpleVictory 2, numImplicitTypes = 2 } -- Action, Victory
  where
    noblesCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    noblesCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let drawCards = (thePlayer ^. #strategy . #noblesStrategy) aig
      if drawCards
        then do
          theDeal <- basicCardAction 3 (-1) 0 0 p
          pure $ Just $ Nobles theDeal 0
        else do
          theDeal <- basicCardAction 0 1 0 0 p
          pure $ Just $ Nobles theDeal 2

-- | +3 Cards
-- Reveal the top 4 cards of your deck. Put the Victory cards and Curses into your hand. 
-- Put the rest back in any order.
patrolCard :: Card
patrolCard = Card { cardName = "Patrol", cost = 5, action = patrolCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    patrolCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    patrolCardAction p = do
      -- First draw 3 cards
      theDeal <- basicCardAction 3 (-1) 0 0 p
      -- Then look at top 4 cards
      topFour <- deal 4 p
      let (victories, others) = partition isVictoryOrCurse topFour
      -- Add victories to hand
      -- This happens in the call to 'deal' above
      -- Put others back in any order
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let orderedCards = (thePlayer ^. #strategy . #patrolOrderStrategy) aig others
      handToDeck p orderedCards
      pure $ Just $ Patrol theDeal victories orderedCards
    
    isVictoryOrCurse :: Card -> Bool
    isVictoryOrCurse card = card `elem` victoryCards || card == curseCard

-- | +1 Action
--
-- Choose one: +$2; or discard your hand, +4 Cards, and each other player with at least 5 cards in hand discards their hand and draws 4 cards.
minionCard :: Card
minionCard = Card { cardName = "Minion", cost = 5, action = minionCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    minionCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    minionCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let chooseMoney = (thePlayer ^. #strategy . #minionStrategy) aig
      if chooseMoney
        then do
          _ <- basicCardAction 0 0 0 2 p
          pure $ Just $ Minion True (DominionDraw []) Map.empty
        else do
          -- First discard the entire hand
          let handToDiscard = thePlayer ^. #hand
          discardCards p handToDiscard
          
          -- Then draw 4 new cards
          drawn <- deal 4 p
          
          -- Handle other players
          players' <- use #players
          let pns = PlayerNumber <$> [0 .. length players' - 1]
          responses <- forM pns $ \p' -> do
            if p' == p
              then pure (p', Right [])  -- Active player already handled
              else do
                pl <- findPlayer p'
                case defendsAgainstAttack minionCard pl of
                  Just defender -> pure (p', Left defender)
                  Nothing -> do
                    if length (pl ^. #hand) >= 5
                      then do
                        let toDiscard = pl ^. #hand
                        discardCards p' toDiscard
                        drawn' <- deal 4 p'
                        pure (p', Right drawn')
                      else pure (p', Right [])  -- Less than 5 cards, not affected
          
          pure $ Just $ Minion False (DominionDraw drawn) (Map.fromList responses)

-- | +3 Cards
--
-- Each other player chooses one: they discard 2 cards; or they gain a Curse to their hand.
torturerCard :: Card
torturerCard = Card { cardName = "Torturer", cost = 5, action = torturerCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    torturerCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    torturerCardAction p = do
      drawn <- basicCardAction 3 (-1) 0 0 p
      
      -- Handle other players' choices
      players' <- use #players
      let pns = PlayerNumber <$> [0 .. length players' - 1]
      responses <- forM pns $ \p' -> do
        if p' == p
          then pure (p', Right (Right False))  -- Active player not affected
          else do
            pl <- findPlayer p'
            case defendsAgainstAttack torturerCard pl of
              Just defender -> pure (p', Left defender)
              Nothing -> do
                aig <- mkDominionAIGame p'
                let chooseDiscard = (pl ^. #strategy . #torturerStrategy) aig
                if chooseDiscard
                  then do
                    let toDiscard = (pl ^. #strategy . #discardStrategy) aig (2, 2)
                    discardCards p' toDiscard
                    pure (p', Right (Left toDiscard))
                  else do
                    gainCardsToHand p' [curseCard]
                    pure (p', Right (Right False))
      
      pure $ Just $ Torturer drawn (Map.fromList responses)

-- | Trash 2 cards from your hand. If you did, gain a Silver to your hand.
tradingPostCard :: Card
tradingPostCard = Card { cardName = "Trading Post", cost = 5, action = tradingPostCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    tradingPostCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    tradingPostCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let toTrash = (thePlayer ^. #strategy . #trashStrategy) aig (2, 2) (thePlayer ^. #hand)
      if length toTrash == 2
        then do
          trashCards p toTrash
          gained <- gainCardsToHand p [silverCard]
          pure $ Just $ TradingPost toTrash (headMay gained)
        else do
          -- Trash as many as possible if less than 2 available (or chosen by strategy)
          trashCards p toTrash
          pure $ Just $ TradingPost toTrash Nothing

-- | Trash a card from your hand. Gain a card costing up to $2 more than it.
-- If the gained card is an Action or Treasure, put it on top of your deck;
-- if it's a Victory card, gain it to your hand.
replaceCard :: Card
replaceCard = Card { cardName = "Replace", cost = 5, action = replaceCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    replaceCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    replaceCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let toTrashList = (thePlayer ^. #strategy . #trashStrategy) aig (1, 1) (thePlayer ^. #hand)
      case headMay toTrashList of
        Nothing -> pure Nothing -- Cannot replace if no card is trashed
        Just toTrash -> do
          trashCards p [toTrash]
          let costLimit = toTrash ^. #cost + 2
          let mToGain = (thePlayer ^. #strategy . #gainCardStrategy) aig costLimit
          case mToGain of
            Nothing -> pure $ Just $ Replace toTrash undefined Discard -- Need a placeholder or better handling
            Just toGain -> do
              let location = case () of
                               _ | toGain `elem` victoryCards -> Hand
                               _ | toGain ^. #cardType == Action -> Deck
                               _ | toGain ^. #cardType == Value -> Deck -- Value includes Treasure
                               _ | toGain ^. #cardType == Duration -> Deck -- Assuming Durations go to deck
                               _ | toGain ^. #cardType == CurseType -> Hand -- Assuming Curses go to hand too
                               _ -> Discard -- Default fallback, should ideally not be needed
              case location of
                Hand -> gainCardsToHand p [toGain]
                Deck -> gainCardsToDeck p [toGain]
                Discard -> gainCardsToDiscard p [toGain] -- Should not happen based on logic
              pure $ Just $ Replace toTrash toGain location

-- | Reveal a card from your hand. For each type it has (Action, Treasure, Victory, Curse),
-- choose one: +1 Action; or +1 Buy; or +$3; or gain a Gold.
courtierCard :: Card
courtierCard = Card { cardName = "Courtier", cost = 5, action = courtierCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    courtierCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    courtierCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      -- Reveal a card
      let revealedCard = (thePlayer ^. #strategy . #courtierRevealStrategy) aig
      -- Get the number of implicit types for the revealed card
      let numTypes = revealedCard ^. #numImplicitTypes
      -- Get the list of desired bonuses from the strategy
      let chosenBonuses = (thePlayer ^. #strategy . #courtierBonusStrategy) aig revealedCard numTypes

      -- Apply the chosen bonuses
      for_ chosenBonuses $ \mBonus -> do
        case mBonus of
          Just CourtierAction    -> basicCardAction 0 1 0 0 p >> pure ()
          Just CourtierBuy       -> basicCardAction 0 0 1 0 p >> pure ()
          Just CourtierMoney     -> basicCardAction 0 0 0 3 p >> pure ()
          Just CourtierGainGold  -> gainCardsToDiscard p [goldCard] >> pure ()
          Nothing                -> pure () -- No bonus chosen for this slot

      _ <- basicCardAction 0 (-1) 0 0 p -- Consume the base action for playing Courtier
      pure $ Just $ Courtier revealedCard chosenBonuses -- Store revealed card and chosen bonuses
