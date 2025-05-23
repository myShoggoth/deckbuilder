{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE ViewPatterns              #-}

module DeckBuilding.Dominion.Cards.Base
    ( goldCard
    , silverCard
    , copperCard
    , provinceCard
    , duchyCard
    , estateCard
    , curseCard
    , marketCard
    , moatCard
    , smithyCard
    , villageCard
    , festivalCard
    , laboratoryCard
    , cellarCard
    , chapelCard
    , harbingerCard
    , merchantCard
    , vassalCard
    , bureaucratCard
    , gardensCard
    , militiaCard
    , moneylenderCard
    , poacherCard
    , remodelCard
    , throneRoomCard
    , banditCard
    , councilRoomCard
    , witchCard
    , mineCard
    , sentryCard
    , libraryCard
    , artisanCard
    , workshopCard
    , treasureCards
    , victoryCards
    , kingdomCards2ndEdition
    , firstGameKingdomCards
    , baseSetActionTerminators
    , defendsAgainstAttack
    , gainCurse
    ) where

import Control.Lens ( (^.), use, (%=), (.=), Ixed(ix) )
import Control.Monad ( unless )
import Data.Generics.Product ( HasField(field) )
import Data.Generics.Labels ()
import Data.List (delete, find, intersect, (\\), partition)
import qualified Data.Map as Map
import DeckBuilding.Dominion.Cards.Utils
    ( simpleVictory, valueCardAction, basicCardAction, trashCards, discardCards, handToDeck, discardToDeck, gainCardsToDeck, gainCardsToDiscard, gainCardsToHand )
import DeckBuilding.Types ( PlayerNumber(..) )
import DeckBuilding.Dominion.Types
    ( DominionPlayer (playerName, deck, discard, hand, played, actions, buys, money, victory, turns, island, duration, nativeVillage, lighthouse, pirateShip, outpost, gained, strategy),
      Card(Card, cardName, cost, action, cardType, victoryPoints, numImplicitTypes),
      CardType(Action, Value, CurseType),
      DominionState,
      DominionAction(ThroneRoom, Remodel, Vassal, Bandit,
        Cellar, Militia, MoneyLender, Poacher, Chapel,
        Harbinger, Bureaucrat, CouncilRoom, Witch, Mine,
        Library, Sentry, Artisan, Workshop, Gold, Silver,
        Copper, Province, Duchy, Estate, Gardens, Curse,
        Market, Laboratory, Moat, Village, Smithy, Festival,
        Merchant),
      BanditDecision(BanditDecision),
      DominionDraw(DominionDraw) )
import DeckBuilding.Dominion.Utils
    ( deal, numEmptyDecks, decreaseCards, firstCardInPlay, findPlayer, mkDominionAIGame, removeFromCards, cardPlayed, isCardInPlay )
import System.Random.Shuffle ( shuffle' )
import Safe (headMay)
import Data.Either (isRight, fromRight)
import Control.Conditional (whenM)

-- Cards and their actions

-- | $3
goldCard :: Card
goldCard = Card { cardName = "Gold", cost = 6, action = valueCardAction 3 Gold, cardType = Value, victoryPoints = simpleVictory 0, numImplicitTypes = 1 } -- Treasure

-- | $2
silverCard :: Card
silverCard = Card { cardName = "Silver", cost = 3, action = silverCardAction, cardType = Value, victoryPoints = simpleVictory 0, numImplicitTypes = 1 } -- Treasure
  where
    -- Silver cards need extra logic to make Merchant work in all cases.
    silverCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    silverCardAction p    = do
        thePlayer <- findPlayer p
        let monies = if merchantCard `elem` (thePlayer ^. #played) && silverCard `notElem` (thePlayer ^. #played)
                        then 3
                        else 2
        valueCardAction monies Silver p

-- | $1
copperCard :: Card
copperCard = Card { cardName = "Copper", cost = 0, action = valueCardAction 1 Copper, cardType = Value, victoryPoints = simpleVictory 0, numImplicitTypes = 1 } -- Treasure

-- | Convience 'List' of treasure 'Card's.
treasureCards :: [Card]
treasureCards = [goldCard, silverCard, copperCard]

-- | 6VP
provinceCard :: Card
provinceCard = Card { cardName = "Province", cost = 8, action = valueCardAction 0 Province, cardType = Value, victoryPoints = simpleVictory 6, numImplicitTypes = 1 } -- Victory

-- | 3VP
duchyCard :: Card
duchyCard = Card { cardName = "Duchy", cost = 5, action = valueCardAction 0 Duchy, cardType = Value, victoryPoints = simpleVictory 3, numImplicitTypes = 1 } -- Victory

-- | 1VP
estateCard :: Card
estateCard = Card { cardName = "Estate", cost = 2, action = valueCardAction 0 Estate, cardType = Value, victoryPoints = simpleVictory 1, numImplicitTypes = 1 } -- Victory

-- | -1VP
curseCard :: Card
curseCard = Card { cardName = "Curse", cost = 0, action = valueCardAction 0 Curse, cardType = CurseType, victoryPoints = simpleVictory (-1), numImplicitTypes = 1 } -- Curse

-- | Convience 'List' of 'Card's that affect victory values.
victoryCards :: [Card]
victoryCards = [curseCard, estateCard, duchyCard, gardensCard, provinceCard]

-- | +1 Card
--
-- +1 Action
--
-- +1 Buy
--
-- +$1
marketCard :: Card
marketCard = Card { cardName = "Market", cost = 5, action = marketCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 } :: Card
  where
    marketCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    marketCardAction p = do
      theDraw <- basicCardAction 1 0 1 1 p
      pure $ Just $ Market theDraw

-- | +2 Cards
--
-- When another player plays an Attack card, you may first reveal this from your hand, to be unaffected by it.
moatCard :: Card
moatCard = Card { cardName = "Moat", cost = 2, action = moatCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    moatCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    moatCardAction p = do
      theDraw <- basicCardAction 2 (-1) 0 0 p
      pure $ Just $ Moat theDraw

-- | +3 Cards
smithyCard :: Card
smithyCard = Card { cardName = "Smithy", cost = 4, action = smithyCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    smithyCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    smithyCardAction p = do
      theDraw <- basicCardAction 3 (-1) 0 0 p
      pure $ Just $ Smithy theDraw

-- | +1 Card
--
-- +2 Actions
villageCard :: Card
villageCard = Card { cardName = "Village", cost = 3, action = villageCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    villageCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    villageCardAction p = do
      theDraw <- basicCardAction 1 1 0 0 p
      pure $ Just $ Village theDraw

-- | +2 Actions
--
-- +1 Buy
--
-- +$2
festivalCard :: Card
festivalCard = Card { cardName = "Festival", cost = 5, action = festivalCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    festivalCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    festivalCardAction p = do
      _ <- basicCardAction 0 1 1 2 p
      pure $ Just Festival

-- | +2 Cards
--
-- +1 Action
laboratoryCard :: Card
laboratoryCard = Card { cardName = "Laboratory", cost = 5, action = marketCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    marketCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    marketCardAction p = do
      theDraw <- basicCardAction 2 0 0 0 p
      pure $ Just $ Laboratory theDraw

-- | +1 Action
--
-- Discard any number of cards, then draw that many.
cellarCard :: Card
cellarCard = Card { cardName = "Cellar", cost = 2, action = cellarCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    cellarCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    cellarCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let discards = (thePlayer ^. #strategy . #discardStrategy) aig (0, length (thePlayer ^. #hand))
      -- TODO: Verify the cards are valid to discard
      --  1. Are the number of cards being discarded within the bounds?
      --  2. Are all of the cards being discarded actually in that player's hand?
      discardCards p discards
      theDraw <- deal (length discards) p
      pure $ Just $ Cellar discards $ DominionDraw theDraw

-- | Trash up to 4 cards from your hand.
chapelCard :: Card
chapelCard = Card { cardName = "Chapel", cost = 2, action = chapelCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    chapelCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    chapelCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      -- Remove the chapel from the hand so the AI doesn't decide it can trash it as one of the four
      #players . ix (unPlayerNumber p) . #hand .= removeFromCards (thePlayer ^. #hand) [chapelCard]
      let toTrash = (thePlayer ^. #strategy . #trashStrategy) aig (0, 4) []
      -- then restore, `thePlayer` in this case has the old values anyway
      #players . ix (unPlayerNumber p) . #hand .= thePlayer ^. #hand
      trashCards p toTrash
      _ <- basicCardAction 0 (-1) 0 0 p
      pure $ Just $ Chapel toTrash

-- | +1 Card
--
-- +1 Action
--
-- Look through your discard pile. You may put a card from it onto your deck.
harbingerCard :: Card
harbingerCard = Card { cardName = "Harbinger", cost = 4, action = harbingerCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    harbingerCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    harbingerCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      theDraw <- basicCardAction 1 0 0 0 p
      let cards = (thePlayer ^. #strategy . #retrieveStrategy) aig (0, 1) (thePlayer ^. #discard)
      discardToDeck p cards
      pure $ Just $ Harbinger theDraw $ headMay cards

-- | +1 Card
--
-- +1 Action
--
-- The first time you play a Silver this turn, +$1.
merchantCard :: Card
merchantCard = Card { cardName = "Merchant", cost = 3, action = merchantCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    merchantCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    merchantCardAction p = do
      thePlayer <- findPlayer p
      let silverPlayed = silverCard `elem` thePlayer ^. #played
      theDraw <- basicCardAction 1 0 0 (monies silverPlayed) p
      pure $ Just $ Merchant theDraw
      where monies True  = 1
            monies False = 0

-- | +$2
--
-- Discard the top card of your deck. If it's an Action card, you may play it.
vassalCard :: Card
vassalCard = Card { cardName = "Vassal", cost = 3, action = vassalCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    vassalCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    vassalCardAction p = do
      thePlayer <- findPlayer p
      r <- use #random
      _ <- basicCardAction 0 (-1) 0 2 p
      let (enoughDeck, _newDiscard)
            | not (null (thePlayer ^. #deck)) = (thePlayer ^. #deck, thePlayer ^. #discard)
            | otherwise                       = ( thePlayer ^. #deck ++ shuffle' (thePlayer ^. #discard) (length (thePlayer ^. #discard)) r, [])
      let topOfDeck Nothing                 = return Nothing
          topOfDeck (Just c')               = do
            _ <- deal 1 p
            if c' ^. #cardType == Action
              then do
                mdm <- (c' ^. #action) p
                cardPlayed c' p
                pure $ Just $ Vassal mdm
              else do
                #players . ix (unPlayerNumber p) . #discard %= (c':)
                pure $ Just $ Vassal Nothing
      topOfDeck $ find (const True) enoughDeck

-- TODO: This needs to move to Utils and handle cards beyond Moat.
defendsAgainstAttack :: Card -> DominionPlayer -> Maybe Card
defendsAgainstAttack _ p =
  if moatCard `elem` p ^. #hand || p ^. #lighthouse > 0
    then Just moatCard
    else Nothing

-- | Gain a Silver onto your deck. Each other player reveals a Victory
-- card from their hand and puts it onto their deck (or reveals a hand
-- with no Victory cards).
bureaucratCard :: Card
bureaucratCard = Card { cardName = "Bureaucrat", cost = 4, action = bureaucratCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    bureaucratCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    bureaucratCardAction p = do
      gainCardsToDeck p [silverCard]
      players' <- use #players
      discards <- mapM (discardVictory p) $ PlayerNumber <$> [0.. length players' - 1]
      #decks %= Map.mapWithKey (decreaseCards silverCard)
      _ <- basicCardAction 0 (-1) 0 0 p
      pure $ Just $ Bureaucrat $ Map.fromList $ zip (PlayerNumber <$> [0.. length players' - 1]) discards
    discardVictory :: PlayerNumber -> PlayerNumber -> DominionState (Either Card (Maybe Card))
    discardVictory e p | p == e = return $ Right Nothing
    discardVictory _ p = do
      thePlayer <- findPlayer p
      case defendsAgainstAttack bureaucratCard thePlayer of
        Just defender -> return $ Left defender
        Nothing       -> do
          case find (`elem` victoryCards) (thePlayer ^. #hand) of
            Nothing -> return $ Right Nothing
            Just c  -> do
              discardCards p [c]
              return $ Right $ Just c

-- | Worth 1VP per 10 cards you have (round down).
gardensCard :: Card
gardensCard = Card { cardName = "Gardens", cost = 4, action = valueCardAction 0 Gardens, cardType = Value, victoryPoints = gardensCardVictory, numImplicitTypes = 1 }
  where
    gardensCardVictory :: PlayerNumber -> DominionState Int
    gardensCardVictory p = do
      thePlayer <- findPlayer p
      let points = length ( thePlayer ^. #hand ++ thePlayer ^. #discard ++ thePlayer ^. #played ++ thePlayer ^. #deck ) `div` 10
      pure points

-- | +$2
--
-- Each other player discards down to 3 cards in hand.
militiaCard :: Card
militiaCard = Card { cardName = "Militia", cost = 4, action = militiaCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    militiaCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    militiaCardAction p = do
      players' <- use #players
      _ <- basicCardAction 0 (-1) 0 2 p
      playerResponses <- mapM (militiaDiscard p) $ PlayerNumber <$> [0.. length players' - 1]
      return $ Just $ Militia $ Map.fromList playerResponses
    militiaDiscard :: PlayerNumber -> PlayerNumber -> DominionState (PlayerNumber, Either Card [Card])
    militiaDiscard e p | p == e = return (e, Right [])
    militiaDiscard _ p = do
      thePlayer <- findPlayer p
      case defendsAgainstAttack militiaCard thePlayer of
        Just defender -> return (p, Left defender)
        Nothing       -> do
          aig <- mkDominionAIGame p
          let discards = (thePlayer ^. #strategy . #discardStrategy) aig ( length (thePlayer ^. #hand) - 3, length (thePlayer ^. #hand) - 3 )
          discardCards p discards
          return (p, Right discards)

-- | You may trash a Copper from your hand for +$3.	
moneylenderCard :: Card
moneylenderCard = Card { cardName = "Moneylender", cost = 4, action = moneylenderCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    moneylenderCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    moneylenderCardAction p = do
      thePlayer <- findPlayer p
      if copperCard `elem` thePlayer ^. #hand
        then do
          trashCards p [copperCard]
          _ <- basicCardAction 0 (-1) 0 3 p
          return $ Just MoneyLender
        else return Nothing

-- | +1 Card
--
-- +1 Action
--
-- +$1
--
-- Discard a card per empty Supply pile.
poacherCard :: Card
poacherCard = Card { cardName = "Poacher", cost = 4, action = poacherCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    poacherCardAction ::  PlayerNumber -> DominionState (Maybe DominionAction)
    poacherCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      theDraw <- basicCardAction 1 0 0 1 p
      emptyDecks <- numEmptyDecks
      let discards = (thePlayer ^. #strategy . #discardStrategy) aig (emptyDecks, emptyDecks)
      discardCards p discards
      return $ Just $ Poacher theDraw discards

-- | Trash a card from your hand. Gain a card costing up to $2 more than it.
remodelCard :: Card
remodelCard = Card { cardName = "Remodel", cost = 4, action = remodelCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    remodelCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    remodelCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let moves = (thePlayer ^. #strategy . #trashStrategy) aig (0, 1) []
      if length moves == 1
        then do
          trashCards p moves
          let newCard = (thePlayer ^. #strategy . #gainCardStrategy) aig (head moves ^. #cost + 2)
          case newCard of
            Nothing -> pure Nothing
            Just card -> do
              _ <- basicCardAction 0 (-1) 0 0 p
              gainCardsToDiscard p [card]
              pure $ Just $ Remodel (head moves) card
        else
          return Nothing

-- | You may play an Action card from your hand twice.
throneRoomCard :: Card
throneRoomCard = Card { cardName = "Throne Room", cost = 4, action = throneRoomCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    throneRoomCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    throneRoomCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let mc = (thePlayer ^. #strategy . #throneRoomStrategy) aig
      case mc of
        Nothing     -> return Nothing
        (Just card) -> do
          mm1 <- (card ^. #action) p
          case mm1 of
            Nothing -> return Nothing -- Maybe we should error out, here?
            Just m1 -> do
              cardPlayed card p -- If we're able to do this once, it has been played.
              -- This is a little subtle. You don't use up actions when doing
              -- the throne roomed card, just the thrown room. So we run the action
              -- twice (which will use up two actions), and then give one back, so
              -- we end up having use a single action for the throne room.
              _ <- basicCardAction 0 1 0 0 p
              mm2 <- (card ^. #action) p
              case mm2 of
                Nothing -> return Nothing
                Just m2 ->
                  return $ Just $ ThroneRoom card m1 m2

-- | Gain a Gold. Each other player reveals the top 2 cards of their deck,
-- trashes a revealed Treasure other than Copper, and discards the rest.
banditCard :: Card
banditCard = Card { cardName = "Bandit", cost = 5, action = banditCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    banditCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    banditCardAction p = do
      ps <- use #players
      decisions <- mapM (banditDiscard p) $ PlayerNumber <$> [0.. length ps - 1]
      gainCardsToDiscard p [goldCard]
      #decks %= Map.mapWithKey (decreaseCards goldCard)
      _ <- basicCardAction 0 (-1) 0 0 p
      let oppDecisions = filter (\(_, x) -> isRight x) decisions
          fixedOppDecisions :: [(PlayerNumber, Either Card BanditDecision)] = fixup <$> oppDecisions
          fixup :: (PlayerNumber, Either () (Either Card BanditDecision)) -> (PlayerNumber, Either Card BanditDecision)
          fixup (p', x) = (p', fromRight (Left moatCard) x)
      pure $ Just $ Bandit $ Map.fromList fixedOppDecisions
    banditDiscard :: PlayerNumber -> PlayerNumber -> DominionState (PlayerNumber, Either () (Either Card BanditDecision))
    banditDiscard e p | p == e = pure (e, Left ())
    banditDiscard e p = do
      thePlayer <- findPlayer p
      case defendsAgainstAttack militiaCard thePlayer of
        Just defender -> return (e, Right $ Left defender)
        Nothing       -> do
          toptwo <- deal 2 p
          let totrash   = take 1 $ intersect toptwo (delete copperCard (reverse treasureCards))
          let todiscard = toptwo \\ totrash
          trashCards p totrash
          discardCards p todiscard
          pure (e, Right $ Right $ BanditDecision (headMay totrash) todiscard)

-- | +4 Cards
--
-- +1 Buy
--
-- Each other player draws a card.
councilRoomCard :: Card
councilRoomCard = Card { cardName = "Council Room", cost = 5, action = councilRoomCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    councilRoomCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    councilRoomCardAction p = do
      ps <- use #players
      draws <- mapM (councilRoomDraw p) $ PlayerNumber <$> [0.. length ps - 1]
      theDeal <- basicCardAction 4 (-1) 0 0 p
      pure $ Just $ CouncilRoom theDeal $ Map.fromList draws
    councilRoomDraw :: PlayerNumber -> PlayerNumber -> DominionState (PlayerNumber, Maybe Card)
    councilRoomDraw e p | p == e = return (p, Nothing)
    councilRoomDraw _ p = do
      cs <- deal 1 p
      return (p, headMay cs)

-- | +2 Cards
--
-- Each other player gains a Curse.
witchCard :: Card
witchCard = Card { cardName = "Witch", cost = 5, action = witchCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    witchCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    witchCardAction p = do
      ps <- use #players
      curses <- mapM (witchCurse p) $ PlayerNumber <$> [0.. length ps - 1]
      drawn <- basicCardAction 2 (-1) 0 0 p
      pure $ Just $ Witch drawn $ Map.fromList curses
    witchCurse :: PlayerNumber -> PlayerNumber -> DominionState (PlayerNumber, Either Card (Maybe Card))
    -- TODO: Model the player playing the witch ala Bandit
    witchCurse e p | p == e = return (p, Right Nothing)
    witchCurse _ p = do
      thePlayer <- findPlayer p
      case defendsAgainstAttack witchCard thePlayer of
        Just defender -> return (p, Left defender)
        Nothing -> do
          decks <- use #decks
          if curseCard `Map.notMember` decks || decks Map.! curseCard <= 0
            then return (p, Right Nothing)
            else do
              mc <- gainCurse p
              return (p, Right mc)

gainCurse :: PlayerNumber -> DominionState (Maybe Card)
gainCurse p = do
  haveCurses <- isCardInPlay curseCard
  if haveCurses
    then do
      gainCardsToDiscard p [curseCard]
      #decks %= Map.mapWithKey (decreaseCards curseCard)
      return $ Just curseCard
    else return Nothing

-- | You may trash a Treasure card from your hand. Gain a Treasure
-- card to your hand costing up to $3 more than it.
mineCard :: Card
mineCard = Card { cardName = "Mine", cost = 5, action = mineCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    mineCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    mineCardAction p = do
      thePlayer <- findPlayer p
      mc <- firstCardInPlay $ intersect (thePlayer ^. #hand) $ tail treasureCards
      case mc of
        Nothing -> return Nothing
        (Just card)
            | card == copperCard  -> exch copperCard silverCard p
            | card == silverCard  -> exch silverCard goldCard p
            | otherwise           -> return Nothing
    exch :: Card -> Card -> PlayerNumber -> DominionState (Maybe DominionAction)
    exch c1 c2 p = do
      trashCards p [c1]
      gainCardsToHand p [c2]
      _ <- basicCardAction 0 (-1) 0 0 p
      return $ Just $ Mine c1 c2

-- | Draw until you have 7 cards in hand, skipping any Action
-- cards you choose to; set those aside, discarding them afterwards.
libraryCard :: Card
libraryCard = Card { cardName = "Library", cost = 5, action = libraryCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    libraryCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    libraryCardAction p = do
      (keeps, discards) <- drawTo 7 p ([], [])
      _ <- basicCardAction 0 (-1) 0 0 p
      return $ Just $ Library keeps discards
    drawTo :: Int -> PlayerNumber -> ([Card], [Card]) -> DominionState ([Card], [Card])
    drawTo num p (draws, discards)= do
      thePlayer <- findPlayer p
      let todraw = num - (length draws + length (thePlayer ^. #hand))
      if todraw <= 0
        then return (draws, discards)
        else do
          newcards <- deal todraw p
          if null newcards
            then return (draws, discards)
            else do
              decisions <- mapM (discardOrPlay p) newcards
              let (map fst -> keeps, map fst -> dontkeeps) = partition snd decisions
              (newDraws, newDiscards) <- drawTo num p (draws ++ keeps, discards ++ dontkeeps)
              return (newDraws, newDiscards)
    discardOrPlay :: PlayerNumber -> Card -> DominionState (Card, Bool)
    discardOrPlay p c = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let keep = (thePlayer ^. #strategy . #libraryStrategy) aig c
      unless (c ^. #cardType == Value || keep) $
        discardCards p [c]
      return (c, c ^. #cardType == Value || keep )

-- | +1 Card
--
-- +1 Action
--
-- Look at the top 2 cards of your deck. Trash and/or discard any number
-- of them. Put the rest back on top in any order.
sentryCard :: Card
sentryCard = Card { cardName = "Sentry", cost = 5, action = sentryCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    sentryCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    sentryCardAction p = do
      drawn <- basicCardAction 1 0 0 0 p
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let oldhand = thePlayer ^. #hand
      newcards <- deal 2 p
      let (trashem, disc, keep) = (thePlayer ^. #strategy . #sentryStrategy) aig newcards
      #trash %= (trashem ++)
      #players . ix (unPlayerNumber p) . #discard %= (disc ++)
      #players . ix (unPlayerNumber p) . #deck %= (keep ++)
      #players . ix (unPlayerNumber p) . #hand .= oldhand
      return $ Just $ Sentry drawn trashem disc keep

-- | Gain a card to your hand costing up to $5.
-- Put a card from your hand onto your deck.
artisanCard :: Card
artisanCard = Card { cardName = "Artisan", cost = 6, action = artisanCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    artisanCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    artisanCardAction p = do
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let mc = (thePlayer ^. #strategy . #gainCardStrategy) aig 5
      case mc of
        Nothing   -> return Nothing
        Just card -> do
          _ <- basicCardAction 0 (-1) 0 0 p
          #decks %= Map.mapWithKey (decreaseCards card)
          #players . ix (unPlayerNumber p) . #hand %= (card:)
          #players . ix (unPlayerNumber p) . #gained %= (card:)
          aig' <- mkDominionAIGame p
          let putOnDeck = (thePlayer ^. #strategy . #handToDeckStrategy) aig' 1
          if length putOnDeck == 1
            then handToDeck p putOnDeck
            else pure ()
          return $ Just $ Artisan card (head putOnDeck)

-- | Gain a card costing up to $4.
workshopCard :: Card
workshopCard = Card { cardName = "Workshop", cost = 3, action = workshopCardAction, cardType = Action, victoryPoints = simpleVictory 0, numImplicitTypes = 1 }
  where
    workshopCardAction :: PlayerNumber -> DominionState (Maybe DominionAction)
    workshopCardAction p = do
      _ <- basicCardAction 0 (-1) 0 0 p
      thePlayer <- findPlayer p
      aig <- mkDominionAIGame p
      let mc = (thePlayer ^. #strategy . #gainCardStrategy) aig 4
      case mc of
        Nothing -> return Nothing
        Just card -> do
          #decks %= Map.mapWithKey (decreaseCards card)
          #players . ix (unPlayerNumber p) . #hand %= (card:)
          #players . ix (unPlayerNumber p) . #gained %= (card:)
          return $ Just $ Workshop card

-- | The kingdom cards from Dominion 2nd edition.
kingdomCards2ndEdition :: [Card]
kingdomCards2ndEdition = [
    marketCard
  , moatCard
  , smithyCard
  , villageCard
  , festivalCard
  , laboratoryCard
  , cellarCard
  , chapelCard
  , harbingerCard
  , merchantCard
  , vassalCard
  , bureaucratCard
  , gardensCard
  , militiaCard
  , moneylenderCard
  , poacherCard
  , remodelCard
  , throneRoomCard
  , banditCard
  , councilRoomCard
  , witchCard
  , mineCard
  , sentryCard
  , libraryCard
  , artisanCard
  ]

-- | The ten kingdom cards recommended for a player's first game.
firstGameKingdomCards :: [Card]
firstGameKingdomCards = [
    cellarCard
  , marketCard
  , merchantCard
  , militiaCard
  , mineCard
  , moatCard
  , remodelCard
  , smithyCard
  , villageCard
  , workshopCard
  ]

baseSetActionTerminators :: [Card]
baseSetActionTerminators = [
    chapelCard
  , moatCard
  , vassalCard
  , workshopCard
  , bureaucratCard
  , militiaCard
  , moneylenderCard
  , remodelCard
  , smithyCard
  , throneRoomCard
  , banditCard
  , councilRoomCard
  , libraryCard
  , mineCard
  , witchCard
  , artisanCard
  ]
