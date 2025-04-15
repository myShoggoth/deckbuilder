{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}

module DeckBuilding.Dominion.Utils
    ( deal
    , numEmptyDecks
    , firstCardInPlay
    , decreaseCards
    , increaseCards
    , isCardInPlay
    , findPlayer
    , removeFromCards
    , discardCard
    , executeBuys
    , mkDominionAIGame
    , cardPlayed
    ) where

import Control.Lens ( preuse, (^.), use, (%=), (.=), Ixed(ix), (-=), (<>=))
import Control.Monad ( filterM, when, unless )
import Control.Monad.RWS
    ( MonadState(get) )
import Data.Generics.Product ( HasField(field) )
import Data.Generics.Labels ()
import Data.List ( delete, find, group, sort )
import qualified Data.Map as Map
import DeckBuilding.Types ( PlayerNumber(unPlayerNumber) )
import DeckBuilding.Dominion.Types
    ( DominionPlayer (playerName, nativeVillage),
      Card(cardName),
      DominionAIGame(..),
      DominionState,
      DominionBuy(DominionBuy), DominionBoard (embargoes, lastBuys), CardType (Duration) )
import System.Random (split)
import System.Random.Shuffle ( shuffle' )
import DeckBuilding (deal')

-- | Deal n cards, reshuffling the player's deck if needed.
deal :: Int -> PlayerNumber -> DominionState [Card]
deal 0   _    = return []
deal num pnum = do
  p <- findPlayer pnum
  r <- use #random
  let (r', newCards, newDeck, newDiscard) = deal' r (p ^. #deck) (p ^. #discard) num
  #random .= r'
  (#players . ix (unPlayerNumber pnum) . #deck) .= newDeck
  (#players . ix (unPlayerNumber pnum) . #discard) .= newDiscard
  (#players . ix (unPlayerNumber pnum) . #hand) %= (++ newCards)
  return newCards

-- | How many of the game's decks have been emptied?
numEmptyDecks :: DominionState Int
numEmptyDecks = do
  decks' <- use #decks
  return $ length $ Map.filter (== 0) decks'

-- | Move a card from the player's hand to their played pile.
cardPlayed :: Card -> PlayerNumber -> DominionState ()
cardPlayed c p = do
  thePlayer <- findPlayer p
  unless (c ^. #cardType == Duration) $
    (#players . ix (unPlayerNumber p) . #played) <>= [c]
  (#players . ix (unPlayerNumber p) . #hand) .= removeFromCards (thePlayer ^. #hand) [c]

-- | If the cards are the same, return number of cards - 1.
decreaseCards :: Card -> Card -> Int -> Int
decreaseCards  _  _ 0 = 0
decreaseCards c1 c2 n = if c1 == c2
    then n - 1
    else n

-- | If the cards are the same, return the number of cards + the value passed in
increaseCards :: Card -> Int -> Card -> Int -> Int
increaseCards c1 n c2 x = if c1 == c2
    then x + n
    else x

-- | Is this card part of this game, and if so are there any left?
isCardInPlay :: Card -> DominionState Bool
isCardInPlay c = do
  gs <- get
  return $ c `Map.member` (gs ^. #decks) && (gs ^. #decks) Map.! c > 0

-- | Find the first card, if any, in the list which is still in play.
firstCardInPlay :: [Card] -> DominionState (Maybe Card)
firstCardInPlay cs = do
  cardsInPlay <- filterM isCardInPlay cs
  return $ find (const True) $ tail cardsInPlay

-- | Find player # n, error if not found
findPlayer :: PlayerNumber -> DominionState DominionPlayer
findPlayer p = do
  mp <- preuse(#players . ix (unPlayerNumber p))
  case mp of
    Just player' -> pure player'
    Nothing      -> error $ "Unable to find player #" <> show p

-- | Remove this list of cards from that list of cards.
removeFromCards :: [Card] -> [Card] -> [Card]
removeFromCards = foldr delete

-- | Discard a single card, primarily intended for evaluateHand
-- so we can discard Action cards when the player has no actions
-- left.
discardCard :: Card -> PlayerNumber -> DominionState ()
discardCard card p = do
  thePlayer <- findPlayer p
  let newHand = removeFromCards (thePlayer ^. #hand) [card]
  (#players . ix (unPlayerNumber p) . #discard) %= (++ [card])
  (#players . ix (unPlayerNumber p) . #hand) .= newHand

-- | Take n cards from a Supply deck and put them in the
-- player's discard pile.
supplyToDiscard :: Card -> PlayerNumber -> Int -> DominionState ()
supplyToDiscard _ _ 0 = return ()
supplyToDiscard c p n = do
  decks <- use #decks
  if c `Map.notMember` decks || decks Map.! c <= 0
    then return ()
    else do
      #players . ix (unPlayerNumber p) . #discard %= (c:)
      #decks %= Map.mapWithKey (decreaseCards c)
      supplyToDiscard c p (n - 1)

-- | Run the buys the AI requested.
executeBuys :: [DominionBuy] -> DominionAIGame -> DominionState ()
executeBuys [] _ = return ()
executeBuys ((DominionBuy _ c):xs) g = do
  buyCard (playerNum g)
  mkDominionAIGame (g ^. #playerNum) >>= executeBuys xs
  where
    -- | Decrease the amount of the cards in the game deck, subtract the money
    --  from the player, and add the card to the player's discard pile.
    buyCard :: PlayerNumber -> DominionState ()
    buyCard p = do
      thePlayer <- findPlayer p
      when (thePlayer ^. #buys <= 0) $
        error $ "Buy move requested by " <> show p <> " without buys.\n"
      let bridgeCardsPlayed = length $ filter (\card -> cardName card == "Bridge") (thePlayer ^. #played)
      let adjustedCost = max 0 ((c ^. #cost) - bridgeCardsPlayed)
      when (adjustedCost > (thePlayer ^. #money)) $
        error $ "Buy move requested by " <> show p <> " without enough money after Bridge discount.\n"
      ds <- use #decks
      when (ds Map.! c <= 0) $ -- TODO: Couldn't make the lens version see the type instance, why not?
        error $ "Buy move requested by " <> show p <> " with empty deck of " <> show (cardName c) <> ".\n"
      supplyToDiscard c p 1
      (#players . ix (unPlayerNumber p) . #buys) -= 1
      (#players . ix (unPlayerNumber p) . #money) -= adjustedCost
      ems <- use #embargoes
      ep <- use #embargoPenalty
      supplyToDiscard ep p (ems Map.! c)
      #lastBuys %= Map.mapWithKey (addToBuys p c)
    addToBuys :: Eq a1 => a1 -> a2 -> a1 -> [a2] -> [a2]
    addToBuys p c p1 xs = if p == p1 then c : xs else xs

mkDominionAIGame :: PlayerNumber -> DominionState DominionAIGame
mkDominionAIGame pnum = do
  thePlayer <- findPlayer pnum
  decks' <- use #decks
  trash' <- use #trash
  embargoes' <- use #embargoes
  pure DominionAIGame
    { playerNum = pnum
    , hand = thePlayer ^. #hand
    , played = thePlayer ^. #played
    , actions = thePlayer ^. #actions
    , buys = thePlayer ^. #buys
    , money = thePlayer ^. #money
    , turns = thePlayer ^. #turns
    , cards = buildCardMap thePlayer
    , trash = trash'
    , decks = decks'
    , embargoes = embargoes'
    , nativeVillages = thePlayer ^. #nativeVillage
    , pirateShip = thePlayer ^. #pirateShip
    }
  where
    buildCardMap :: DominionPlayer -> Map.Map Card Int
    buildCardMap p = Map.fromList $ map (\x -> (head x, length x)) $ group $ sort allCards
      where
        allCards = (p ^. #hand) <> (p ^. #played) <> (p ^. #discard) <> (p ^. #deck)

