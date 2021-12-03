{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module DeckBuilding.Dominion.Types
    ( module DeckBuilding.Dominion.Types
    ) where

import Control.Lens.At (Index)
import Control.Monad.State.Lazy (State)
import qualified Data.Map as Map
import qualified Data.Semigroup as Semi
import Data.Text ( unpack, Text )
import GHC.Generics ( Generic )
import System.Random ( StdGen, mkStdGen )
import DeckBuilding.Types ( PlayerNumber )
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..), Arbitrary (arbitrary))
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Instances ()

data DominionGame = DominionGame
  { players :: [(Text, Strategy)]
  , kingdoms :: [Card]
  , seed :: StdGen
  , turns :: [DominionTurn]
  -- | [(Player Name, Score)]
  , result :: [(Text, Int)]
  }
  deriving stock (Generic, Show)
  deriving Arbitrary via GenericArbitrary DominionGame

instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> arbitrary

newtype DominionTurn = DominionTurn [DominionPlayerTurn]
  deriving stock (Show, Generic)
  deriving Arbitrary via GenericArbitrary DominionTurn

data DominionPlayerTurn = DominionPlayerTurn
  { playerNumber :: PlayerNumber
  , turnNumber :: Int
  , buys :: [DominionBuy]
  , actions :: [DominionAction]
  , draws :: DominionDraw
  }
  deriving stock (Show, Generic)
  deriving Arbitrary via GenericArbitrary DominionPlayerTurn

data DominionBuy = DominionBuy Int Card
  deriving stock (Show, Generic)
  deriving Arbitrary via GenericArbitrary DominionBuy

data DominionAction =
      Copper | Silver | Gold | Harem |
      Curse | Estate | Duchy | Province | Gardens | Duke |
      Ambassador [Card] (Map.Map PlayerNumber (Either Card (Maybe Card))) |
      Artisan Card Card |
      Bandit (Map.Map PlayerNumber (Either Card BanditDecision)) |
      Bazaar DominionDraw |
      Bureaucrat (Map.Map PlayerNumber (Either Card (Maybe Card))) |
      Caravan DominionDraw |
      CaravanDuration DominionDraw |
      Chapel [Card] |
      Cellar [Card] DominionDraw |
      Conspirator DominionDraw |
      CouncilRoom DominionDraw (Map.Map PlayerNumber (Maybe Card)) |
      Courtyard DominionDraw [Card] |
      Cutpurse (Map.Map PlayerNumber (Either Card (Maybe Card))) |
      Embargo Card |
      Explorer Card |
      Festival |
      FishingVillage |
      FishingVillageDuration |
      GhostShip DominionDraw (Map.Map PlayerNumber (Either Card [Card])) |
      Harbinger DominionDraw (Maybe Card) |
      Haven DominionDraw Card |
      HavenDuration Card |
      Island (Maybe Card) |
      Ironworks Card DominionDraw |
      Remodel Card Card |
      Laboratory DominionDraw |
      Library [Card] [Card] |
      Lighthouse |
      LighthouseDuration |
      Lookout Card Card Card |
      Lurker (Either Card Card) |
      Market DominionDraw |
      Merchant DominionDraw |
      Militia (Map.Map PlayerNumber (Either Card [Card])) |
      Mine Card Card |
      Moat DominionDraw |
      MoneyLender |
      NativeVillage (Either Card [Card]) |
      Navigator [Card] |
      PearlDiver DominionDraw Card Bool |
      PirateShip (Either Int (Map.Map PlayerNumber (Either Card (Maybe Card)))) |
      Poacher DominionDraw [Card] |
      Salvager Card |
      SeaHag (Map.Map PlayerNumber (Either Card (Maybe Card, Maybe Card))) |
      Sentry DominionDraw [Card] [Card] [Card] |
      ShantyTown DominionDraw [Card] |
      Smithy DominionDraw |
      ThroneRoom Card DominionAction DominionAction |
      TreasureMap [Card] |
      Vassal (Maybe DominionAction) |
      Village DominionDraw |
      Witch DominionDraw (Map.Map PlayerNumber (Either Card (Maybe Card))) |
      Warehouse DominionDraw [Card] |
      Workshop Card
  deriving stock (Show, Generic)
  deriving Arbitrary via GenericArbitrary DominionAction

newtype DominionDraw = DominionDraw [Card]
  deriving stock (Show, Generic)
  deriving Arbitrary via GenericArbitrary DominionDraw

type DominionState a = State DominionBoard a

data DominionConfig = DominionConfig {
  -- | Names and strategies for each player
  playerDefs   :: [(Text, Strategy)],
  -- | Which kingdom cards to use
  kingdomCards :: [Card]
} deriving stock (Show, Generic)

instance Semi.Semigroup DominionConfig where
  c1 <> c2 = DominionConfig
              (playerDefs c1 ++ playerDefs c2)
              (kingdomCards c1 ++ kingdomCards c2)

instance Monoid DominionConfig where
  mempty = DominionConfig [] []
  mappend = (Semi.<>)

-- | Represents the state of a single game of Dominion.
data DominionBoard = DominionBoard {
  -- | The players of the game.
  players :: [DominionPlayer],
  -- | All the decks, basic and Kingdom: (Card, Number Left)
  decks   :: Map.Map Card Int,
  -- | The trash pile.
  trash   :: [Card],
  -- | Embargo tiles (Seaside expansion)
  embargoes  :: Map.Map Card Int,

  -- I'm putting these here to avoid cyclic dependencies, I would like a better solution.
  defenders :: [Card],
  embargoPenalty :: Card,

  -- | The current random number generator, needs to be updated when used.
  random  :: StdGen
} deriving stock (Generic)

-- | The redacted state of the game for use by 'Strategy' functions.
data DominionAIGame = DominionAIGame {
  -- | Which player this is
  playerNum  :: PlayerNumber,
  -- | Current hand
  hand       :: [Card],
  -- | Cards that have been played this turn.
  played     :: [Card],
  -- | Number of actions remaining.
  actions    :: Int,
  -- | Number of buys remaining.
  buys       :: Int,
  -- | Amount of money available to spend on cards.
  money      :: Int,
  -- | How many turns has this player completed?
  turns      :: Int,
  -- | Number of each kind of card the player has
  cards      :: Map.Map Card Int,
  -- | The trash pile.
  trash      :: [Card],
  -- | All the decks, basic and Kingdom: (Card, Number Left)
  decks      :: Map.Map Card Int,
  -- | Embargo tiles (Seaside expansion)
  embargoes  :: Map.Map Card Int,
  -- | Contents of the Native Village mat (Seaside expansion)
  nativeVillages :: [Card],
  -- | Number of Coin tokens on the Pirate Ship mat
  pirateShip :: Int
} deriving stock (Show, Generic)

-- | The three 'CardType's are
-- * 'Value' - The 'Card' does not require an action to play,
-- and normally givens money or victory points (not useful
-- until scoring).
-- * 'Action' - Does require an action to play, each type of action
-- 'Card' has its own logic.
-- * 'Duration' - For Action - Duration cards, the Action function
-- is called when initially played, and the Duration function is
-- called at the beginning of the next turn.
data CardType = Value | Action | Duration
  deriving stock (Show, Eq, Generic)
  deriving Arbitrary via GenericArbitrary CardType

-- | A Dominion card, basic supply, kingdom, or expansion.
data Card = Card {
  -- | Name of the card, like Copper or Market. Used mostly for debugging.
  cardName :: Text,
  -- | Money cost of the card.
  cost     :: Int,
  {-|
    The function that changes that game state based on the card. This is the
    core of the whole engine.

    'Card': The card being played.
    Int: The number of the player that is playing the card.

    Updates the game state based on what the card does, then returns the
    player number.
  -}
  action   :: PlayerNumber -> DominionState (Maybe DominionAction),
  -- | Value or Action
  cardType :: CardType,
  -- | The function that determines the score for the card
  -- at the end of the game
  score    :: PlayerNumber -> DominionState Int
} deriving stock (Generic)

instance Arbitrary Card where
  arbitrary = do
    cst <- arbitrary
    ct <- arbitrary
    return $ Card {
    cardName = "Arbitrary Card",
    cost = cst,
    action = \_ -> return Nothing,
    cardType = ct,
    score = \_ -> return 0
  }

type instance Index Card = Card

instance Ord Card where
  compare c1 c2 = compare (cardName c1) (cardName c2)

instance Eq Card where
  a == b = cardName a == cardName b

instance Show Card where
  show c = unpack $ cardName c

-- | What poor choices have I made that led me to this?
instance Semi.Semigroup Card where
  c1 <> c2 = c1

{-|
  The playing strategy used by the player. A list of functions that are
  called at different times in the game for the player to make a decision.

  To create a new strategy, implement each of the functions (or use one of
  the basic ones if that's good enough) and create a 'Strategy'. Then pass it
  when creating a 'DominionPlayer' and see how it does in the game.

  Because these are done in the context of the State Monad, the strategy
  can see the entire game state, including stuff real players wouldn't be
  able to know. Don't use that stuff.

  See DeckBuilding.Dominion.Strategies.Basic for very simple versions.
-}
data Strategy = Strategy {
  -- | Friendly name for the strategy, mostly used for debugging.
  strategyName       :: Text,
  -- | Called when it is time for the player to buy new cards. The strategy
  --  is responsible for lowering the money, adding the cards to the discard
  --  pile, etc.
  buyStrategy        :: DominionAIGame -> [DominionBuy],
  -- | When a card action has the player discard, this function is called.
  --  (min, max) are the minimum number of cards the player has to discard,
  --  and the maximum they are allowed to.
  discardStrategy    :: DominionAIGame -> (Int, Int) -> [Card],
  -- | The player is being prompted to trash zero or more cards from either
  -- their hand only, or the discard pile and the hand. (min, max) are the 
  -- minimum and maximum numbers of cards that may be trashed, and the
  -- discard pile is passed in if applicable.
  -- Return value is a tuple of the card trashing from the hand and the discard
  -- pile, respectively.
  trashStrategy      :: DominionAIGame -> (Int, Int) -> [Card] -> [Card],
  -- | Like discardStrategy, except for retrieving cards from the player's
  --  discard pile.
  retrieveStrategy   :: DominionAIGame -> (Int, Int) -> [Card] -> [Card],
  -- | Called before the hand is evaluated, lets the strategy determine
  --  which order they want the cards played in.
  nextCard           :: PlayerNumber -> DominionState (Maybe Card),
  -- | When a card lets the player gain a card up to cost n into their discard
  --  pile, this is called.
  gainCardStrategy   :: DominionAIGame -> Int -> Maybe Card,
  -- | Specifically for the Throne Room card, lets the strategy pick which
  --  card (Just Card) to play twice, or none if Nothing. Pick a card remaining
  --  in the player's hand.
  throneRoomStrategy :: DominionAIGame -> Maybe Card,
  -- | For the Library card, called when the player draws an action and returns
  --  whether or not the player wants to skip that card.
  libraryStrategy    :: DominionAIGame -> Card -> Bool,
  -- | For the Sentry card, gives the top two cards of the player's deck, then
  --  says which ones that player wants to (trash, discard, keep).
  sentryStrategy     :: DominionAIGame -> [Card] -> ([Card], [Card], [Card]),
  -- | For cards like Artisan, pick n cards that the player would like to put
  --  back onto the top of their deck. The function does that work.
  handToDeckStrategy :: DominionAIGame -> Int -> [Card],
  -- | For the Lurker card, either pick an Action card from supply (Left) or
  --  gain a card from the trash (Right)
  lurkerStrategy     :: DominionAIGame -> Either Card Card,
  -- | When playing the Island card, what card is put on the Island with it?
  islandStrategy     :: DominionAIGame -> Maybe Card,
  -- | Pick one or two (identical) cards to put back in the supply
  -- and make other players gain.
  ambassadorStrategy :: DominionAIGame -> [Card],
  -- | Pick a Supply pile to put an Embargo token on
  embargoStrategy :: DominionAIGame -> Card,
  -- | Pick a Card to set aside for the next turn
  havenStrategy :: DominionAIGame -> Card,
  -- | Add the top card of the deck to the Native Village mat (True),
  -- or bring all of the cards from that mat into the hand (False)?
  nativeVillageStrategy :: DominionAIGame -> Bool,
  -- | Do we move this card from the bottom of the deck to the top?
  pearlDiverStrategy :: DominionAIGame -> Card -> Bool,
  -- | Take three cards from the deck, pick one fo trash, one to discard,
  -- and one to return to the deck.
  lookoutStrategy :: DominionAIGame -> [Card] -> (Card, Card, Card),
  -- | Look at the cards, either return an empty list to discard all of
  -- the originals, or reorder to be put back on the top of the deck.
  navigatorStrategy :: DominionAIGame -> [Card] -> [Card],
  -- | Does the player take +money equal to the number of Coin tokens
  -- on their pirate ship mat, or do they look at the top two cards
  -- of each other player's deck and tell them whether to trash a
  -- treasure (if they do so for one player, they get a Coin token)
  pirateShipStrategy :: DominionAIGame -> Bool,
  -- | The top 2 cards for a particular other player, if at least one
  -- is a treasure card, can return that card to trash it for that
  -- player (if this happens at least one, the pirate ship player
  -- gains a Coin token for their pirate ship mat).
  pirateShipDecisionStrategy :: DominionAIGame -> [Card] -> Maybe Card,
  -- | Choose which card to trash, gaining its cost as +money
  salvagerStrategy :: DominionAIGame -> Maybe Card
} deriving stock (Generic)

instance Show Strategy where
  show s = unpack $ strategyName s

instance Eq Strategy where
  a == b = strategyName a == strategyName b

instance Arbitrary Strategy where
  arbitrary = do
    sn <- arbitrary
    return $ Strategy
      { strategyName = sn
      , buyStrategy = return mempty
      , discardStrategy = return mempty
      , trashStrategy = return mempty
      , retrieveStrategy = return mempty
      , nextCard = return (return mempty)
      , gainCardStrategy = return mempty
      , throneRoomStrategy = return mempty
      , libraryStrategy = return (pure False)
      , sentryStrategy = return mempty
      , handToDeckStrategy = return mempty
      , lurkerStrategy = return $ Left arbitraryCard
      , islandStrategy = return mempty
      , ambassadorStrategy = return mempty
      , embargoStrategy = return arbitraryCard
      , havenStrategy = return arbitraryCard
      , nativeVillageStrategy = return False
      , pearlDiverStrategy = return (pure False)
      , lookoutStrategy = return (pure (arbitraryCard, arbitraryCard, arbitraryCard))
      , navigatorStrategy = return mempty
      , pirateShipStrategy = return False
      , pirateShipDecisionStrategy = return mempty
      , salvagerStrategy = return mempty
      }
      where
        arbitraryCard = Card "Arbitrary Card" 1 (\_ -> return Nothing) Value (\_ -> return 0)

data DominionPlayer = DominionPlayer {
  -- | Player name, mostly used for debugging.
  playerName :: Text,
  -- | Player's current deck.
  deck       :: [Card],
  -- | Player's current discard pile.
  discard    :: [Card],
  -- | Current hand.
  hand       :: [Card],
  -- | Cards that have been played this turn.
  played     :: [Card],
  -- | Number of actions remaining.
  actions    :: Int,
  -- | Number of buys remaining.
  buys       :: Int,
  -- | Amount of money available to spend on cards.
  money      :: Int,
  -- | Number of victory points in the hand. Not relevant until the end of the
  --  game.
  victory    :: Int,
  -- | How many turns has this player completed?
  turns      :: Int,
  -- | The Island mat contents (Seaside Expansion)
  island     :: [Card],
  -- | Duration cards' duration actions to be run at the
  -- start of the following turn.
  duration   :: [PlayerNumber -> DominionState (Maybe DominionAction)],
  -- | Native Village mat contents
  nativeVillage :: [Card],
  -- | How many Lighthouses are protecting this player?
  lighthouse :: Int,
  -- | Coin tokens on the pirate ship mat
  pirateShip :: Int,
  -- NOTE: Add new items above the strategy
  -- | The Strategy used by this player.
  strategy   :: Strategy
} deriving stock (Generic)

instance Eq DominionPlayer where
  a == b = playerName a == playerName b

instance Ord DominionPlayer where
  compare p1 p2
    | victory p1 == victory p2  = turns (p1 :: DominionPlayer) `compare` turns (p2 :: DominionPlayer)
    | otherwise                 = victory p2 `compare` victory p1

data CardPlay = Standard Card | PlayThroneRoom Card | PlayRemodel Card Card | PlayCellar [Card]
  deriving (Show, Eq)

newtype BoughtCard = BoughtCard Card
  deriving (Show, Eq)

-- | A decision for the victim of the Bandit card:
-- Did you have to trash a treasure card, and if so which one?
-- What cards did you discard?
data BanditDecision = BanditDecision
  { trashed :: Maybe Card
  , discarded :: [Card]
  } deriving stock (Show, Eq, Generic)
    deriving Arbitrary via GenericArbitrary BanditDecision
