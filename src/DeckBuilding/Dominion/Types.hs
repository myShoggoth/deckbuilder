{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels          #-}

module DeckBuilding.Dominion.Types
    ( -- Explicitly export Card and its fields, plus other necessary types
      Card ( Card -- Constructor
             , cardName -- Field
             , cost     -- Field
             , action   -- Field
             , cardType -- Field
             , victoryPoints -- Field
             , numImplicitTypes -- Field
             )
    , DominionState
    , Strategy (..)
    , DominionAIGame (..)
    , DominionBuy (..)
    , DominionPlayer (..)
    , CardType (..)
    , DominionAction (..)
    , DominionDraw (..)
    , DominionConfig (..)
    , DominionBoard (..)
    , DominionGame (..)
    , DominionTurn (..)
    , DominionPlayerTurn (..)
    , CourtierChoice (..)
    , CardLocation (..)
    , BanditDecision(..)
    -- Keep other exports if they were explicit, or add as needed
    --, module DeckBuilding.Dominion.Types -- Remove or comment out the re-export
    ) where

import Control.Lens.At (Index)
import Control.Monad.State.Lazy (State)
import qualified Data.Map as Map
import qualified Data.Semigroup as Semi
import Data.Text ( unpack, Text )
import GHC.Generics ( Generic )
import System.Random ( StdGen, mkStdGen )
import DeckBuilding.Types ( PlayerNumber, Game )
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..), Arbitrary (arbitrary))
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Instances ()
import Data.Map (Map)
import Data.Generics.Labels ()
import Control.Lens ( (^.), use, (%=), (.=), Ixed(ix) )
import Data.Generics.Product ( HasField(field) )

data DominionGame = DominionGame
  { players :: [(Text, Strategy)]
  , kingdoms :: [Card]
  , seed :: StdGen
  , turns :: [DominionTurn]
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
  , gained :: [Card]
  }
  deriving stock (Show, Generic)
  deriving Arbitrary via GenericArbitrary DominionPlayerTurn

data DominionBuy = DominionBuy Int Card
  deriving stock (Show, Generic, Eq)
  deriving Arbitrary via GenericArbitrary DominionBuy

data DominionAction =
      Copper | Silver | Gold | Harem |
      Curse | Estate | Duchy | Province | Gardens | Duke |
      Ambassador [Card] (Map.Map PlayerNumber (Either Card (Maybe Card))) |
      Artisan Card Card |
      Astrolabe |
      AstrolabeDuration |
      Bandit (Map.Map PlayerNumber (Either Card BanditDecision)) |
      Baron Bool |
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
      Masquerade DominionDraw [Maybe Card] (Maybe Card) |
      Merchant DominionDraw |
      MerchantShip |
      MerchantShipDuration |
      Militia (Map.Map PlayerNumber (Either Card [Card])) |
      Mine Card Card |
      Moat DominionDraw |
      MoneyLender |
      NativeVillage (Either Card [Card]) |
      Navigator [Card] |
      Outpost |
      OutpostDuration DominionDraw [DominionBuy] |
      Pawn DominionDraw |
      PearlDiver DominionDraw Card Bool |
      PirateShip (Either Int (Map.Map PlayerNumber (Either Card (Maybe Card)))) |
      Poacher DominionDraw [Card] |
      Salvager Card |
      SeaHag (Map.Map PlayerNumber (Either Card (Maybe Card, Maybe Card))) |
      Sentry DominionDraw [Card] [Card] [Card] |
      ShantyTown DominionDraw [Card] |
      Smithy DominionDraw |
      Smuggler Card |
      Steward DominionDraw Int [Card] |
      Swindler (Map.Map PlayerNumber (Maybe Card, Maybe Card)) |
      Tactician [Card] |
      TacticianDuration DominionDraw |
      ThroneRoom Card DominionAction DominionAction |
      TreasureMap [Card] |
      Treasury DominionDraw |
      TreasuryDuration |
      Vassal (Maybe DominionAction) |
      Village DominionDraw |
      Witch DominionDraw (Map.Map PlayerNumber (Either Card (Maybe Card))) |
      Warehouse DominionDraw [Card] |
      Wharf DominionDraw |
      WharfDuration DominionDraw |
      WishingWell DominionDraw (Maybe Card) Bool |
      Workshop Card |
      TidePools DominionDraw |
      TidePoolsDuration [Card] |
      SeaChart DominionDraw (Maybe Card) |
      Blockade Card |
      Monkey DominionDraw |
      MonkeyDuration DominionDraw |
      Corsair (Map PlayerNumber (Either Card (Maybe Card))) |
      Sailor Bool |
      SeaWitch (Map PlayerNumber (Either Card (Maybe Card))) |
      Bridge |
      Diplomat DominionDraw Int |
      Upgrade DominionDraw (Maybe Card) (Maybe Card) |
      Mill [Card] |
      MiningVillage Card |
      SecretPassage (Maybe Card) (Maybe Card) |
      Nobles DominionDraw Int |
      Patrol DominionDraw [Card] [Card] |
      Minion Bool DominionDraw (Map.Map PlayerNumber (Either Card [Card])) |
      Torturer DominionDraw (Map.Map PlayerNumber (Either Card (Either [Card] Bool))) |
      TradingPost [Card] (Maybe Card) |
      Replace Card Card CardLocation |
      Courtier Card [Maybe CourtierChoice]
  deriving stock (Show, Generic, Eq)
  deriving Arbitrary via GenericArbitrary DominionAction

newtype DominionDraw = DominionDraw [Card]
  deriving stock (Show, Generic, Eq)
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
  -- | What cards did each player buy in their last turn?
  lastBuys :: Map.Map PlayerNumber [Card],

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
data CardType = Value | Action | Duration | Victory | CurseType
  deriving stock (Show, Eq, Generic)
  deriving Arbitrary via GenericArbitrary CardType

-- | A Dominion card, basic supply, kingdom, or expansion.
data Card = Card {
  -- | Name of the card, like Copper or Market. Used mostly for debugging.
  cardName :: Text,
  -- | Cost of the card in money.
  cost :: Int,
  -- | The action to take when the card is played.
  action :: PlayerNumber -> DominionState (Maybe DominionAction),
  -- | The primary type, used for interactions like Moat, Vassal etc.
  cardType :: CardType,
  -- | How many points the card is worth at the end of the game.
  victoryPoints :: PlayerNumber -> DominionState Int,
  -- | Number of implicit types for Courtier (Action, Treasure, Victory, Curse)
  numImplicitTypes :: Int -- Added field for Courtier
} deriving stock (Generic)

type instance Index Card = Card

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
  strategyName               :: Text,
  -- | Called when it is time for the player to buy new cards. The strategy
  --  is responsible for lowering the money, adding the cards to the discard
  --  pile, etc.
  buyStrategy                :: DominionAIGame -> [DominionBuy],
  -- | When a card action has the player discard, this function is called.
  --  (min, max) are the minimum number of cards the player has to discard,
  --  and the maximum they are allowed to.
  discardStrategy            :: DominionAIGame -> (Int, Int) -> [Card],
  -- | The player is being prompted to trash zero or more cards from either
  -- their hand only, or the discard pile and the hand. (min, max) are the 
  -- minimum and maximum numbers of cards that may be trashed, and the
  -- discard pile is passed in if applicable.
  -- Return value is a tuple of the card trashing from the hand and the discard
  -- pile, respectively.
  trashStrategy              :: DominionAIGame -> (Int, Int) -> [Card] -> [Card],
  -- | Like discardStrategy, except for retrieving cards from the player's
  --  discard pile.
  retrieveStrategy           :: DominionAIGame -> (Int, Int) -> [Card] -> [Card],
  -- | Called before the hand is evaluated, lets the strategy determine
  --  which order they want the cards played in.
  nextCard                   :: PlayerNumber -> DominionState (Maybe Card),
  -- | When a card lets the player gain a card up to cost n into their discard
  --  pile, this is called.
  gainCardStrategy           :: DominionAIGame -> Int -> Maybe Card,
  -- | Specifically for the Throne Room card, lets the strategy pick which
  --  card (Just Card) to play twice, or none if Nothing. Pick a card remaining
  --  in the player's hand.
  throneRoomStrategy         :: DominionAIGame -> Maybe Card,
  -- | For the Library card, called when the player draws an action and returns
  --  whether or not the player wants to skip that card.
  libraryStrategy            :: DominionAIGame -> Card -> Bool,
  -- | For the Sentry card, gives the top two cards of the player's deck, then
  --  says which ones that player wants to (trash, discard, keep).
  sentryStrategy             :: DominionAIGame -> [Card] -> ([Card], [Card], [Card]),
  -- | For cards like Artisan, pick n cards that the player would like to put
  --  back onto the top of their deck. The function does that work.
  handToDeckStrategy         :: DominionAIGame -> Int -> [Card],
  -- | For the Lurker card, either pick an Action card from supply (Left) or
  --  gain a card from the trash (Right)
  lurkerStrategy             :: DominionAIGame -> Either Card Card,
  -- | When playing the Island card, what card is put on the Island with it?
  islandStrategy             :: DominionAIGame -> Maybe Card,
  -- | Pick one or two (identical) cards to put back in the supply
  -- and make other players gain.
  ambassadorStrategy         :: DominionAIGame -> [Card],
  -- | Pick a Supply pile to put an Embargo token on
  embargoStrategy            :: DominionAIGame -> Card,
  -- | Pick a Card to set aside for the next turn
  havenStrategy              :: DominionAIGame -> Card,
  -- | Add the top card of the deck to the Native Village mat (True),
  -- or bring all of the cards from that mat into the hand (False)?
  nativeVillageStrategy      :: DominionAIGame -> Bool,
  -- | Do we move this card from the bottom of the deck to the top?
  pearlDiverStrategy         :: DominionAIGame -> Card -> Bool,
  -- | Take three cards from the deck, pick one fo trash, one to discard,
  -- and one to return to the deck.
  lookoutStrategy            :: DominionAIGame -> [Card] -> (Card, Card, Card),
  -- | Look at the cards, either return an empty list to discard all of
  -- the originals, or reorder to be put back on the top of the deck.
  navigatorStrategy          :: DominionAIGame -> [Card] -> [Card],
  -- | Does the player take +money equal to the number of Coin tokens
  -- on their pirate ship mat, or do they look at the top two cards
  -- of each other player's deck and tell them whether to trash a
  -- treasure (if they do so for one player, they get a Coin token)
  pirateShipStrategy         :: DominionAIGame -> Bool,
  -- | The top 2 cards for a particular other player, if at least one
  -- is a treasure card, can return that card to trash it for that
  -- player (if this happens at least one, the pirate ship player
  -- gains a Coin token for their pirate ship mat).
  pirateShipDecisionStrategy :: DominionAIGame -> [Card] -> Maybe Card,
  -- | Choose which card to trash, gaining its cost as +money
  salvagerStrategy           :: DominionAIGame -> Maybe Card,
  -- | Do we want to return the Treasury card to the deck?
  treasuryStrategy           :: DominionAIGame -> Bool,
  -- | Choose two: +1 card, +1 action, +1 buy, +1 money
  -- Choices must be different
  pawnStrategy               :: DominionAIGame -> (Int, Int, Int, Int),
  -- | Choose a card to pass to the left (if possible)
  masqueradePassStrategy     :: DominionAIGame -> Maybe Card,
  -- | Choose either draw 2 cards, gain two money, or trash two cards from hand
  stewardStrategy            :: DominionAIGame -> (Int, Int, [Card]),
  -- | Choose a supply card of a particular cost to replace the trashed card
  -- for the other player.
  swindlerStrategy           :: DominionAIGame -> Int -> Maybe Card,
  -- | Guess which card is on the top of the deck, get it in hand if right.
  wishingWellStrategy        :: DominionAIGame -> Card,
  -- | Pick a card to gain that costs up to 6 that the player to this
  -- player's right gained last turn. The lists of gained cards is passed in. 
  smugglerStrategy           :: DominionAIGame -> [Card] -> Maybe Card,
  -- | Secret Passage, pick a card to put on top of the deck, and one to put in hand.
  -- First card in the return tuple is the card to put on top of the deck, second is the card to put in hand.
  secretPassageStrategy      :: DominionAIGame -> Maybe Card -> Maybe Card -> (Maybe Card, Maybe Card),
  -- | Choose one: +3 Cards; or +2 Actions
  noblesStrategy             :: DominionAIGame -> Bool,  -- True for +3 Cards, False for +2 Actions
  -- | Put revealed Victory cards and Curses back in any order
  patrolOrderStrategy        :: DominionAIGame -> [Card] -> [Card],
  -- | Choose one: +$2; or discard your hand, +4 Cards
  minionStrategy             :: DominionAIGame -> Bool,  -- True for +$2, False for discard/draw
  -- | Choose one: discard 2 cards; or gain a Curse to hand
  torturerStrategy           :: DominionAIGame -> Bool,  -- True for discard, False for Curse
  -- | Reveal the card on top of the deck and get a bonus
  courtierRevealStrategy     :: DominionAIGame -> Card,
  -- | Get a bonus based on the revealed card
  courtierBonusStrategy      :: DominionAIGame -> Card -> Int -> [Maybe CourtierChoice]
} deriving stock (Generic)

instance Show Strategy where
  show s = unpack $ strategyName s

instance Eq Strategy where
  a == b = strategyName a == strategyName b

instance Arbitrary Strategy where
  arbitrary = do
    sn <- arbitrary
    let arbitraryCard = Card "ArbitraryPlaceholder" 0 (\_ -> pure Nothing) Value (\_ -> pure 0) 1
    return $ Strategy {
      strategyName = sn,
      buyStrategy = \_ -> [],
      discardStrategy = \_ _ -> [],
      trashStrategy = \_ _ _ -> [],
      retrieveStrategy = \_ _ _ -> [],
      gainCardStrategy = \_ _ -> Nothing,
      throneRoomStrategy = \_ -> Nothing,
      libraryStrategy = \_ _ -> True,
      sentryStrategy = \_ cs -> ([], [], cs),
      handToDeckStrategy = \_ n -> [],
      lurkerStrategy = \_ -> Left arbitraryCard,
      islandStrategy = \_ -> Nothing,
      ambassadorStrategy = \_ -> [],
      embargoStrategy = \_ -> arbitraryCard,
      havenStrategy = \_ -> arbitraryCard,
      nativeVillageStrategy = \_ -> False,
      pearlDiverStrategy = \_ _ -> False,
      lookoutStrategy = \_ cards -> case cards of
                                     [c1, c2, c3] -> (c1, c2, c3)
                                     _            -> error "Arbitrary Lookout strategy needs 3 cards",
      navigatorStrategy = \_ cs -> cs,
      pirateShipStrategy = \_ -> False,
      pirateShipDecisionStrategy = \_ _ -> Nothing,
      salvagerStrategy = \_ -> Nothing,
      treasuryStrategy = \_ -> True,
      pawnStrategy = \_ -> (1, 1, 0, 0),
      masqueradePassStrategy = \_ -> Nothing,
      stewardStrategy = \_ -> (2, 0, []),
      swindlerStrategy = \_ _ -> Nothing,
      wishingWellStrategy = \_ -> arbitraryCard,
      smugglerStrategy = \_ _ -> Nothing,
      secretPassageStrategy = \_ c1 c2 -> (c1, c2),
      noblesStrategy = \_ -> True,
      patrolOrderStrategy = \_ cs -> cs,
      minionStrategy = \_ -> True,
      torturerStrategy = \_ -> True,
      courtierRevealStrategy = \aig -> head (aig ^. #hand),
      courtierBonusStrategy = \_ _ numTypes -> replicate numTypes Nothing
    }

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
  -- start of the following turn, along with the associated card.
  duration   :: [(Card, PlayerNumber -> DominionState (Maybe DominionAction))],
  -- | Native Village mat contents
  nativeVillage :: [Card],
  -- | How many Lighthouses are protecting this player?
  lighthouse :: Int,
  -- | Coin tokens on the pirate ship mat
  pirateShip :: Int,
  -- | Has the player played an Outpost this turn?
  outpost :: Bool,
  -- | All cards gained this turn, needed for Smuggler
  gained :: [Card],
  -- NOTE: Add new items above the strategy
  -- | The Strategy used by this player.
  strategy   :: Strategy
} deriving stock (Generic)

instance Eq DominionPlayer where
  a == b = playerName a == playerName b

instance Ord DominionPlayer where
  compare p1 p2
    | victory p1 == victory p2  = (p1 ^. #turns) `compare` (p2 ^. #turns)
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

data CourtierChoice = CourtierAction | CourtierBuy | CourtierMoney | CourtierGainGold
  deriving stock (Eq, Show, Generic)
  deriving Arbitrary via GenericArbitrary CourtierChoice

data CardLocation = Hand | Deck | Discard
  deriving stock (Show, Eq, Generic)
  deriving Arbitrary via GenericArbitrary CardLocation

-- Restore manual instances based on cardName because functions can't be derived
instance Ord Card where
  compare c1 c2 = compare (cardName c1) (cardName c2)

instance Eq Card where
  c1 == c2 = cardName c1 == cardName c2

instance Show Card where
  show c = unpack $ cardName c

-- Restore basic Arbitrary instance for Card (adjust if needed)
instance Arbitrary Card where
  arbitrary = do
    cn <- arbitrary -- Generate arbitrary Text for name
    co <- arbitrary -- Generate arbitrary Int for cost
    ct <- arbitrary -- Generate arbitrary CardType
    return $ Card {
      cardName = cn,
      cost = co,
      action = \_ -> return Nothing, -- Placeholder action
      cardType = ct,
      victoryPoints = \_ -> return 0, -- Placeholder VP
      numImplicitTypes = 1 -- Default to 1 type for arbitrary cards
    }
