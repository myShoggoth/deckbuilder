{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE DerivingStrategies        #-}

module DeckBuilding.Legendary.Types
    ( module DeckBuilding.Legendary.Types
    ) where

import           Control.Monad.RWS
import qualified Data.DList         as DL
import qualified Data.Semigroup     as Semi
import           Data.Text
import           System.Random
import           GHC.Generics
import           Control.Lens
import Data.Generics.Product ()
import Data.Generics.Labels ()

data LegendaryMove = Turn Int LegendaryPlayer |
                    Play HeroCard |
                    Deal Int [HeroCard] |
                    Discard [HeroCard] |
                    Buy HeroCard |
                    Attack VillainCard
                    deriving stock (Show, Eq)

type LegendaryState a = RWS LegendaryConfig (DL.DList LegendaryMove) LegendaryGame a

data LegendaryConfig = LegendaryConfig {
  -- | Names and strategies for each playerz
  playerDefs  :: [(Text, Strategy)],
  -- | How many games to run
  games       :: Int,
  heroDeck    :: [[HeroCard]],
  villainDeck :: [[VillainCard]],
  bystanders  :: [VillainCard],
  entrance    :: CityLocation,
  scheme      :: Scheme,
  theMastermind  :: Mastermind,
  -- | One random number generator per game
  seeds       :: [StdGen]
} deriving stock (Show, Generic)

instance Semi.Semigroup LegendaryConfig where
  c1 <> c2 = LegendaryConfig (playerDefs c1 ++ playerDefs c2)
                              (games c1 + games c2)
                              ((c1 ^. #heroDeck) ++ (c2 ^. #heroDeck))
                              ((c1 ^. #villainDeck) ++ (c2 ^. #villainDeck))
                              ((c1 ^. #bystanders) ++ (c2 ^. #bystanders))
                              (c1 ^. #entrance)
                              (c1 ^. #scheme)
                              (c1 ^. #theMastermind)
                              ((c1 ^. #seeds) ++ (c2 ^. #seeds))

instance Monoid LegendaryConfig where
  mempty = LegendaryConfig [] 0 [] [] [] defaultCity memptyScheme memptyMastermind []
  mappend = (Semi.<>)

memptyScheme :: Scheme
memptyScheme = Scheme "" (pure True) ((\_ -> pure ()) :: Int -> LegendaryState ()) 0

memptyMastermind :: Mastermind
memptyMastermind = Mastermind "" 0 0 (pure True) (\_ -> pure ()) [] [[]]

data Mastermind = Mastermind {
  mastermindName      :: Text,
  attack              :: Int,
  victoryPoints       :: Int,
  mmEvilWins          :: LegendaryState Bool,
  masterStrikeAction  :: Int -> LegendaryState (),
  aspects             :: [VillainCard],
  alwaysLeads         :: [[VillainCard]]
} deriving stock (Generic)

instance Show Mastermind where
  show mm = show $ mastermindName mm

-- TODO need a way for the scheme to adjust the setup
data Scheme = Scheme {
  schemeName        :: Text,
  evilWins          :: LegendaryState Bool,
  schemeTwistAction :: Int -> LegendaryState (),
  twists            :: Int
} deriving stock (Generic)

instance Show Scheme where
  show s = show $ schemeName s

data Location = Sewers | Bank | Rooftops | Streets | Bridge
  deriving stock (Show, Generic, Eq)

data CityLocation = CityLocation {
  location :: Location,
  villain :: Maybe VillainCard,
  hostages :: [VillainCard],
  next :: Maybe CityLocation
} deriving stock (Show, Generic)

defaultCity :: CityLocation
defaultCity =
  CityLocation Sewers Nothing []
    ( Just $ CityLocation Bank Nothing []
      ( Just $ CityLocation Rooftops Nothing []
        (Just $ CityLocation Streets Nothing []
          ( Just $ CityLocation Bridge Nothing [] Nothing
          )
        )
      )
    )

instance Eq CityLocation where
  cl1 == cl2 = (location cl1) == (location cl2)

data LegendaryGame = LegendaryGame {
  -- | The players of the game.
  players           :: [LegendaryPlayer],
  scheme            :: Scheme,
  masterminds       :: [Mastermind],
  wounds            :: Int,
  bystanders        :: [VillainCard],
  officers          :: Int,
  villainDeck       :: [VillainCard],
  heroDeck          :: [HeroCard],
  escapees          :: [VillainCard],
  entrance          :: CityLocation,
  hq                :: [Maybe HeroCard],
  -- | The KO pile.
  koPile            :: [HeroCard],
  -- | The current random number generator, needs to be updated when used.
  random            :: StdGen
} deriving stock (Show, Generic)

data HeroTeam = NoTeam | SHIELD | XMen | Avengers | SpiderFriends

data HeroClass = NoClass | Strength | Instinct | Tech | Ranged
  deriving Eq

-- | A Legendary Hero card
data HeroCard = HeroCard {
  -- | Name of the card, like Copper or Market. Used mostly for debugging.
  heroName  :: Text,
  -- | Money cost of the card.
  cost      :: Int,
  {-|
    The function that changes that game state based on the card. This is the
    core of the whole engine.

    Card: The card being played.
    Int: The number of the player that is playing the card.

    Updates the game state based on what the card does, then returns the
    player's new hand.
  -}
  action    :: HeroCard -> Int -> LegendaryState Int,
  heroClass :: [HeroClass],
  heroTeam  :: HeroTeam
} deriving stock (Generic)

instance Ord HeroCard where
  compare c1 c2 = compare (heroName c1) (heroName c2)

instance Eq HeroCard where
  a == b = heroName a == heroName b

instance Show HeroCard where
  show c = unpack $ heroName c

data VillainCard = VillainCard {
  villainName   :: Text,
  attack        :: Int,
  bribable      :: Bool,
  isBystander   :: Bool,
  victoryPoints :: (VillainCard -> Int -> LegendaryState Int),
  ambushAction  :: Maybe (VillainCard -> Int -> LegendaryState ()),
  fightAction   :: Maybe (VillainCard -> CityLocation -> Int -> LegendaryState Int),
  escapeAction  :: Maybe (VillainCard -> Int -> LegendaryState ())
} deriving stock (Generic)

instance Ord VillainCard where
  compare c1 c2 = compare (villainName c1) (villainName c2)

instance Eq VillainCard where
  a == b = villainName a == villainName b

instance Show VillainCard where
  show c = unpack $ villainName c

data DrawDiscardChoice = DrawChoice | DiscardChoice
  deriving stock (Show)

{-|
  The playing strategy used by the player. A list of functions that are
  called at different times in the game for the player to make a decision.

  To create a new strategy, implement each of the functions (or use one of
  the basic ones if that's good enough) and create a Strategy. Then pass it
  when creating a DominionPlayer and see how it does in the game.

  Because these are done in the context of the State Monad, the strategy
  can see the entire game state, including stuff real players wouldn't be
  able to know. Don't use that stuff.

  See DeckBuilding.Dominion.Strategies.Basic for very simple versions.
-}
data Strategy = Strategy {
  -- | Friendly name for the strategy, mostly used for debugging.
  strategyName     :: Text,
  -- | Called when it is time for the player to buy new cards. The strategy
  --  is responsible for lowering the money, adding the cards to the discard
  --  pile, etc.
  buyStrategy      :: Int -> LegendaryState Int,
  -- | Called before the hand is evaluated, lets the strategy determine
  --  which order they want the cards played in.
  nextCard           :: Int -> LegendaryState (Maybe HeroCard),
  -- | When a card action has the player discard, this function is called.
  --  (min, max) are the minimum number of cards the player has to discard,
  --  and the maximum they are allowed to.
  discardStrategy  :: (Int, Int) -> Int -> LegendaryState [HeroCard],
  -- | like discardStrategy, except for trashing cards.
  trashStrategy    :: (Int, Int) -> Int -> LegendaryState [HeroCard],
  -- | Like discardStrategy, except for retrieving cards from the player's
  --  discard pile.
  retrieveStrategy :: (Int, Int) -> Int -> LegendaryState [HeroCard],
  -- | Called before the hand is evaluated, lets the strategy determine
  --  which order they want the cards played in.
  orderHand        :: Int -> LegendaryState [HeroCard],
  -- | When a card lets the player gain a card up to cost n into their discard
  --  pile, this is called.
  gainCardStrategy :: Int -> Int -> LegendaryState (Maybe HeroCard),
  attackStrategy   :: Int -> LegendaryState Int,
  koNOfStrategy  :: (Int, Int) -> [HeroCard] -> Int -> LegendaryState ([HeroCard], [HeroCard]),
  -- | Possible cards -> Number to recruit -> max cost
  recruitNStrategy :: [HeroCard] -> Int -> Int -> Int -> LegendaryState ([HeroCard]),
  -- | Others must draw or discard N cards -> because of player P
  othersDrawOrDiscardStrategy :: Int -> Int -> LegendaryState DrawDiscardChoice
} deriving stock (Generic)

instance Show Strategy where
  show s = unpack $ strategyName s

instance Eq Strategy where
  a == b = strategyName a == strategyName b

data LegendaryPlayer = LegendaryPlayer {
  -- | Player name, mostly used for debugging.
  playerName  :: Text,
  -- | Player's current deck.
  deck        :: [HeroCard],
  -- | Player's current discard pile.
  discard     :: [HeroCard],
  -- | Current hand.
  hand        :: [HeroCard],
  -- | Cards that have been played this turn.
  played      :: [HeroCard],
  -- | Amount of money available to spend on cards.
  unusedMoney       :: Int,
  unusedAttack      :: Int,
  nextTurnCards     :: Int,
  victoryPile :: [VillainCard],
  victory     :: Int,
  -- | How many turns has this player completed?
  turns       :: Int,
  -- | The Strategy used by this player.
  strategy    :: Strategy
} deriving stock (Show, Generic)

instance Eq LegendaryPlayer where
  a == b = playerName a == playerName b

instance Ord LegendaryPlayer where
  compare p1 p2
    | victory p1 == victory p2  = turns p1 `compare` turns p2
    | otherwise                 = victory p2 `compare` victory p1
