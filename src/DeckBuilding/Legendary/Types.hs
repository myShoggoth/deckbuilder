{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module DeckBuilding.Legendary.Types
    ( module DeckBuilding.Legendary.Types
    ) where

import           Control.Monad.RWS
import qualified Data.DList         as DL
import qualified Data.Semigroup     as Semi
import           Data.Text
import           System.Random
import           GHC.Generics

data LegendaryMove = Turn Int LegendaryPlayer |
                    Play Card |
                    Deal Int [Card] |
                    Discard [Card] |
                    Buy Card |
                    Attack Card
                    deriving (Show, Eq)

type LegendaryState a = RWS LegendaryConfig (DL.DList LegendaryMove) LegendaryGame a

data LegendaryConfig = LegendaryConfig {
  -- | Names and strategies for each player
  playerDefs :: [(Text, Strategy)],
  -- | How many games to run
  games      :: Int,
  -- | One random number generator per game
  seeds      :: [StdGen]
} deriving Show

instance Semi.Semigroup LegendaryConfig where
  c1 <> c2 = LegendaryConfig (playerDefs c1 ++ playerDefs c2) (games c1 + games c2) (seeds c1 ++ seeds c2)

instance Monoid LegendaryConfig where
  mempty = LegendaryConfig [] 0 []
  mappend = (Semi.<>)

data Mastermind = Mastermind {
  mastermindName :: Text,
  mmEvilWins     :: LegendaryState Bool,
  masterStrike   :: LegendaryState ()
}

instance Show Mastermind where
  show mm = show $ mastermindName mm

data Scheme = Scheme {
  schemeName  :: Text,
  sEvilWins   :: LegendaryState Bool,
  schemeTwist :: LegendaryState ()
}

instance Show Scheme where
  show s = show $ schemeName s

data LegendaryGame = LegendaryGame {
  -- | The players of the game.
  players           :: [LegendaryPlayer],
  scheme            :: Scheme,
  schemeTwists      :: Int,
  mastermind        :: Mastermind,
  mastermindAspects :: [Card],
  masterStrikes     :: Int,
  wounds            :: Int,
  bystanders        :: [Card],
  officers          :: Int,
  villainDeck       :: [Card],
  heroDeck          :: [Card],
  escapees          :: [Card],
  city              :: [Card],
  hq                :: [Card],
  -- | The KO pile.
  koPile            :: [Card],
  -- | The current random number generator, needs to be updated when used.
  random            :: StdGen
} deriving Show

-- | A Legendary card
data Card = Card {
  -- | Name of the card, like Copper or Market. Used mostly for debugging.
  cardName  :: Text,
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
  action    :: Card -> Int -> LegendaryState [Card],
  heroClass :: [Text],
  heroTeam  :: Maybe Text
}

instance Ord Card where
  compare c1 c2 = compare (cardName c1) (cardName c2)

instance Eq Card where
  a == b = cardName a == cardName b

instance Show Card where
  show c = unpack $ cardName c

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
  -- | When a card action has the player discard, this function is called.
  --  (min, max) are the minimum number of cards the player has to discard,
  --  and the maximum they are allowed to.
  discardStrategy  :: (Int, Int) -> Int -> LegendaryState [Card],
  -- | like discardStrategy, except for trashing cards.
  trashStrategy    :: (Int, Int) -> Int -> LegendaryState [Card],
  -- | Like discardStrategy, except for retrieving cards from the player's
  --  discard pile.
  retrieveStrategy :: (Int, Int) -> Int -> LegendaryState [Card],
  -- | Called before the hand is evaluated, lets the strategy determine
  --  which order they want the cards played in.
  orderHand        :: Int -> LegendaryState [Card],
  -- | When a card lets the player gain a card up to cost n into their discard
  --  pile, this is called.
  gainCardStrategy :: Int -> Int -> LegendaryState (Maybe Card),
  attackStrategy   :: Int -> LegendaryState Int
}

instance Show Strategy where
  show s = unpack $ strategyName s

instance Eq Strategy where
  a == b = strategyName a == strategyName b

data LegendaryPlayer = LegendaryPlayer {
  -- | Player name, mostly used for debugging.
  playerName  :: Text,
  -- | Player's current deck.
  deck        :: [Card],
  -- | Player's current discard pile.
  discard     :: [Card],
  -- | Current hand.
  hand        :: [Card],
  -- | Cards that have been played this turn.
  played      :: [Card],
  -- | Amount of money available to spend on cards.
  money       :: Int,
  victoryPile :: [Card],
  victory     :: Int,
  -- | How many turns has this player completed?
  turns       :: Int,
  -- | The Strategy used by this player.
  strategy    :: Strategy
} deriving Show


instance Eq LegendaryPlayer where
  a == b = playerName a == playerName b

instance Ord LegendaryPlayer where
  compare p1 p2
    | victory p1 == victory p2  = turns p1 `compare` turns p2
    | otherwise                   = victory p2 `compare` victory p1

deriving instance Generic LegendaryGame
deriving instance Generic LegendaryPlayer
deriving instance Generic Card
deriving instance Generic Mastermind
deriving instance Generic Scheme
deriving instance Generic Strategy

