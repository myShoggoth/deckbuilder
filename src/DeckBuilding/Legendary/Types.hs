{-# LANGUAGE TemplateHaskell #-}

module DeckBuilding.Legendary.Types
    ( module DeckBuilding.Legendary.Types
    ) where

import           Control.Lens
import           Control.Monad.RWS
import qualified Data.DList         as DL
import qualified Data.Map           as Map
import qualified Data.Semigroup     as Semi
import           DeckBuilding.Types
import           System.Random

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
  _playerDefs :: [(String, Strategy)],
  -- | How many games to run
  _games      :: Int,
  -- | One random number generator per game
  _seeds      :: [StdGen]
} deriving Show

instance Semi.Semigroup LegendaryConfig where
  c1 <> c2 = LegendaryConfig ((_playerDefs c1) ++ (_playerDefs c2)) ((_games c1) + (_games c2)) ((_seeds c1) ++ (_seeds c2))

instance Monoid LegendaryConfig where
  mempty = LegendaryConfig [] 0 []
  mappend = (Semi.<>)

data Mastermind = Mastermind {
  _mastermindName :: String,
  _mmEvilWins     :: LegendaryState Bool,
  _masterStrike   :: LegendaryState ()
}

instance Show Mastermind where
  show mm = show $ _mastermindName mm

data Scheme = Scheme {
  _schemeName  :: String,
  _sEvilWins   :: LegendaryState Bool,
  _schemeTwist :: LegendaryState ()
}

instance Show Scheme where
  show s = show $ _schemeName s

data LegendaryGame = LegendaryGame {
  -- | The players of the game.
  _players           :: [LegendaryPlayer],
  _scheme            :: Scheme,
  _schemeTwists      :: Int,
  _mastermind        :: Mastermind,
  _mastermindAspects :: [Card],
  _masterStrikes     :: Int,
  _wounds            :: Int,
  _bystanders        :: [Card],
  _officers          :: Int,
  _villainDeck       :: [Card],
  _heroDeck          :: [Card],
  _escapees          :: [Card],
  _city              :: [Card],
  _hq                :: [Card],
  -- | The KO pile.
  _KoPile            :: [Card],
  -- | The current random number generator, needs to be updated when used.
  _random            :: StdGen
} deriving Show

-- | A Legendary card
data Card = Card {
  -- | Name of the card, like Copper or Market. Used mostly for debugging.
  _cardName  :: String,
  -- | Money cost of the card.
  _cost      :: Int,
  {-|
    The function that changes that game state based on the card. This is the
    core of the whole engine.

    Card: The card being played.
    Int: The number of the player that is playing the card.

    Updates the game state based on what the card does, then returns the
    player number.
  -}
  _action    :: Card -> Int -> LegendaryState Int,
  _heroClass :: [String],
  _heroTeam  :: Maybe String
}

instance Ord Card where
  compare c1 c2 = compare (_cardName c1) (_cardName c2)

instance Eq Card where
  a == b = _cardName a == _cardName b

instance Show Card where
  show = _cardName

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
  _strategyName     :: String,
  -- | Called when it is time for the player to buy new cards. The strategy
  --  is responsible for lowering the money, adding the cards to the discard
  --  pile, etc.
  _buyStrategy      :: Int -> LegendaryState [Card],
  -- | When a card action has the player discard, this function is called.
  --  (min, max) are the minimum number of cards the player has to discard,
  --  and the maximum they are allowed to.
  _discardStrategy  :: (Int, Int) -> Int -> LegendaryState [Card],
  -- | like discardStrategy, except for trashing cards.
  _trashStrategy    :: (Int, Int) -> Int -> LegendaryState [Card],
  -- | Like discardStrategy, except for retrieving cards from the player's
  --  discard pile.
  _retrieveStrategy :: (Int, Int) -> Int -> LegendaryState [Card],
  -- | Called before the hand is evaluated, lets the strategy determine
  --  which order they want the cards played in.
  _orderHand        :: Int -> LegendaryState [Card],
  -- | When a card lets the player gain a card up to cost n into their discard
  --  pile, this is called.
  _gainCardStrategy :: Int -> Int -> LegendaryState (Maybe Card),
  _attackStrategy   :: Int -> LegendaryState [Card]
}

instance Show Strategy where
  show = _strategyName

instance Eq Strategy where
  a == b = _strategyName a == _strategyName b

data LegendaryPlayer = LegendaryPlayer {
  -- | Player name, mostly used for debugging.
  _playerName  :: String,
  -- | Player's current deck.
  _deck        :: [Card],
  -- | Player's current discard pile.
  _discard     :: [Card],
  -- | Current hand.
  _hand        :: [Card],
  -- | Cards that have been played this turn.
  _played      :: [Card],
  -- | Amount of money available to spend on cards.
  _money       :: Int,
  _victoryPile :: [Card],
  _victory     :: Int,
  -- | How many turns has this player completed?
  _turns       :: Int,
  -- | The Strategy used by this player.
  _strategy    :: Strategy
} deriving Show

instance Eq LegendaryPlayer where
  a == b = _playerName a == _playerName b

instance Ord LegendaryPlayer where
  compare p1 p2
    | _victory p1 == _victory p2  = _turns p1 `compare` _turns p2
    | otherwise                   = _victory p2 `compare` _victory p1

-- | Control.Lens is used to make updating the data structures easier.
makeLenses ''Mastermind
makeLenses ''Scheme
makeLenses ''LegendaryGame
makeLenses ''LegendaryConfig
makeLenses ''Card
makeLenses ''Strategy
makeLenses ''LegendaryPlayer
