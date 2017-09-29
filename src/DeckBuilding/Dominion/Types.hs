{-# LANGUAGE TemplateHaskell #-}

module DeckBuilding.Dominion.Types
    ( module DeckBuilding.Dominion.Types
    ) where

import qualified Data.Map as Map
import System.Random
import Control.Lens
import Control.Monad.State

data Game = Game {
  -- | The players of the game.
  _players  :: [Player],
  -- | All the decks, basic and Kingdom: (Card, Number Left)
  _decks    :: Map.Map Card Int,
  -- | The trash pile.
  _trash    :: [Card],
  -- | The current random number generator, needs to be updated when used.
  _random   :: StdGen
} deriving Show

data CardType = Value | Action
  deriving (Show, Eq)

-- | A Dominion card, basic supply, kingdom, or expansion.
data Card = Card {
  -- | Name of the card, like Copper or Market. Used mostly for debugging.
  _cardName :: String,
  -- | Money cost of the card.
  _cost     :: Int,
  {-|
    The function that changes that game state based on the card. This is the
    core of the whole engine.

    Card: The card being played.
    Player: The player that is playing the card.

    Updates the game state based on what the card does, then returns the
    updated version of the player within the State Monad.
  -}
  _action   :: Card -> Player -> State Game Player,
  -- | Value or Action
  _cardType :: CardType
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
  when creating a Player and see how it does in the game.

  Because these are done in the context of the State Monad, the strategy
  can see the entire game state, including stuff real players wouldn't be
  able to know. Don't use that stuff.

  See DeckBuilding.Dominion.Strategies.Basic for very simple versions.
-}
data Strategy = Strategy {
  -- | Friendly name for the strategy, mostly used for debugging.
  _strategyName       :: String,
  -- | Called when it is time for the player to buy new cards. The strategy
  --  is responsible for lowering the money, adding the cards to the discard
  --  pile, etc.
  _buyStrategy        :: Player -> State Game Player,
  -- | When a card action has the player discard, this function is called.
  --  (min, max) are the minimum number of cards the player has to discard,
  --  and the maximum they are allowed to.
  _discardStrategy    :: (Int, Int) -> Player -> State Game Player,
  -- | like discardStrategy, except for trashing cards.
  _trashStrategy      :: (Int, Int) -> Player -> State Game Player,
  -- | Like discardStrategy, except for retrieving cards from the player's
  --  discard pile.
  _retrieveStrategy   :: (Int, Int) -> Player -> State Game Player,
  -- | Called before the hand is evaluated, lets the strategy determine
  --  which order they want the cards played in.
  _orderHand          :: Player -> State Game Player,
  -- | When a card lets the player gain a card up to cost n into their discard
  --  pile, this is called.
  _gainCardStrategy   :: Int -> Player -> State Game Player,
  -- | Specifically for the Throne Room card, lets the strategy pick which
  --  card (Just Card) to play twice, or none if Nothing. Pick a card remaining
  --  in the player's hand.
  _throneRoomStrategy :: Player -> State Game (Maybe Card),
  -- | For the Library card, called when the player draws an action and returns
  --  whether or not the player wants to skip that card.
  _libraryStrategy    :: Card -> State Game Bool,
  -- | For the Sentry card, gives the top two cards of the player's deck, then
  --  says which ones that player wants to (trash, discard, keep).
  _sentryStrategy     :: [Card] -> Player -> State Game ([Card], [Card], [Card]),
  -- | For cards like Artisan, pick n cards that the player would like to put
  --  back onto the top of their deck. The function does that work.
  _handToDeckStrategy :: Int -> Player -> State Game Player,
  -- | For the Lurker card, either pick an Action card from supply (Left) or
  --  gain a card from the trash (Right)
  _lurkerStrategy     :: Card -> Player -> State Game (Either Card Card)
}

instance Show Strategy where
  show = _strategyName

instance Eq Strategy where
  a == b = _strategyName a == _strategyName b

data Player = Player {
  -- | Player name, mostly used for ebugging.
  _playerName :: String,
  -- | Player's current deck.
  _deck       :: [Card],
  -- | Player's current discard pile.
  _discard    :: [Card],
  -- | Current hand.
  _hand       :: [Card],
  -- | Cards that have been played this turn.
  _played     :: [Card],
  -- | Number of actions remaining.
  _actions    :: Int,
  -- | Number of buys remaining.
  _buys       :: Int,
  -- | Amount of money available to spend on cards.
  _money      :: Int,
  -- | Number of victory points in the hand. Not relevant until the end of the
  --  game.
  _victory    :: Int,
  -- | How many turns has this player completed?
  _turns      :: Int,
  -- | The Strategy used by this player.
  _strategy   :: Strategy
} deriving Show

instance Eq Player where
  a == b = _playerName a == _playerName b

instance Ord Player where
  compare p1 p2
    | _victory p1 == _victory p2  = _turns p1 `compare` _turns p2
    | otherwise                   = _victory p2 `compare` _victory p1

-- | The result of a game. Either Left "Player Name" who is the winner, or
--  Right Int which is the number of players that tied for the lead.
type Result = Either String Int

-- | Control.Lens is used to make updating the data structures easier.
makeLenses ''Game
makeLenses ''Card
makeLenses ''Strategy
makeLenses ''Player
