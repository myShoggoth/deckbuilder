{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module DeckBuilding.Dominion.Types
    ( module DeckBuilding.Dominion.Types
    ) where

import           Control.Monad.RWS
import qualified Data.DList        as DL
import qualified Data.Map          as Map
import qualified Data.Semigroup    as Semi
import           GHC.Generics
import           System.Random

data DominionMove = Turn Int DominionPlayer |
                    Play Card |
                    Deal Int [Card] |
                    Discard [Card] |
                    ThroneRoom Card |
                    Remodel Card Card |
                    Buy Card |
                    Retreive [Card] |
                    Trash [Card] |
                    GameOver [(String, Int)]
                    deriving (Show, Eq)

type DominionState a = RWS DominionConfig (DL.DList DominionMove) DominionGame a

data DominionConfig = DominionConfig {
  -- | Names and strategies for each player
  playerDefs   :: [(String, Strategy)],
  -- | Which kingdom cards to use
  kingdomCards :: [Card],
  -- | How many games to run
  games        :: Int,
  -- | One random number generator per game
  seeds        :: [StdGen]
} deriving Show

instance Semi.Semigroup DominionConfig where
  c1 <> c2 = DominionConfig ((playerDefs c1) ++ (playerDefs c2)) ((kingdomCards c1) ++ (kingdomCards c2)) ((games c1) + (games c2)) ((seeds c1) ++ (seeds c2))

instance Monoid DominionConfig where
  mempty = DominionConfig [] [] 0 []
  mappend = (Semi.<>)

data DominionGame = DominionGame {
  -- | The players of the game.
  players :: [DominionPlayer],
  -- | All the decks, basic and Kingdom: (Card, Number Left)
  decks   :: Map.Map Card Int,
  -- | The trash pile.
  trash   :: [Card],
  -- | The current random number generator, needs to be updated when used.
  random  :: StdGen
} deriving Show

data CardType = Value | Action
  deriving (Show, Eq)

-- | A Dominion card, basic supply, kingdom, or expansion.
data Card = Card {
  -- | Name of the card, like Copper or Market. Used mostly for debugging.
  cardName :: String,
  -- | Money cost of the card.
  cost     :: Int,
  {-|
    The function that changes that game state based on the card. This is the
    core of the whole engine.

    Card: The card being played.
    Int: The number of the player that is playing the card.

    Updates the game state based on what the card does, then returns the
    player number.
  -}
  action   :: Card -> Int -> DominionState Int,
  -- | Value or Action
  cardType :: CardType
}

instance Ord Card where
  compare c1 c2 = compare (cardName c1) (cardName c2)

instance Eq Card where
  a == b = cardName a == cardName b

instance Show Card where
  show = cardName

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
  strategyName       :: String,
  -- | Called when it is time for the player to buy new cards. The strategy
  --  is responsible for lowering the money, adding the cards to the discard
  --  pile, etc.
  buyStrategy        :: Int -> DominionState Int,
  -- | When a card action has the player discard, this function is called.
  --  (min, max) are the minimum number of cards the player has to discard,
  --  and the maximum they are allowed to.
  discardStrategy    :: (Int, Int) -> Int -> DominionState [Card],
  -- | like discardStrategy, except for trashing cards.
  trashStrategy      :: (Int, Int) -> Int -> DominionState [Card],
  -- | Like discardStrategy, except for retrieving cards from the player's
  --  discard pile.
  retrieveStrategy   :: (Int, Int) -> Int -> DominionState [Card],
  -- | Called before the hand is evaluated, lets the strategy determine
  --  which order they want the cards played in.
  nextCard           :: Int -> DominionState (Maybe Card),
  -- | When a card lets the player gain a card up to cost n into their discard
  --  pile, this is called.
  gainCardStrategy   :: Int -> Int -> DominionState (Maybe Card),
  -- | Specifically for the Throne Room card, lets the strategy pick which
  --  card (Just Card) to play twice, or none if Nothing. Pick a card remaining
  --  in the player's hand.
  throneRoomStrategy :: Int -> DominionState (Maybe Card),
  -- | For the Library card, called when the player draws an action and returns
  --  whether or not the player wants to skip that card.
  libraryStrategy    :: Card -> DominionState Bool,
  -- | For the Sentry card, gives the top two cards of the player's deck, then
  --  says which ones that player wants to (trash, discard, keep).
  sentryStrategy     :: [Card] -> Int -> DominionState ([Card], [Card], [Card]),
  -- | For cards like Artisan, pick n cards that the player would like to put
  --  back onto the top of their deck. The function does that work.
  handToDeckStrategy :: Int -> Int -> DominionState [Card],
  -- | For the Lurker card, either pick an Action card from supply (Left) or
  --  gain a card from the trash (Right)
  lurkerStrategy     :: Card -> Int -> DominionState (Either Card Card)
}

instance Show Strategy where
  show = strategyName

instance Eq Strategy where
  a == b = strategyName a == strategyName b

data DominionPlayer = DominionPlayer {
  -- | Player name, mostly used for debugging.
  playerName :: String,
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
  -- | The Strategy used by this player.
  strategy   :: Strategy
} deriving Show

instance Eq DominionPlayer where
  a == b = playerName a == playerName b

instance Ord DominionPlayer where
  compare p1 p2
    | victory p1 == victory p2  = turns p1 `compare` turns p2
    | otherwise                 = victory p2 `compare` victory p1

deriving instance Generic DominionGame
deriving instance Generic DominionPlayer
deriving instance Generic Card
deriving instance Generic Strategy
deriving instance Generic DominionConfig

