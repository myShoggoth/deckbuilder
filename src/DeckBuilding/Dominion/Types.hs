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

module DeckBuilding.Dominion.Types
    ( module DeckBuilding.Dominion.Types
    ) where

import Control.Lens.At (Index)
import Control.Monad.RWS ( RWS )
import qualified Data.DList as DL
import qualified Data.Map as Map
import qualified Data.Semigroup as Semi
import Data.Text ( unpack, Text )
import GHC.Generics ( Generic )
import System.Random ( StdGen )
import DeckBuilding.Types ( Result, PlayerNumber )

data DominionMove = Turn PlayerNumber Int DominionPlayer |
                    Deal PlayerNumber Int [Card] |
                    Buy PlayerNumber Card |
                    Cellar PlayerNumber [Card] |
                    PlayBasic PlayerNumber Card Int Int Int Int |
                    PlayValue PlayerNumber Card Int |
                    ThroneRoom PlayerNumber Card DominionMove DominionMove |
                    Remodel PlayerNumber Card Card |
                    Vassal PlayerNumber (Maybe DominionMove) |
                    Militia PlayerNumber (Map.Map PlayerNumber (Either Card [Card])) |
                    MoneyLender PlayerNumber Card |
                    Poacher PlayerNumber [Card] |
                    Chapel PlayerNumber [Card] |
                    Harbinger PlayerNumber (Maybe Card) |
                    Bureaucrat PlayerNumber (Map.Map PlayerNumber (Maybe Card)) |
                    Bandit PlayerNumber (Map.Map PlayerNumber (Either Card BanditDecision)) |
                    CouncilRoom PlayerNumber [Card] (Map.Map PlayerNumber (Maybe Card)) |
                    Witch PlayerNumber [Card] (Map.Map PlayerNumber (Either Card (Maybe Card))) |
                    Mine PlayerNumber Card Card |
                    Library PlayerNumber [Card] [Card] |
                    Sentry PlayerNumber [Card] [Card] [Card] |
                    Artisan PlayerNumber Card Card |
                    Workshop PlayerNumber Card |
                    GameOver [(Text, Int)]

instance Show DominionMove where
  show (Turn _ n p )    = "Turn " <> show n <> " for player " <> show (playerName p)
  show (Deal _ n xs)    = "    Dealing " <> show n <> " card(s):\n"
                        <> show xs
  show (Buy _ c)        = "  Buying " <> show c
  show (Cellar _ xs)    = "  Cellared " <> show xs
  show (PlayValue _ c m)= "  Playing value card " <> show (cardName c) <> " to gain $" <> show m
  show (PlayBasic _ c d a b m) = "  Playing basic action card " <> show (cardName c) <> ": draw " <> show d <> ", actions " <> show a <> ", buys " <> show b <> ", $" <> show m
  show (ThroneRoom _ c _ _) = "  Using Thrown Room on " <> show c
  show (Remodel _ c c') = "  Remodelling " <> show c <> " into " <> show c'
  show (Harbinger _ mc)   = "    Harbinger, discarding " <> show mc
  show (GameOver xs)  = "Game Over!\n"
                        <> "Results: " <> show xs

type DominionState a = RWS DominionConfig (DL.DList DominionMove) DominionGame a

data DominionConfig = DominionConfig {
  -- | Names and strategies for each player
  playerDefs   :: [(Text, Strategy)],
  -- | Which kingdom cards to use
  kingdomCards :: [Card],
  -- | How many games to run
  games        :: Int,
  -- | One random number generator per game
  seeds        :: [StdGen]
} deriving stock (Show, Generic)

instance Semi.Semigroup DominionConfig where
  c1 <> c2 = DominionConfig
              (playerDefs c1 ++ playerDefs c2)
              (kingdomCards c1 ++ kingdomCards c2)
              (games c1 + games c2)
              (seeds c1 ++ seeds c2)

instance Monoid DominionConfig where
  mempty = DominionConfig [] [] 0 []
  mappend = (Semi.<>)

-- | Represents the state of a single game of Dominion.
data DominionGame = DominionGame {
  -- | The players of the game.
  players :: [DominionPlayer],
  -- | All the decks, basic and Kingdom: (Card, Number Left)
  decks   :: Map.Map Card Int,
  -- | The trash pile.
  trash   :: [Card],
  -- | The current random number generator, needs to be updated when used.
  random  :: StdGen
} deriving stock (Show, Generic)

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
  decks      :: Map.Map Card Int
} deriving stock (Show, Generic)

-- | The two 'CardType's are
-- * 'Value' - The 'Card' does not require an action to play,
-- and normally givens money or victory points (not useful
-- until scoring).
-- * 'Action' - Does require an action to play, each type of action
-- 'Card' has its own logic.
data CardType = Value | Action
  deriving (Show, Eq)

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
  action   :: Card -> PlayerNumber -> DominionState (Maybe DominionMove),
  -- | Value or Action
  cardType :: CardType,
  -- | The function that determines the score for the card
  -- at the end of the game
  score    :: Card -> PlayerNumber -> DominionState Int
} deriving stock (Generic)

type instance Index Card = Card

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
  buyStrategy        :: DominionAIGame -> [DominionMove],
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
  lurkerStrategy     :: DominionAIGame -> Card -> Either Card Card
} deriving stock (Generic)

instance Show Strategy where
  show s = unpack $ strategyName s

instance Eq Strategy where
  a == b = strategyName a == strategyName b

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
  -- | The Strategy used by this player.
  strategy   :: Strategy
} deriving stock (Show, Generic)

instance Eq DominionPlayer where
  a == b = playerName a == playerName b

instance Ord DominionPlayer where
  compare p1 p2
    | victory p1 == victory p2  = turns (p1 :: DominionPlayer) `compare` turns (p2 :: DominionPlayer)
    | otherwise                 = victory p2 `compare` victory p1

data CardPlay = Standard Card | PlayThroneRoom Card | PlayRemodel Card Card | PlayCellar [Card]
  deriving (Show, Eq)

data BoughtCard = BoughtCard Card
  deriving (Show, Eq)

-- | A representation of a 'Player''s turn.
-- * Which cards were played.
-- * Which cards were bought.
data PlayerTurn = PlayerTurn
  { playerNum   :: PlayerNumber
  , cardsPlayed :: [CardPlay]
  , cardsBought :: [BoughtCard]
  } deriving (Show, Eq)

-- | A single turn in a game, what turn number it is and each 'Player''s
-- 'PlayerTurn'.
data GameTurn = GameTurn
  { number :: Int
  , playerTurns :: [PlayerTurn]
  } deriving (Show, Eq)

-- | A single game of Dominion, including all of the 'GameTurn's,
-- and the end 'Result'.
data DominionTree = DominionTree
  { turns :: [GameTurn]
  , results :: Result
  } deriving (Show, Eq)

-- | A decision for the victim of the Bandit card:
-- Did you have to trash a treasure card, and if so which one?
-- What cards did you discard?
data BanditDecision = BanditDecision
  { trashed :: Maybe Card
  , discarded :: [Card]
  } deriving (Show, Eq)
