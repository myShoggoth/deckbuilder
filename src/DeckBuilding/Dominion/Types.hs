{-# LANGUAGE TemplateHaskell #-}

module DeckBuilding.Dominion.Types
    ( module DeckBuilding.Dominion.Types
    ) where

import qualified Data.Map as Map
import System.Random
import Control.Lens
import Control.Monad.State

data Game = Game {
  _players  :: [Player],
  _decks    :: Map.Map Card Int,
  _random   :: StdGen
} deriving Show

data CardType = Value | Action
  deriving (Show, Eq)

data Card = Card {
  _cardName :: String,
  _cost     :: Int,
  _action   :: Card -> Player -> State Game Player,
  _cardType :: CardType
}

instance Ord Card where
  compare c1 c2 = compare (_cardName c1) (_cardName c2)

instance Eq Card where
  a == b = _cardName a == _cardName b

instance Show Card where
  show c = _cardName c

data Strategy = Strategy {
  _strategyName     :: String,
  _buyStrategy      :: Player -> State Game Player,
  _discardStrategy  :: (Int, Int) -> Player -> State Game Player,
  _trashStrategy    :: (Int, Int) -> Player -> State Game Player,
  _retrieveStrategy :: (Int, Int) -> Player -> State Game Player,
  _orderHand        :: Player -> State Game Player,
  _gainCardStrategy :: Int -> Player -> State Game Player
}

instance Show Strategy where
  show s = _strategyName s

instance Eq Strategy where
  a == b = _strategyName a == _strategyName b

data Player = Player {
  _playerName :: String,
  _deck       :: [Card],
  _discard    :: [Card],
  _hand       :: [Card],
  _played     :: [Card],
  _actions    :: Int,
  _buys       :: Int,
  _money      :: Int,
  _victory    :: Int,
  _strategy   :: Strategy
} deriving Show

instance Eq Player where
  a == b = _playerName a == _playerName b

instance Ord Player where
  compare p1 p2 = compare (_playerName p1) (_playerName p2)

type Result = Either String Int

makeLenses ''Game
makeLenses ''Card
makeLenses ''Strategy
makeLenses ''Player
