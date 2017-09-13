{-# LANGUAGE TemplateHaskell #-}

module DeckBuilding.Dominion.Types
    ( module DeckBuilding.Dominion.Types
    ) where

import qualified Data.Map as Map
import System.Random
import Control.Lens

data GameState = GameState {
  _players  :: [Player],
  _decks    :: Map.Map Card Int,
  _random   :: StdGen
} deriving Show

--instance Show GameState where
--  show gs = show (_players gs) ++ " " ++ show (_decks gs)

data CardType = Value | Action
  deriving Show

data Card = Card {
  _cardName :: String,
  _cost     :: Int,
  _action   :: Card -> Player -> GameState -> GameState,
  _cardType :: CardType
}

instance Ord Card where
  compare c1 c2 = compare (_cardName c1) (_cardName c2)

instance Eq Card where
  a == b = _cardName a == _cardName b

instance Show Card where
  show c = _cardName c

data Player = Player {
  _playerName :: String,
  _deck       :: [Card],
  _discard    :: [Card],
  _hand       :: [Card],
  _played     :: [Card],
  _actions    :: Int,
  _buys       :: Int,
  _money      :: Int,
  _victory    :: Int
} deriving Show

instance Eq Player where
  a == b = _playerName a == _playerName b

instance Ord Player where
  compare p1 p2 = compare (_playerName p1) (_playerName p2)

type Result = Either String Int

makeLenses ''GameState
makeLenses ''Card
makeLenses ''Player
