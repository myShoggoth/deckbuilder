{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DerivingVia #-}

module DeckBuilding.Types
    ( module DeckBuilding.Types
    ) where

import Control.Monad.State.Lazy ( State )
import Data.Text ( Text )

-- | The result of a game. Includes the optional text representation
-- of the move by move description of what happened (usually
-- the output of the printer printer), the winner(s), and their score.
data Result = Result
  {
    moveByMove :: Maybe Text,
    winners :: [PlayerNumber],
    score :: Int
  }
  deriving (Show, Eq)

newtype PlayerNumber = PlayerNumber { unPlayerNumber :: Int }
  deriving (Show, Eq, Ord) via Int

class Game g where
  -- | Create the initial state of the 'Game' turn.
  start :: State g ()
  -- | Returns whether or not the 'Game' is over.
  finished :: State g Bool
  -- | Returns a list of the 'Player' numbers in order for this 'Game' turn.
  turnOrder :: State g [PlayerNumber]
