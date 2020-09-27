{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DerivingVia #-}

module DeckBuilding.Types
    ( module DeckBuilding.Types
    ) where

import           Control.Monad.RWS
import           Data.Text

-- | The result of a game. Either Left ("Player Name", Points) who is the winner, or
--  Right Int which is the number of players that tied for the lead.
type Result = Either (Text, Int) Int

instance {-# OVERLAPPING #-} Eq Result where 
  (Left (s1, _)) == (Left (s2, _)) = s1 == s2
  (Right n1)     == (Right n2)     = n1 == n2
  _              == _              = False

newtype PlayerNumber = PlayerNumber { unPlayerNumber :: Int }
  deriving (Show, Eq) via Int

class (Monoid c, Monoid l) => Game c l g where
  -- | Create the initial state of the 'Game' turn.
  start :: RWS c l g ()
  -- | Returns whether or not the 'Game' is over.
  finished :: RWS c l g Bool
  -- | Calculate the end 'Result' scores.
  result :: RWS c l g Result
  -- | Run a turn for 'Player' n.
  runTurn :: PlayerNumber -> RWS c l g Bool
  -- | Returns a list of the 'Player' numbers in order for this 'Game' turn.
  turnOrder :: RWS c l g [PlayerNumber]
  -- | Count up the total points for 'Player' n.
  tallyPoints :: PlayerNumber -> RWS c l g ()
