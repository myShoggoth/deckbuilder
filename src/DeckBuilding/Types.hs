{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

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

class (Monoid c, Monoid l) => Game c l g where
  start :: RWS c l g ()
  finished :: RWS c l g Bool
  result :: RWS c l g Result
  runTurn :: Int -> RWS c l g Bool
  turnOrder :: RWS c l g [Int]
  tallyPoints :: Int -> RWS c l g ()
