{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module Dominion.Utils
    ( evaluateHandHelper
    ) where

import           Control.Lens
import           Data.Generics.Product
import           DeckBuilding.Dominion
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils

evaluateHandHelper :: Int -> DominionState Int
evaluateHandHelper pnum = do
  player <- findPlayer pnum
  evaluateHand pnum player (player ^. field @"hand")
