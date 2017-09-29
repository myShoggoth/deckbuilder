module DeckBuilding.Dominion.Cards.Intrigue
    ( courtyardCard
    , lurkerCard
    ) where

import DeckBuilding.Dominion.Types
import DeckBuilding.Dominion.Cards.Utils
import DeckBuilding.Dominion.Utils

import Control.Lens
import Control.Monad.State
import qualified Data.Map                    as Map
import Data.List (delete)

courtyardCardAction :: Card -> Player -> State Game Player
courtyardCardAction c p = do
  p' <- basicCardAction 3 (-1) 0 0 0 c p
  (p' ^. strategy . handToDeckStrategy) 1 p'

courtyardCard   = Card "Courtyard"    2 courtyardCardAction Action

lurk :: Either Card Card -> Player -> State Game Player
lurk (Left c) p                       = do
  icip <- isCardInPlay c
  if icip
    then do
      gs <- get
      put $ over trash (c:) $ over decks (Map.mapWithKey (decreaseCards c)) gs
      return p
    else return p
lurk (Right c@(Card _ _ _ Action)) p  = do
  gs <- get
  if c `elem` (gs ^. trash)
    then do
      put $ over trash (delete c) gs
      return $ over discard (c:) p
    else return p
lurk (Right c) p                      = return p

lurkerCardAction :: Card -> Player -> State Game Player
lurkerCardAction c p = do
  ec <- (p ^. strategy . lurkerStrategy) c p
  lurk ec p

lurkerCard      = Card "Lurker"   2 lurkerCardAction Action
