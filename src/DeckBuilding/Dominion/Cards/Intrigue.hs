module DeckBuilding.Dominion.Cards.Intrigue
    ( courtyardCard
    , lurkerCard
    , shantyTownCard
    , conspiratorCard
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

hasActionCards :: Int -> [Card] -> Bool
hasActionCards num cs = (num <= length (filter (\c -> (c ^. cardType) == Action) cs))

shantyTownCardAction :: Card -> Player -> State Game Player
shantyTownCardAction c p = if hasActionCards 1 (p ^. hand)
  then basicCardAction 0 1 0 0 0 c p
  else basicCardAction 2 1 0 0 0 c p

shantyTownCard  = Card "Shanty Town"  3 shantyTownCardAction Action

conspiratorCardAction :: Card -> Player -> State Game Player
conspiratorCardAction c p = if hasActionCards 2 (p ^. played)
  then basicCardAction 1 0 0 2 0 c p
  else basicCardAction 0 (-1) 0 2 0 c p

conspiratorCard = Card "Conspirator"  4 conspiratorCardAction Action
