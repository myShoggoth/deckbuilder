module DeckBuilding.Dominion.Cards.Intrigue
    ( courtyardCard
    , lurkerCard
    , shantyTownCard
    , conspiratorCard
    , ironworksCard
    , dukeCard
    , haremCard
    ) where

import           DeckBuilding.Dominion.Cards.Base
import           DeckBuilding.Dominion.Cards.Utils
import           DeckBuilding.Dominion.Types
import           DeckBuilding.Dominion.Utils

import           Control.Lens
import           Control.Monad.RWS
import           Data.List                         (delete)
import qualified Data.Map                          as Map

courtyardCardAction :: Card -> Int -> DominionState Int
courtyardCardAction c p = do
  (Just player) <- preuse (players . ix p)
  (player ^. strategy . handToDeckStrategy) 1 p
  basicCardAction 3 (-1) 0 0 0 c p

courtyardCard   = Card "Courtyard"    2 courtyardCardAction Action

lurk :: Either Card Card -> Int -> DominionState Int
lurk (Left c) p                       = do
  icip <- isCardInPlay c
  if icip
    then do
      gs <- get
      trash %= (c:)
      decks %= (Map.mapWithKey (decreaseCards c))
      return p
    else return p
lurk (Right c@(Card _ _ _ Action)) p  = do
  trsh <- use trash
  if c `elem` trsh
    then do
      trash %= (delete c)
      (players . ix p . discard) %= (c:)
      return p
    else return p
lurk (Right c) p                      = return p

lurkerCardAction :: Card -> Int -> DominionState Int
lurkerCardAction c p = do
  (Just player) <- preuse (players . ix p)
  ec <- (player ^. strategy . lurkerStrategy) c p
  lurk ec p
  basicCardAction 0 0 0 0 0 c p

lurkerCard      = Card "Lurker"   2 lurkerCardAction Action

hasActionCards :: Int -> [Card] -> Bool
hasActionCards num cs = num <= length (filter (\c -> (c ^. cardType) == Action) cs)

shantyTownCardAction :: Card -> Int -> DominionState Int
shantyTownCardAction c p = do
  (Just player) <- preuse (players . ix p)
  if hasActionCards 1 (player ^. hand)
    then basicCardAction 0 1 0 0 0 c p
    else basicCardAction 2 1 0 0 0 c p

shantyTownCard  = Card "Shanty Town"  3 shantyTownCardAction Action

conspiratorCardAction :: Card -> Int -> DominionState Int
conspiratorCardAction c p = do
  (Just player) <- preuse (players . ix p)
  if hasActionCards 2 (player ^. played)
    then basicCardAction 1 0 0 2 0 c p
    else basicCardAction 0 (-1) 0 2 0 c p

conspiratorCard = Card "Conspirator"  4 conspiratorCardAction Action

ironworksCardAction :: Card -> Int -> DominionState Int
ironworksCardAction c p = do
  (Just player) <- preuse (players . ix p)
  mc <- (player ^. strategy . gainCardStrategy) 4 p
  case mc of
    Nothing   -> return p
    Just card
          | (card ^. cardType) == Action -> basicCardAction 0 0 0 0 0 c p
          | card `elem` treasureCards    -> basicCardAction 0 (-1) 0 1 0 c p
          | card `elem` victoryCards     -> basicCardAction 1 (-1) 0 0 0 c p
          | otherwise                    -> basicCardAction 0 (-1) 0 0 0 c p

ironworksCard   = Card "Ironworks"    4 ironworksCardAction Action

dukeCardAction :: Card -> Int -> DominionState Int
dukeCardAction c p = do
  (Just player) <- preuse (players . ix p)
  let points = length $ filter (== duchyCard) ( (player ^. hand) ++ (player ^. discard) ++ (player ^. played) ++ (player ^. deck) )
  valueCard 0 points c p

dukeCard        = Card "Duke"         5 dukeCardAction Action

haremCard       = Card "Harem"        6 (valueCard 2 2) Value
