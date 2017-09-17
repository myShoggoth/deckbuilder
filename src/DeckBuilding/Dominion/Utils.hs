module DeckBuilding.Dominion.Utils
    ( deal
    , updatePlayer
    , hasActionsLeft
    , numEmptyDecks
    ) where

import DeckBuilding.Dominion.Types
import System.Random.Shuffle
import System.Random (split)
import Data.List (delete, find, elemIndex)
import qualified Data.Map as Map
import Control.Lens
import Control.Monad.State

deal :: Int -> Player -> State Game Player
deal 0   p = return p
deal num p = do
  gs <- get
  let (enoughDeck, newDiscard)
          | length (p ^. deck) >= num   = (p ^. deck, p ^. discard)
          | otherwise                   = ( (p ^. deck) ++ (shuffle' (p ^. discard) (length (p ^. discard)) (gs ^. random)), [])
  let (newHand, newDeck)  = splitAt num enoughDeck
  let player              = set deck newDeck $ set discard newDiscard $ over hand (++ newHand) $ p
  put $ over random (snd . split) gs
  updatePlayer player
  return player

updatePlayer :: Player -> State Game Player
updatePlayer p = do
  gs <- get
  let Just index = elemIndex p (gs ^. players)
  put $ over players (set (element index) p) gs
  return p

hasActionsLeft :: Player -> Bool
hasActionsLeft (Player _ _ _ _ _ 0 _ _ _ _) = False
hasActionsLeft _                            = True

numEmptyDecks :: State Game Int
numEmptyDecks = do
  gs <- get
  return $ length $ Map.filter (== 0) (gs ^. decks)
