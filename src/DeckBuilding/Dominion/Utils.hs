module DeckBuilding.Dominion.Utils
    ( deal
    , updatePlayer
    , valueCard
    , basicCardAction
    , doBuy
    , doBuys
    , buyCard
    , hasActionsLeft
    , numEmptyDecks
    ) where

import DeckBuilding.Dominion.Types
import System.Random.Shuffle
import System.Random (split)
import Data.List (delete, find)
import Data.Foldable (foldrM)
import qualified Data.Map as Map
import Control.Lens
import Control.Monad.State

deal :: Int -> Player -> State Game Player
deal 0   p = do
  return p
deal num p = do
  gs <- get
  let (enoughDeck, newDiscard)
          | length (p ^. deck) >= num   = (p ^. deck, p ^. discard)
          | otherwise                   = ( (p ^. deck) ++ (shuffle' (p ^. discard) (length (p ^. discard)) (gs ^. random)), [])
  let (newHand, newDeck)  = splitAt num enoughDeck
  let player              = set deck newDeck $ set discard newDiscard $ over hand (++ newHand) $ p
  put $ over players ( (player:) . (delete p)) $ over random (snd . split) gs
  return player

updatePlayer :: Player -> State Game Player
updatePlayer p = do
  gs <- get
  put $ over players ( (p:) . (delete p) ) gs
  return p

valueCard :: Int -> Int -> Card -> Player -> State Game Player
valueCard m v c p = do
  updatePlayer $ over hand (delete c) $ over played (c:) $ over money (+m) $ over victory (+v) $ p

hasActionsLeft :: Player -> Bool
hasActionsLeft (Player _ _ _ _ _ 0 _ _ _) = False
hasActionsLeft _                          = True

basicCardAction :: Int -> Int -> Int -> Int -> Int -> Card -> Player -> State Game Player
basicCardAction draw a b m v c p = do
  if hasActionsLeft p
    then do
      p' <- deal draw p
      let player = over hand (delete c) $ over played (c:) $ over actions (+a) $ over buys (+b) $ over money (+m) $ over victory (+v) $ p'
      updatePlayer player
    else return p

doBuy :: Int -> Int -> [Card] -> [Maybe Card]
doBuy 0 _ _ = []
doBuy n 0 _ = []
doBuy n m cs = findHighCostCard : doBuy (n - 1) (m - (mcost findHighCostCard)) cs
  where findHighCostCard = find (\c -> (c ^. cost) <= m) cs
        mcost (Just c)   = (c ^. cost)
        mcost Nothing    = 0

numEmptyDecks :: State Game Int
numEmptyDecks = do
  gs <- get
  return $ length $ Map.filter (== 0) (gs ^. decks)

doBuys :: Player -> [Card] -> State Game Player
doBuys p cards = do
  gs <- get
  let nonEmptyDecks = filter (\c -> (Map.member c (gs ^. decks)) && (gs ^. decks) Map.! c > 0) cards
  foldrM (\mc player -> buyCard mc player) p (doBuy (p ^. buys) (p ^. money ) nonEmptyDecks)

decreaseCards :: Card -> Card -> Int -> Int
decreaseCards  _  _ 0 = 0
decreaseCards c1 c2 n = if (c1 == c2)
                          then n - 1
                          else n

buyCard ::  Maybe Card -> Player -> State Game Player
buyCard Nothing  p = return p
buyCard (Just c) p = do
  gs <- get
  put $ over decks (Map.mapWithKey (decreaseCards c)) gs
  let p' = over discard (c:) $ over buys (+ (-1)) $ over money (\m -> m - (c ^. cost)) $ p
  updatePlayer $ p'
  return p'
