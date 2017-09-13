module DeckBuilding.Dominion.Utils
    ( deal
    , changeTurn
    , valueCard
    , basicCardAction
    , doBuy
    , doBuys
    , buyCard
    ) where

import DeckBuilding.Dominion.Types
import System.Random.Shuffle
import System.Random (split)
import Data.List (delete, find)
import qualified Data.Map as Map
import Control.Lens

deal :: Int -> Player -> GameState -> GameState
deal 0   _ gs = gs
deal num p gs = changeTurn player $ over random (snd . split) gs
  where (enoughDeck, newDiscard)
          | length (p' ^. deck) >= num  = (p' ^. deck, p' ^. discard)
          | otherwise                   = ( (p' ^. deck) ++ (shuffle' (p' ^. discard) (length (p' ^. discard)) (gs ^. random)), [])
        (newHand, newDeck)              = splitAt num enoughDeck
        player                          = set deck newDeck $ set discard newDiscard $ over hand (++ newHand) $ p'
        Just p'                         = find (== p) (gs ^. players)

changeTurn :: Player -> GameState -> GameState
changeTurn p gs = over players ( (p:) . (delete p) ) gs

valueCard :: Int -> Int -> Card -> Player -> GameState -> GameState
valueCard m v c p gs = changeTurn player gs
  where player    = over hand (delete c) $ over played (c:) $ over money (+m) $ over victory (+v) $ p'
        Just p'   = find (== p) (gs ^. players)

basicCardAction :: Int -> Int -> Int -> Int -> Int -> Card -> Player -> GameState -> GameState
basicCardAction draw a b m v c p gs = player p'
  where player (Player _ _ _ _ _ 0 _ _ _) = gs
        player _                          = changeTurn (over hand (delete c) $ over played (c:) $ over actions (+a) $ over buys (+b) $ over money (+m) $ over victory (+v) $ p'') gs'
        Just p'                           = find (== p) (gs ^. players)
        gs'                               = deal draw p' gs
        Just p''                          = find (== p) (gs' ^. players)

doBuy :: Int -> Int -> [Card] -> [Maybe Card]
doBuy 0 _ _ = []
doBuy n 0 _ = []
doBuy n m cs = findHighCostCard : doBuy (n - 1) (m - (mcost findHighCostCard)) cs
  where findHighCostCard = find (\c -> (c ^. cost) <= m) cs
        mcost (Just c)   = (c ^. cost)
        mcost Nothing    = 0

doBuys :: Player -> [Card] -> GameState -> GameState
doBuys p cards gs = foldr (\mc acc -> buyCard p' mc acc) gs (doBuy (p' ^. buys) (p' ^. money ) (removeEmptyDecks cards gs))
  where removeEmptyDecks cards gs = filter (\c -> (Map.member c (gs ^. decks)) && (gs ^. decks) Map.! c > 0) cards
        Just p'                   = find (== p) (gs ^. players)

decreaseCards :: Card -> Card -> Int -> Int
decreaseCards  _  _ 0 = 0
decreaseCards c1 c2 n = if (c1 == c2)
                          then n - 1
                          else n

buyCard :: Player -> Maybe Card -> GameState -> GameState
buyCard p Nothing  gs = gs
buyCard p (Just c) gs = changeTurn (player c) $ over decks (Map.mapWithKey (decreaseCards c)) gs
  where
    player c  = over discard (c:) $ over buys (+ (-1)) $ over money (\m -> m - (c ^. cost)) $ p'
    Just p'   = find (== p) (gs ^. players)
