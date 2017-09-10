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
import System.Random
import Data.List (delete, find)
import qualified Data.Map as Map

deal :: Int -> Player -> GameState -> GameState
deal 0   _ gs = gs
deal num p gs = changeTurn player (GameState (_players gs) (_decks gs) (choose (split (_random gs))))
  where (enoughDeck, discard)
          | length (_deck p') >= num = (_deck p', _discard p')
          | otherwise                = ( (_deck p') ++ (shuffle' (_discard p') (length (_discard p')) (_random gs)), [])
        (hand, deck)  = splitAt num enoughDeck
        player        = Player (_playerName p') deck discard ( (_hand p') ++ hand ) (_played p') (_actions p') (_buys p') (_money p') (_victory p')
        Just p'       = find (== p) (_players gs)
        choose (_, g) = g

changeTurn :: Player -> GameState -> GameState
changeTurn p gs = GameState (p : (delete p (_players gs)) ) (_decks gs) (_random gs)

valueCard :: Int -> Int -> Card -> Player -> GameState -> GameState
valueCard money victory c p gs = changeTurn player gs
  where player    = Player (_playerName p) (_deck p') (_discard p') (delete c (_hand p')) (c : _played p') (_actions p') (_buys p') (_money p' + money) (_victory p' + victory)
        Just p'   = find (== p) (_players gs)

basicCardAction :: Int -> Int -> Int -> Int -> Int -> Card -> Player -> GameState -> GameState
basicCardAction draw actions buys money victory c p gs = player p'
  where player (Player _ _ _ _ _ 0 _ _ _) = gs
        player _                          = changeTurn (Player (_playerName p'') (_deck p'') (_discard p'') (delete c (_hand p'')) (c : _played p'') (_actions p'' + actions) (_buys p'' + buys) (_money p'' + money) (_victory p'' + victory)) gs'
        Just p'                           = find (== p) (_players gs)
        gs'                               = deal draw p' gs
        Just p''                          = find (== p) (_players gs')

doBuy :: Int -> Int -> [Card] -> [Maybe Card]
doBuy 0 _ _ = []
doBuy n 0 _ = []
doBuy n m cs = findHighCostCard : doBuy (n - 1) (m - (cost findHighCostCard)) cs
  where findHighCostCard = find (\c -> (_cost c) <= m) cs
        cost (Just c)     = (_cost c)
        cost Nothing      = 0

doBuys :: Player -> [Card] -> GameState -> GameState
doBuys p cards gs = foldr (\mc acc -> buyCard p' mc acc) gs (doBuy (_buys p') (_money p') (removeEmptyDecks cards gs))
  where removeEmptyDecks cards gs = filter (\c -> (Map.member c (_decks gs)) && (_decks gs) Map.! c > 0) cards
        Just p'                   = find (== p) (_players gs)

decreaseCards :: Card -> Card -> Int -> Int
decreaseCards  _  _ 0 = 0
decreaseCards c1 c2 n = if (c1 == c2)
                          then n - 1
                          else n

buyCard :: Player -> Maybe Card -> GameState -> GameState
buyCard p Nothing  gs = gs
buyCard p (Just c) gs = changeTurn (player c) (decks c)
  where
    decks c   = GameState (_players gs) (Map.mapWithKey (decreaseCards c) (_decks gs) ) (_random gs)
    player c  = Player (_playerName p') (_deck p') (c : (_discard p') ) (_hand p') (_played p') (_actions p') (_buys p' - 1) (_money p' - (_cost c)) (_victory p')
    Just p'   = find (== p) (_players gs)
