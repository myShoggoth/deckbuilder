module DeckBuilding.Dominion.Utils
    ( deal
    , updatePlayer
    , numEmptyDecks
    , firstCardInPlay
    , decreaseCards
    , isCardInPlay
    ) where

import           Control.Lens
import           Control.Monad               (filterM)
import           Control.Monad.State
import           Data.List                   (delete, elemIndex, find)
import qualified Data.Map                    as Map
import           DeckBuilding.Dominion.Types
import           System.Random               (split)
import           System.Random.Shuffle

-- | Deal n cards, reshuffling the player's deck if needed.
deal :: Int -> Player -> State Game Player
deal 0   p = return p
deal num p = do
  gs <- get
  let (enoughDeck, newDiscard)
          | length (p ^. deck) >= num   = (p ^. deck, p ^. discard)
          | null (p ^. discard)         = (p ^. deck, [])
          | otherwise                   = ( (p ^. deck) ++ shuffle' (p ^. discard) (length (p ^. discard)) (gs ^. random), [])
  let (newHand, newDeck)  = splitAt num enoughDeck
  put $ over random (snd . split) gs
  updatePlayer $ set deck newDeck $ set discard newDiscard $ over hand (++ newHand) p

-- | Update the player in the game state.
updatePlayer :: Player -> State Game Player
updatePlayer p = do
  gs <- get
  let Just index = elemIndex p (gs ^. players)
  put $ over players (set (element index) p) gs
  return p

-- | How many of the game's decks have been emptied?
numEmptyDecks :: State Game Int
numEmptyDecks = do
  gs <- get
  return $ length $ Map.filter (== 0) (gs ^. decks)

-- | If the cards are the same, return number of cards - 1.
decreaseCards :: Card -> Card -> Int -> Int
decreaseCards  _  _ 0 = 0
decreaseCards c1 c2 n = if c1 == c2
    then n - 1
    else n

-- | Is this card part of this game, and if so are there any left?
isCardInPlay :: Card -> State Game Bool
isCardInPlay c = do
  gs <- get
  return $ c `Map.member` (gs ^. decks) && (gs ^. decks) Map.! c > 0

-- | Find the first card, if any, in the list which is still in play.
firstCardInPlay :: [Card] -> State Game (Maybe Card)
firstCardInPlay cs = do
  cards <- filterM isCardInPlay cs
  return $ find (const True) $ tail cards
