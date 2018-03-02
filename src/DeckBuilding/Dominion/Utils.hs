module DeckBuilding.Dominion.Utils
    ( deal
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
deal :: Int -> Int -> State DominionGame [Card]
deal 0   _    = return []
deal num pnum = do
  (Just p) <- preuse (players . ix pnum)
  r <- use random
  let (enoughDeck, newDiscard)
          | length (p ^. deck) >= num   = (p ^. deck, p ^. discard)
          | null (p ^. discard)         = (p ^. deck, [])
          | otherwise                   = ( (p ^. deck) ++ shuffle' (p ^. discard) (length (p ^. discard)) r, [])
  let (newCards, newDeck)  = splitAt num enoughDeck
  random %= (snd . split)
  (players . ix pnum . deck) .= newDeck
  (players . ix pnum . discard) .= newDiscard
  (players . ix pnum . hand) %= (++ newCards)
  return newCards

-- | How many of the game's decks have been emptied?
numEmptyDecks :: State DominionGame Int
numEmptyDecks = do
  decks <- use decks
  return $ length $ Map.filter (== 0) decks

-- | If the cards are the same, return number of cards - 1.
decreaseCards :: Card -> Card -> Int -> Int
decreaseCards  _  _ 0 = 0
decreaseCards c1 c2 n = if c1 == c2
    then n - 1
    else n

-- | Is this card part of this game, and if so are there any left?
isCardInPlay :: Card -> State DominionGame Bool
isCardInPlay c = do
  gs <- get
  return $ c `Map.member` (gs ^. decks) && (gs ^. decks) Map.! c > 0

-- | Find the first card, if any, in the list which is still in play.
firstCardInPlay :: [Card] -> State DominionGame (Maybe Card)
firstCardInPlay cs = do
  cards <- filterM isCardInPlay cs
  return $ find (const True) $ tail cards
