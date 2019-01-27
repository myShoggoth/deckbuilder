module DeckBuilding.Legendary.Utils
    ( deal
    , findPlayer
    ) where

import           Control.Lens
import           Control.Monad.RWS
import qualified Data.DList                  as DL
import           DeckBuilding.Legendary.Types
import           System.Random               (split)
import           System.Random.Shuffle

-- | Deal n cards, reshuffling the player's deck if needed.
deal :: Int -> Int -> LegendaryState [Card]
deal 0   _    = return []
deal num pnum = do
  p <- findPlayer pnum
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
  tell $ DL.singleton $ Deal num newCards
  return newCards

-- | Find player # n, error if not found
findPlayer :: Int -> LegendaryState (LegendaryPlayer)
findPlayer p = do
  mp <- preuse(players . ix p)
  case mp of
    Just player' -> pure player'
    Nothing -> error $ "Unable to find player #" <> show p
