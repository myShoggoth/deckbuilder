{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module DeckBuilding.Legendary.Utils
    ( deal
    , findPlayer
    ) where

import           Control.Lens
import           Control.Monad.RWS
import qualified Data.DList                   as DL
import           Data.Generics.Product
import           DeckBuilding.Legendary.Types
import           System.Random                (split)
import           System.Random.Shuffle


-- | Deal n cards, reshuffling the player's deck if needed.
deal :: Int -> Int -> LegendaryState [Card]
deal 0   _    = return []
deal num pnum = do
  p <- findPlayer pnum
  r <- use $ field @"random"
  let (enoughDeck, newDiscard)
          | length (p ^. field @"deck") >= num   = (p ^. field @"deck", p ^. field @"discard")
          | null (p ^. field @"discard")         = (p ^. field @"deck", [])
          | otherwise                   = ( (p ^. field @"deck") ++ shuffle' (p ^. field @"discard") (length (p ^. field @"discard")) r, [])
  let (newCards, newDeck)  = splitAt num enoughDeck
  field @"random" %= (snd . split)
  (field @"players" . ix pnum . field @"deck") .= newDeck
  (field @"players" . ix pnum . field @"discard") .= newDiscard
  (field @"players" . ix pnum . field @"hand") %= (++ newCards)
  tell $ DL.singleton $ Deal num newCards
  return newCards

-- | Find player # n, error if not found
findPlayer :: Int -> LegendaryState LegendaryPlayer
findPlayer p = do
  mp <- preuse(field @"players" . ix p)
  case mp of
    Just player' -> pure player'
    Nothing      -> error $ "Unable to find player #" <> show p
