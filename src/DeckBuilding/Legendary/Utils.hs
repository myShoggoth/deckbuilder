{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedLabels          #-}

module DeckBuilding.Legendary.Utils where

import           Control.Lens
import           Control.Monad.RWS
import qualified Data.DList                   as DL
import           Data.Generics.Product
import           DeckBuilding.Legendary.Types
import           System.Random                (split)
import           System.Random.Shuffle
import qualified Data.List                    as List
import           Control.Applicative          ((<|>))

-- | Deal n cards, reshuffling the player's deck if needed.
deal :: Int -> Int -> LegendaryState [HeroCard]
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

capturedAction :: VillainCard -> Int -> LegendaryState ()
capturedAction c pnum = (field @"entrance" . field @"hostages") <>= [c]

drawVillain :: Int -> Int -> LegendaryState ()
drawVillain n p = do
  vdeck <- use $ field @"villainDeck"
  if List.null vdeck
    then error "Villain deck is empty!"
    else do
      let (newVillains, newVillainDeck) = splitAt n vdeck
      (field @"villainDeck") .= newVillainDeck
      void $ sequence $ (enterVillain p) <$> newVillains
  where
    enterVillain :: Int -> VillainCard -> LegendaryState ()
    enterVillain p v =
      if isBystander v
        then capturedAction v p
        else do
          entr <- use $ field @"entrance"
          let (newCity, escapees) = advanceVillain entr v [] 
          field @"escapees" <>= escapees
          field @"entrance" .= newCity

    -- | Advance this villain one city location.
    -- If the location is empty, just move from one to another.
    -- If it is not empty, advance that villain first.
    -- If we're in the last city location, the villain escapes.
    -- Returns the new CityLocation linked list and the list of
    -- escaped cards (includes any Bystanders)
    advanceVillain :: CityLocation -> VillainCard -> [VillainCard] -> (CityLocation, [VillainCard])
    advanceVillain loc incomingv incomingb = case villain loc of
        Nothing -> (CityLocation (loc ^. #location) (Just incomingv) ( (loc ^. #hostages) <> incomingb) (loc ^. #next), [])
        Just v -> case next loc of
          Nothing -> (CityLocation (loc ^. #location) (Just incomingv) incomingb Nothing, v : hostages loc)
          Just nl -> do
            let (nnl, escapees) = advanceVillain nl v (loc ^. #hostages)
            (CityLocation (loc ^. #location) (Just incomingv) incomingb (Just nnl), escapees)

fillHq :: Int -> LegendaryState ()
fillHq p = do
  hdeck <- use $ field @"heroDeck"
  hqs :: [Maybe HeroCard] <- use $ field @"hq"
  let missing = length . filter (== Nothing) $ hqs
  let (newHeroes, newHeroDeck) = splitAt missing hdeck
  let newhq = allocate newHeroes hqs
  field @"heroDeck" .= newHeroDeck
  field @"hq" .= newhq
  where
    -- | Given a list of `a`s and a list of `Maybe a`s,
    -- put the `a`s into each Nothing until they run out.
    allocate :: [a] -> [Maybe a] -> [Maybe a]
    allocate [] xs = xs
    allocate _ [] = []
    allocate ys (x@(Just _):xs) = x : allocate ys xs
    allocate (y:ys) (Nothing:xs) =  Just y : allocate ys xs