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

import Control.Lens
    ( preuse, (^.), use, (%=), (.=), (<>=), Ixed(ix) )
import Data.Generics.Product ( HasField(field) )
import Control.Monad.RWS ( void )
import qualified Data.DList as DL
import DeckBuilding.Types ( PlayerNumber(unPlayerNumber) )
import DeckBuilding.Legendary.Types
    ( CityLocation(CityLocation, villain, next, hostages),
      HeroCard,
      LegendaryMove(Deal),
      LegendaryPlayer,
      LegendaryState,
      VillainCard(isBystander) )
import System.Random (split)
import System.Random.Shuffle ( shuffle' )
import qualified Data.List as List

-- | Deal n cards, reshuffling the player's deck if needed.
deal :: Int -> PlayerNumber -> LegendaryState [HeroCard]
deal 0   _    = return []
deal num pnum = do
  p <- findPlayer pnum
  r <- use $ #random
  let (r', newCards, newDeck, newDiscard) = deal' r (p ^. #deck) (p ^. #discard) num
  field @"random" .= r'
  (field @"players" . ix (unPlayerNumber pnum) . #deck) .= newDeck
  (field @"players" . ix (unPlayerNumber pnum) . #discard) .= newDiscard
  (field @"players" . ix (unPlayerNumber pnum) . #hand) %= (++ newCards)
  return newCards

-- | Find player # n, error if not found
findPlayer :: PlayerNumber -> LegendaryState LegendaryPlayer
findPlayer p = do
  mp <- preuse(field @"players" . ix (unPlayerNumber p))
  case mp of
    Just player' -> pure player'
    Nothing      -> error $ "Unable to find player #" <> show p

capturedAction :: VillainCard -> PlayerNumber -> LegendaryState ()
capturedAction c _ = (#entrance . #hostages) <>= [c]

drawVillain :: Int -> PlayerNumber -> LegendaryState ()
drawVillain n p = do
  vdeck <- use $ #villainDeck
  if List.null vdeck
    then error "Villain deck is empty!"
    else do
      let (newVillains, newVillainDeck) = splitAt n vdeck
      (field @"villainDeck") .= newVillainDeck
      void $ sequence $ enterVillain <$> newVillains
  where
    enterVillain :: VillainCard -> LegendaryState ()
    enterVillain v =
      if isBystander v
        then capturedAction v p
        else do
          entr <- use $ #entrance
          let (newCity,escaped) = advanceVillain entr v [] 
          field @"escapees" <>= escaped
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
            let (nnl, escaped) = advanceVillain nl v (loc ^. #hostages)
            (CityLocation (loc ^. #location) (Just incomingv) incomingb (Just nnl), escaped)

fillHq :: PlayerNumber -> LegendaryState ()
fillHq _ = do
  hdeck <- use $ #heroDeck
  hqs :: [Maybe HeroCard] <- use $ #hq
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