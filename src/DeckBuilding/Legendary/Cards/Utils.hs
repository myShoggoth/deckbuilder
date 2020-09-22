{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module DeckBuilding.Legendary.Cards.Utils where

import DeckBuilding.Legendary.Types
    ( LegendaryPlayer(hand),
      VillainCard,
      HeroCard(HeroCard),
      HeroClass,
      HeroTeam(NoTeam),
      LegendaryGame(hq),
      CityLocation,
      LegendaryState )
import DeckBuilding.Legendary.Utils ( deal, findPlayer )
import Data.Generics.Product ( HasField(field) )
import Control.Lens
    ( (^..),
      folded,
      (^.),
      use,
      (%=),
      (+=),
      (-=),
      (.=),
      (<>=),
      Ixed(ix) )
import Data.List (delete)
import Control.Monad (forM_)
import Control.Monad.RWS ( MonadState(put, get) )

-- | For value cards, pass the money value.
-- | Money Value
-- | Attack Value
-- | Card
-- | Player Number
valueCard :: Int -> Int -> HeroCard -> Int -> LegendaryState Int
valueCard m pew c p = do
  (field @"players" . ix p . field @"hand") %= delete c
  (field @"players" . ix p . field @"played") %= (c:)
  (field @"players" . ix p . field @"unusedMoney") += m
  (field @"players" . ix p . field @"unusedAttack") += pew
  return p

-- | For basic card values: draw cards, +money, +attack
basicCardAction :: Int -> Int -> Int -> HeroCard -> Int -> LegendaryState Int
basicCardAction draw pew m c p = do
  _ <- deal draw p
  valueCard m pew c p

wound :: HeroCard
wound = HeroCard "Wound" 0 nop [] NoTeam

nop :: HeroCard -> Int -> LegendaryState Int
nop _ p = return p

koNOfTopofDeck :: Int -> Int -> VillainCard -> CityLocation -> Int -> LegendaryState Int
koNOfTopofDeck cards _replace _ _ pnum = do
  cs <- deal cards pnum
  p <- findPlayer pnum
  (toKO, toReplace) <- (p ^. #strategy . #koNOfStrategy) (cards, cards) cs pnum
  (field @"koPile") <>= toKO
  (field @"players" . ix pnum . field @"deck") %= (toReplace <>)
  pure pnum

gainRecruit :: Int -> VillainCard -> CityLocation -> Int -> LegendaryState Int
gainRecruit r _ _ pnum = do
  (field @"players" . ix pnum . field @"unusedMoney") += r
  pure pnum

koNFromHand :: Int -> VillainCard -> CityLocation -> Int -> LegendaryState Int
koNFromHand n _ _ pnum = do
  p <- findPlayer pnum
  (toKO, theRest) <- (p ^. #strategy . #koNOfStrategy) (n, n) (hand p) pnum
  (field @"koPile") <>= toKO
  (field @"players" . ix pnum . field @"hand") .= theRest
  pure pnum

adjustNextTurnCards :: Int -> VillainCard -> CityLocation -> Int -> LegendaryState Int
adjustNextTurnCards antc _ _ pnum = do
  (field @"players" . ix pnum . field @"nextTurnCards") += antc
  pure pnum

vpPerClass :: HeroClass -> VillainCard -> Int -> LegendaryState Int
vpPerClass cl _ pnum = do
  p <- findPlayer pnum
  let allCards = ((p ^. field @"deck") <> (p ^. field @"discard") <> (p ^. field @"played") <> (p ^. field @"hand")) ^.. folded . field @"heroClass" . folded
  pure $ length $ filter (== cl) allCards

classOrWound :: HeroClass -> VillainCard -> Int -> LegendaryState ()
classOrWound cl _ _ = do
  ps <- use $ field @"players"
  forM_ [0 .. length ps - 1] $ \pnum -> do
    p <- findPlayer pnum
    if 0 < (length $ filter (== cl) $ (p ^. field @"hand") ^.. folded . field @"heroClass" . folded)
      then (field @"players" . ix pnum . field @"discard") %= (wound:)
      else pure ()

emptyWoundPile :: LegendaryState Bool
emptyWoundPile = do
  gs <- get
  return $ 0 > (gs ^. #wounds)

recruitN :: (HeroCard -> Bool) -> Int -> Int -> VillainCard -> CityLocation -> Int -> LegendaryState Int
recruitN f n cost _ _ pnum = do
  gs <- get
  p <- findPlayer pnum
  let possibles = filter f (gs ^. #heroDeck)
  chosen <- (p ^. #strategy . #recruitNStrategy) possibles n cost pnum
  removeFromHq chosen
  (field @"players" . ix pnum . #discard) <>= chosen
  (field @"players" . ix pnum . #unusedMoney) -= (cost * length chosen)
  pure pnum

removeFromHq :: [HeroCard] -> LegendaryState ()
removeFromHq hcs = do -- Why doesn't '(field @"hq") %= removeHero hcs' work?!
  gs <- get
  let gs' = gs { hq = removeHero <$> gs ^. #hq }
  put gs'
  where removeHero :: Maybe HeroCard -> Maybe HeroCard
        removeHero mhc =
          case mhc of
            Nothing -> Nothing
            Just hc -> if hc `elem` hcs
                         then Nothing
                         else Just hc