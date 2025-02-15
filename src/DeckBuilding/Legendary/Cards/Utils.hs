{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}

module DeckBuilding.Legendary.Cards.Utils where

import DeckBuilding.Types ( PlayerNumber(PlayerNumber, unPlayerNumber) )
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
valueCard :: Int -> Int -> HeroCard -> PlayerNumber -> LegendaryState PlayerNumber
valueCard m pew c p = do
  (#players . ix (unPlayerNumber p) . #hand) %= delete c
  (#players . ix (unPlayerNumber p) . #played) %= (c:)
  (#players . ix (unPlayerNumber p) . #unusedMoney) += m
  (#players . ix (unPlayerNumber p) . #unusedAttack) += pew
  return p

-- | For basic card values: draw cards, +money, +attack
basicCardAction :: Int -> Int -> Int -> HeroCard -> PlayerNumber -> LegendaryState PlayerNumber
basicCardAction draw pew m c p = do
  _ <- deal draw p
  valueCard m pew c p

wound :: HeroCard
wound = HeroCard "Wound" 0 nop [] NoTeam

nop :: HeroCard -> PlayerNumber -> LegendaryState PlayerNumber
nop _ p = return p

koNOfTopofDeck :: Int -> Int -> VillainCard -> CityLocation -> PlayerNumber -> LegendaryState PlayerNumber
koNOfTopofDeck cards _replace _ _ pnum = do
  cs <- deal cards pnum
  p <- findPlayer pnum
  (toKO, toReplace) <- (p ^. #strategy . #koNOfStrategy) (cards, cards) cs pnum
  #koPile <>= toKO
  (#players . ix (unPlayerNumber pnum) . #deck) %= (toReplace <>)
  pure pnum

gainRecruit :: Int -> VillainCard -> CityLocation -> PlayerNumber -> LegendaryState PlayerNumber
gainRecruit r _ _ pnum = do
  (#players . ix (unPlayerNumber pnum) . #unusedMoney) += r
  pure pnum

koNFromHand :: Int -> VillainCard -> CityLocation -> PlayerNumber -> LegendaryState PlayerNumber
koNFromHand n _ _ pnum = do
  p <- findPlayer pnum
  (toKO, theRest) <- (p ^. #strategy . #koNOfStrategy) (n, n) (hand p) pnum
  #koPile <>= toKO
  (#players . ix (unPlayerNumber pnum) . #hand) .= theRest
  pure pnum

adjustNextTurnCards :: Int -> VillainCard -> CityLocation -> PlayerNumber -> LegendaryState PlayerNumber
adjustNextTurnCards antc _ _ pnum = do
  (#players . ix (unPlayerNumber pnum) . #nextTurnCards) += antc
  pure pnum

vpPerClass :: HeroClass -> VillainCard -> PlayerNumber -> LegendaryState Int
vpPerClass cl _ pnum = do
  p <- findPlayer pnum
  let allCards = ((p ^. #deck) <> (p ^. #discard) <> (p ^. #played) <> (p ^. #hand)) ^.. folded . #heroClass . folded
  pure $ length $ filter (== cl) allCards

classOrWound :: HeroClass -> VillainCard -> PlayerNumber -> LegendaryState ()
classOrWound cl _ _ = do
  ps <- use $ #players
  forM_ [0 .. length ps - 1] $ \pnum -> do
    p <- findPlayer $ PlayerNumber pnum
    if 0 < (length $ filter (== cl) $ (p ^. #hand) ^.. folded . #heroClass . folded)
      then (#players . ix pnum . #discard) %= (wound:)
      else pure ()

emptyWoundPile :: LegendaryState Bool
emptyWoundPile = do
  gs <- get
  return $ 0 > (gs ^. #wounds)

recruitN :: (HeroCard -> Bool) -> Int -> Int -> VillainCard -> CityLocation -> PlayerNumber -> LegendaryState PlayerNumber
recruitN f n cost _ _ pnum = do
  gs <- get
  p <- findPlayer pnum
  let possibles = filter f (gs ^. #heroDeck)
  chosen <- (p ^. #strategy . #recruitNStrategy) possibles n cost pnum
  removeFromHq chosen
  (#players . ix (unPlayerNumber pnum) . #discard) <>= chosen
  (#players . ix (unPlayerNumber pnum) . #unusedMoney) -= (cost * length chosen)
  pure pnum

removeFromHq :: [HeroCard] -> LegendaryState ()
removeFromHq hcs = do -- Why doesn't '(#hq) %= removeHero hcs' work?!
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