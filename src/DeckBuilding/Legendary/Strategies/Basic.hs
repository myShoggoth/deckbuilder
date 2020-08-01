{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}

module DeckBuilding.Legendary.Strategies.Basic
    ( dumbStrategy
    ) where

import           Control.Lens
import           Data.Generics.Product
import           Data.List                              (delete, intersect,
                                                         (\\), sortOn)
import qualified Data.Map                               as Map
import           Safe (headMay)
import           DeckBuilding.Legendary.Cards.Base
import           DeckBuilding.Legendary.Cards.Utils
import           DeckBuilding.Legendary.Types
import           DeckBuilding.Legendary.Utils

-- Strategies

-- Really dumb strategy
dumbStrategy :: Strategy
dumbStrategy = Strategy "Really Dumb"
                            dumbBuy
                            dumbNextCard
                            dumbDiscard
                            dumbTrash
                            dumbRetrieve
                            dumbOrderHand
                            dumbGain
                            dumbAttack
                            dumbKONOf
                            dumbRecruitNStrategy
                            dumbOthersDrawOrDiscardStrategy

dumbBuy :: Int -> LegendaryState Int
dumbBuy pnum = do
    p <- findPlayer pnum
    hqs <- use $ field @"hq"
    let (unused, bought, hq') = doBuy (p ^. #unusedMoney) [] hqs
    (field @"players" . ix pnum . field @"unusedMoney") .= unused
    (field @"players" . ix pnum . field @"discard") <>= bought
    field @"hq" .= hq'
    pure pnum
  where
      doBuy 0 b theHq = (0, b, theHq)
      doBuy n b [] = (n, b, [])
      doBuy n b (Nothing:hqrest) =
          let (unused', b', hqrest') = doBuy n b hqrest
          in (unused', b', Nothing:hqrest')
      doBuy n b (x@(Just h):hqrest) =
          if cost h <= n
              then let (unused', b', hqrest') = doBuy (n - cost h) b hqrest
                   in (unused', h:b', Nothing:hqrest')
              else let (unused', b', hqrest') = doBuy n b hqrest
                   in (unused', b', x:hqrest')

dumbNextCard :: Int -> LegendaryState (Maybe HeroCard)
dumbNextCard pnum = do
    p <- findPlayer pnum
    return $ headMay $ p ^. #hand

dumbDiscard :: (Int, Int) -> Int -> LegendaryState [HeroCard]
dumbDiscard (_, _) pnum = undefined

dumbTrash :: (Int, Int) -> Int -> LegendaryState [HeroCard]
dumbTrash (_, _) pnum = undefined

dumbRetrieve :: (Int, Int) -> Int -> LegendaryState [HeroCard]
dumbRetrieve (_, _) pnum = undefined

dumbOrderHand :: Int -> LegendaryState [HeroCard]
dumbOrderHand pnum = undefined

dumbGain :: Int -> Int -> LegendaryState (Maybe HeroCard)
dumbGain cost pnum = undefined

dumbAttack :: Int -> LegendaryState Int
dumbAttack pnum = pure pnum

dumbKONOf :: (Int, Int) -> [HeroCard] -> Int -> LegendaryState ([HeroCard], [HeroCard])
dumbKONOf (_, _) cs pnum = undefined

dumbRecruitNStrategy :: [HeroCard] -> Int -> Int -> Int -> LegendaryState [HeroCard]
dumbRecruitNStrategy [] _ _ _ = pure []
dumbRecruitNStrategy _ 0 _ _ = pure []
dumbRecruitNStrategy possibles number maxcost pnum = do
    pure $ take number $ sortOn cost $ filter (\x -> cost x <= maxcost) possibles

dumbOthersDrawOrDiscardStrategy :: Int -> Int -> LegendaryState DrawDiscardChoice
dumbOthersDrawOrDiscardStrategy _ _ = pure DrawChoice