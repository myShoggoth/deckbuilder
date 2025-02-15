{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}

module DeckBuilding.Legendary.Strategies.Basic
    ( dumbStrategy
    ) where

import Control.Lens ( (^.), use, (.=), (<>=), Ixed(ix) )
import Data.Generics.Product ( HasField(field) )
import Data.List (sortOn)
import Safe (headMay)
import DeckBuilding.Types ( PlayerNumber(unPlayerNumber) )
import DeckBuilding.Legendary.Types
    ( Strategy(Strategy),
      DrawDiscardChoice(DrawChoice),
      HeroCard(cost),
      LegendaryState )
import DeckBuilding.Legendary.Utils ( findPlayer )

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

dumbBuy :: PlayerNumber -> LegendaryState PlayerNumber
dumbBuy pnum = do
    p <- findPlayer pnum
    hqs <- use $ #hq
    let (unused, bought, hq') = doBuy (p ^. #unusedMoney) [] hqs
    (#players . ix (unPlayerNumber pnum) . #unusedMoney) .= unused
    (#players . ix (unPlayerNumber pnum) . #discard) <>= bought
    #hq .= hq'
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

dumbNextCard :: PlayerNumber -> LegendaryState (Maybe HeroCard)
dumbNextCard pnum = do
    p <- findPlayer pnum
    return $ headMay $ p ^. #hand

dumbDiscard :: (Int, Int) -> PlayerNumber -> LegendaryState [HeroCard]
dumbDiscard (_, _) _ = undefined

dumbTrash :: (Int, Int) -> PlayerNumber -> LegendaryState [HeroCard]
dumbTrash (_, _) _ = undefined

dumbRetrieve :: (Int, Int) -> PlayerNumber -> LegendaryState [HeroCard]
dumbRetrieve (_, _) _ = undefined

dumbOrderHand :: PlayerNumber -> LegendaryState [HeroCard]
dumbOrderHand _ = undefined

dumbGain :: Int -> PlayerNumber -> LegendaryState (Maybe HeroCard)
dumbGain _ _ = undefined

dumbAttack :: PlayerNumber -> LegendaryState PlayerNumber
dumbAttack = pure

dumbKONOf :: (Int, Int) -> [HeroCard] -> PlayerNumber -> LegendaryState ([HeroCard], [HeroCard])
dumbKONOf (_, _) _ _ = undefined

dumbRecruitNStrategy :: [HeroCard] -> Int -> Int -> PlayerNumber -> LegendaryState [HeroCard]
dumbRecruitNStrategy [] _ _ _ = pure []
dumbRecruitNStrategy _ 0 _ _ = pure []
dumbRecruitNStrategy possibles number maxcost _ = do
    pure $ take number $ sortOn cost $ filter (\x -> cost x <= maxcost) possibles

dumbOthersDrawOrDiscardStrategy :: Int -> PlayerNumber -> LegendaryState DrawDiscardChoice
dumbOthersDrawOrDiscardStrategy _ _ = pure DrawChoice