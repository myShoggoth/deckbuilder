{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE BlockArguments            #-}

module Dominion.Utils
    (  defaultConfig
    , initialState
    , p0
    , p1
    , setHand
    , setDeck
    ) where
import DeckBuilding.Dominion.Types (DominionConfig (DominionConfig), DominionState, DominionBoard (DominionBoard), Card (Card))
import DeckBuilding.Types (PlayerNumber (unPlayerNumber, PlayerNumber), Game (start))
import Control.Lens (ix, (.=))
import DeckBuilding.Dominion.Cards (firstGameKingdomCards)
import Control.Monad.State (runState, execState)
import DeckBuilding.Dominion (configToGame)
import System.Random (mkStdGen)
import DeckBuilding.Dominion.Strategies.Basic (bigMoneyStrategy, bigSmithyStrategy)

defaultConfig :: DominionConfig
defaultConfig = DominionConfig
  [ ("Player 1", bigMoneyStrategy)
  , ("Player 2", bigSmithyStrategy)
  ]
  firstGameKingdomCards

initialState :: DominionConfig -> DominionState a -> (a, DominionBoard)
initialState c f = runState f $ execState start $ configToGame c $ mkStdGen 45752345316

p0 :: PlayerNumber
p0 = PlayerNumber 0

p1 :: PlayerNumber
p1 = PlayerNumber 1

setHand :: PlayerNumber -> [Card] -> DominionState ()
setHand p xs = #players . ix (unPlayerNumber p) . #hand .= xs

setDeck :: PlayerNumber -> [Card] -> DominionState ()
setDeck p xs = #players . ix (unPlayerNumber p) . #deck .= xs
