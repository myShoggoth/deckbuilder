module DeckBuilding.Dominion.Cards
    ( module X
    , actionTerminatorCards
    ) where

import DeckBuilding.Dominion.Cards.Base as X
import DeckBuilding.Dominion.Cards.Intrigue as X
import DeckBuilding.Dominion.Types ( Card )

actionTerminatorCards :: [Card]
actionTerminatorCards = X.baseSetActionTerminators
