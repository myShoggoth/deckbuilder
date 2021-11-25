{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : DeckBuilding
Description : A deck-building game engine and simulator
Copyright   : (c) Andrew F. Boardman, 2017
License     : GPL-3
Maintainer  : andrew@myshoggoth.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module DeckBuilding (deal') where

import System.Random (StdGen, split)
import System.Random.Shuffle (shuffle')

-- | Deal n cards, reshuffling the player's deck if needed.
deal' :: StdGen -> [a] -> [a] -> Int -> (StdGen, [a], [a], [a])
deal' g deck discard 0 = (g, [], deck, discard)
deal' g deck discard n =
    ( snd . split $ g
    , newCards
    , newDeck
    , newDiscard
    )
    where
        (enoughDeck, newDiscard)
            | length deck >= n   = (deck, discard)
            | null discard       = (deck, [])
            | otherwise          = (deck ++ shuffle' discard (length discard) g, [])
        (newCards, newDeck)  = splitAt n enoughDeck
