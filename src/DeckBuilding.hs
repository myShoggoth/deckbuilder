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
module DeckBuilding
    ( runTurns
    , runGame
    ) where

import           Control.Monad.RWS

import           DeckBuilding.Types

runTurns :: Game c l g => [Int] -> Bool -> RWS c l g Bool
runTurns _      True  = return True
runTurns []     False = return False
runTurns (x:xs) False = runTurn x >>= runTurns xs

runGame :: Game c l g => Bool -> RWS c l g Result
runGame True  = result
runGame False = turnOrder >>= flip runTurns False >>= runGame
