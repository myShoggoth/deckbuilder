{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}

{-|
Module      : DeckBuilding.Legendary
Description : A deck-building game engine and simulator
Copyright   : (c) Andrew F. Boardman, 2017
License     : GPL-3
Maintainer  : andrew@myshoggoth.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@. Totes.
-}
module DeckBuilding.Legendary
  ( configToGame
  , evaluateHand
  , resetTurn
  ) where

import Control.Lens
    ( (^..), (^.), use, (%=), (+=), (.=), Ixed(ix) )
import Control.Monad.RWS
    ( void, MonadState(get) )
import qualified Data.DList as DL
import qualified Data.List as L
import qualified Data.Text as Text
import Data.Generics.Product ( HasField(field) )
import DeckBuilding.Legendary.Types
    ( LegendaryPlayer(LegendaryPlayer, playerName, victory),
      Strategy,
      LegendaryGame(LegendaryGame),
      Scheme(twists),
      LegendaryConfig(playerDefs, theMastermind),
      LegendaryState,
      LegendaryMove(Play, Turn) )
import DeckBuilding.Legendary.Utils
    ( deal, findPlayer, drawVillain, fillHq )
import DeckBuilding.Legendary.Cards.Base
    ( shieldAgent, shieldTrooper, masterStrike, schemeTwist )
import DeckBuilding.Types ( Game(..), PlayerNumber(PlayerNumber, unPlayerNumber) )
import System.Random (StdGen, split)
import System.Random.Shuffle (shuffle')

{-
doTurn -
  draw villain -- only this part is legendary specific -- basically a game state update
    run villain action if any
    push villains to next city square if needed
    villain escape action if applicable and there is one
    mastermind escape action if applicable and there is one
  loop of
    play card
    buys
    attacks
  deal cards for new turn
  reset player data for new turn
  is game over?
gameResult
-}

-- | Creates a new player with a name and strategy and the default started deck.
newPlayer :: Text.Text -> Strategy -> LegendaryPlayer
newPlayer n = LegendaryPlayer n [] (replicate 8 shieldAgent ++ replicate 4 shieldTrooper) [] [] 0 0 6 [] 0 0

{- |
  Evaluates the cards in the deck. Since cards can cause more to be drawn,
  the default case is to run a card and then recursively call with the new
  hand for the player.
-}
evaluateHand :: PlayerNumber -> LegendaryState PlayerNumber
evaluateHand pnum = do
  thePlayer <- findPlayer pnum
  mc <- (thePlayer ^. #strategy . #nextCard) pnum
  case mc of
    Nothing -> return pnum
    Just c -> do
      _ <- (c ^. #action) c pnum
      evaluateHand pnum

-- | Move played cards to discard pile, reset actions, buys, money, victory.
resetTurn :: PlayerNumber -> LegendaryState PlayerNumber
resetTurn p = do
  player <- findPlayer p
  (#players . ix (unPlayerNumber p) . #discard) %= ( ((player ^. #hand) ++ (player ^. #played) ) ++)
  (#players . ix (unPlayerNumber p) . #played) .= []
  (#players . ix (unPlayerNumber p) . #hand) .= []
  (#players . ix (unPlayerNumber p) . #unusedMoney) .= 0
  (#players . ix (unPlayerNumber p) . #victory) .= 0
  (#players . ix (unPlayerNumber p) . #turns) += 1
  return p

-- | Returns the list of players in total points order, highest first.
sortByPoints :: LegendaryState [LegendaryPlayer]
sortByPoints = do
  players' <- use $ #players
  return $ L.sort players'

configToGame :: LegendaryConfig -> StdGen -> LegendaryGame
configToGame c = LegendaryGame
                  (map (uncurry newPlayer) (c ^. #playerDefs))
                  (c ^. #scheme)
                  [theMastermind c]
                  30 -- starting number of Wounds
                  (genBystanders nPlayers) -- starting number of Bystanders
                  30 -- starting number of SHIELD Offciers
                  (genVillainDeck nPlayers (c ^. #villainDeck))
                  (genHeroDeck nPlayers (c ^. #heroDeck))
                  [] -- Escapees
                  (c ^. #entrance)
                  (replicate 5 Nothing) -- Start w/ empty HQ
                  [] -- KO pile
  where nPlayers = length $ playerDefs c
        genBystanders n = take (30 - bystandersInVillainDeck n) $ c ^. #bystanders
        genVillainDeck n vs =    concat vs
                              <> take (bystandersInVillainDeck n) (reverse $  c ^. #bystanders)
                              <> replicate 5 masterStrike
                              <> replicate (twists $ c ^. #scheme) schemeTwist
        genHeroDeck n hs = concat $ take (numHeros n) hs
        numHeros n = case n of
            5 -> 6
            _ -> 5
        bystandersInVillainDeck n = case n of
            2 -> 2
            3 -> 8
            4 -> 8
            5 -> 12
            _ -> error $ "Unsupported number of players: " <> show n


instance Game LegendaryConfig (DL.DList LegendaryMove) LegendaryGame where
  start = do
    r <- use #random
    gs <- get

    #heroDeck .= shuffle' (gs ^. #heroDeck) (length $ gs ^. #heroDeck) r
    #villainDeck .= shuffle' (gs ^. #villainDeck) (length $ gs ^. #villainDeck) (snd (split r))
    #bystanders .= shuffle' (gs ^. #bystanders) (length $ gs ^. #bystanders) (snd (split $ snd $ split r))

    #random .= snd $ split $ snd $ split $ snd $ split r
    order <- turnOrder
    sequence_ $ deal 6 <$> order


  finished    = do
    gs <- get
    mmEvilWins' <- sequence $ gs ^.. #masterminds . traverse . #mmEvilWins
    sEvilWins' <- gs ^. #scheme . #evilWins
    villainDeck' <- use #villainDeck
    return $ or mmEvilWins' && sEvilWins' && L.null villainDeck'

  runTurn p = do
    thePlayer <- findPlayer p
    drawVillain 1 p
    fillHq p
    _ <- evaluateHand p
      >>= (thePlayer ^. #strategy . #buyStrategy)
      >>= (thePlayer ^. #strategy . #attackStrategy)
      >>= resetTurn
      >>= deal (thePlayer ^. #nextTurnCards)

    (#players . ix (unPlayerNumber p) . #nextTurnCards) .= 6
    finished

  result      = do
      turnOrder' <- turnOrder
      mapM_ tallyPoints turnOrder'
      players' <- sortByPoints
      let grouped = L.groupBy (\p1 p2 -> (p1 ^. #victory) == (p2 ^. #victory) && (p1 ^. #turns) == (p2 ^. #turns)) players'
      return $ result' ((length . head) grouped) players'
    where result' 1 l = Left (playerName $ head l, victory $ head l)
          result' n _ = Right n

  turnOrder  = do
    players' <- use $ #players
    return $ PlayerNumber <$> [0 .. length players' - 1]

  tallyPoints p = do
    player <- findPlayer p
    vpts <- mapM (\c -> (c ^. #victoryPoints) c p) $ player ^. #victoryPile
    (#players . ix (unPlayerNumber p) . #victory) .= sum vpts
