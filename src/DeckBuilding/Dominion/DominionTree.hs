{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module DeckBuilding.Dominion.DominionTree where

import DeckBuilding.Types ( Result )
import DeckBuilding.Dominion.Types
    ( DominionTree(DominionTree),
      GameTurn(GameTurn),
      PlayerTurn(PlayerTurn),
      BoughtCard(..),
      CardPlay(..),
      Card(Card),
      DominionMove(GameOver, Turn, ThroneRoom, Remodel, Harbinger,
                   Buy, Deal) )
import qualified Data.List as List
import Data.Maybe ( catMaybes )

--import Debug.Trace

-- | Given a list of 'DominionMove's and a 'Result', create a 'DominionTree' per game.
buildDominionTrees :: [DominionMove] -> [Result] -> [DominionTree]
buildDominionTrees moves res = buildDominionTree <$> zip theGames res
  where theGames = findGames moves

-- | Given a list of 'DominionMove's for a single game, and the 'Result' of that game,
-- create the corresponding 'DominionTree' representation.
buildDominionTree :: ([DominionMove], Result) -> DominionTree
-- buildDominionTree moves result | trace ("buildDominionTree " <> show moves <> " " <> show result) False=undefined
buildDominionTree (moves, res) = DominionTree (buildGameTurn <$> gameTurns) res
  where allTurns :: [[DominionMove]] = findTurns moves
        gameTurns = List.groupBy sameTurn allTurns

-- | TODO: is this just checking the first move of a list of moves?  I don't remember.
sameTurn :: [DominionMove] -> [DominionMove] -> Bool
--sameTurn x y | trace ("sameTurn: " <> show x <> " vs " <> show y) False=undefined
sameTurn (Turn _ n _:_) (Turn _ n' _:_) = n == n'
sameTurn (x:_) (y:_) = error $ "sameTurn: Either " <> show x <> " or " <> show y <> " is not a Turn"
sameTurn x y = error $ "sameTurn: Something is very wrong with " <> show x <> " or " <> show y <> " or both."

-- | Split a list of 'DominionMove's into a list of a list of 'DominionMove's,
-- where each is the set of moves for a game.
findGames :: [DominionMove] -> [[DominionMove]]
-- findGames xs | trace ("findGames " <> (show (length xs))) False=undefined
findGames [] = []
findGames xs = game : findGames moves
  where (game, _:moves) = break isGameOver xs

-- | Is this move a 'GameOver' marker?
isGameOver :: DominionMove -> Bool
-- isGameOver move | trace ("isGameOver " <> show move) False=undefined
isGameOver (GameOver _) = True
isGameOver _            = False

-- | Is the move a 'Turn' marker?
isTurn :: DominionMove -> Bool
--isTurn move | trace ("isTurn " <> show move) False=undefined
isTurn Turn {} = True
isTurn _       = False

-- | Is this move a 'Buy'?
isBuy :: DominionMove -> Bool
isBuy (Buy _ _) = True
isBuy _       = False

-- | Break up the list of 'DominionMove's into sublists per 'Turn'.
findTurns :: [DominionMove] -> [[DominionMove]]
findTurns [] = []
findTurns (x:xs) = ( x : turn) : findTurns moves
  where (turn, moves) = break isTurn xs

-- | Given a set of player turns, create a 'GameTurn' representation.
buildGameTurn :: [[DominionMove]] -> GameTurn
--buildGameTurn moves | trace ("buildGameTurn " <> show moves) False=undefined
buildGameTurn manyTurns@((Turn _ n _:_):_) = GameTurn n $ buildPlayerTurn <$> manyTurns
buildGameTurn (x:_) = error $ "buildGameTurn: Found non-Turn move where Turn expected: " ++ show x
buildGameTurn _ = error $ "buildGameTurn: whaaaa"

-- | Given a list of 'DominionMove's corresponding to a single player's turn,
-- build a 'PlayerTurn' representation.
buildPlayerTurn :: [DominionMove] -> PlayerTurn
--buildPlayerTurn moves | trace ("buildPlayerTurn " <> show moves) False=undefined
buildPlayerTurn (Turn p _ _:moves) = let (plays, theBuys) = break isBuy moves in PlayerTurn p (buildPlays plays) (catMaybes (buildBuy <$> theBuys))
buildPlayerTurn (x:_) = error $ "buildPlayerTurn: Found non-Turn move where Turn expected: " ++ show x
buildPlayerTurn _ = error $ "buildPlayerTurn: I can't even."

-- | Convert 'DominionMove's into 'CardPlay's.
buildPlays :: [DominionMove] -> [DominionMove]
--buildPlays moves | trace ("buildPlays " <> show moves) False=undefined
buildPlays [] = []
-- TODO: Rewrite this given new structure for DominionMoves
buildPlays (Deal {}:moves) = buildPlays moves -- TODO: skipping these for now
buildPlays dm = dm

-- | Convert 'DominionMove's into 'BoughtCard's.
buildBuy :: DominionMove -> Maybe BoughtCard
--buildBuy move | trace ("buildBuy " <> show move) False=undefined
buildBuy (Buy _ c)    = Just $ BoughtCard c
buildBuy Deal {}      = Nothing -- TODO: skipping these for now
buildBuy (GameOver _) = Nothing
buildBuy move = error $ "Non-buy move found: " <> show move

