{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module DeckBuilding.Dominion.DominionTree where

import           DeckBuilding.Types
import           DeckBuilding.Dominion.Types
import qualified Data.List                              as List
import           Data.Maybe

--import Debug.Trace

buildDominionTrees :: [DominionMove] -> [Result] -> [DominionTree]
buildDominionTrees moves results = (buildDominionTree <$> zip games results)
  where games = findGames moves

buildDominionTree :: ([DominionMove], Result) -> DominionTree
-- buildDominionTree moves result | trace ("buildDominionTree " <> show moves <> " " <> show result) False=undefined
buildDominionTree (moves, result) = DominionTree (buildGameTurn <$> gameTurns) result
  where allTurns :: [[DominionMove]] = findTurns moves
        gameTurns = List.groupBy sameTurn allTurns

sameTurn :: [DominionMove] -> [DominionMove] -> Bool
--sameTurn x y | trace ("sameTurn: " <> show x <> " vs " <> show y) False=undefined
sameTurn ((Turn n _):_) ((Turn n' _):_) = n == n'
sameTurn (x:_) (y:_) = error $ "sameTurn: Either " <> show x <> " or " <> show y <> " is not a Turn"
sameTurn x y = error $ "sameTurn: Something is very wrong with " <> show x <> " or " <> show y <> " or both."

findGames :: [DominionMove] -> [[DominionMove]]
-- findGames xs | trace ("findGames " <> (show (length xs))) False=undefined
findGames [] = []
findGames xs = trimScoring : findGames moves
  where (game, (_:moves)) = break isGameOver xs
        -- The end of the game has a lot of stuff from scoring.
        -- Get rid of it. Everything from the last Buy to the
        -- GameOver.
        trimScoring = List.dropWhileEnd (not . isBuy) game ++ [List.last game]

isGameOver :: DominionMove -> Bool
-- isGameOver move | trace ("isGameOver " <> show move) False=undefined
isGameOver (GameOver _) = True
isGameOver _            = False

isTurn :: DominionMove -> Bool
--isTurn move | trace ("isTurn " <> show move) False=undefined
isTurn (Turn _ _ ) = True
isTurn _           = False

isBuy :: DominionMove -> Bool
isBuy (Buy _) = True
isBuy _       = False

findTurns :: [DominionMove] -> [[DominionMove]]
findTurns [] = []
findTurns (x:xs) = ( x : turn) : findTurns moves
  where (turn, moves) = break isTurn xs

buildGameTurn :: [[DominionMove]] -> GameTurn
--buildGameTurn moves | trace ("buildGameTurn " <> show moves) False=undefined
buildGameTurn turns@(((Turn n _):_):_) = GameTurn n $ buildPlayerTurn <$> turns
buildGameTurn (x:_) = error $ "buildGameTurn: Found non-Turn move where Turn expected: " ++ show x

buildPlayerTurn :: [DominionMove] -> PlayerTurn
--buildPlayerTurn moves | trace ("buildPlayerTurn " <> show moves) False=undefined
buildPlayerTurn ((Turn _ p):moves) = let (plays, buys) = break isBuy moves in PlayerTurn (playerName p) (buildPlays plays) (catMaybes (buildBuy <$> buys))
buildPlayerTurn (x:_) = error $ "buildPlayerTurn: Found non-Turn move where Turn expected: " ++ show x

buildPlays :: [DominionMove] -> [CardPlay]
--buildPlays moves | trace ("buildPlays " <> show moves) False=undefined
buildPlays [] = []
buildPlays ((Play (Card "Throne Room" _ _ _)):(ThroneRoom c):moves) = PlayThroneRoom c : buildPlays moves
buildPlays ((Play (Card "Remodel" _ _ _)):(Remodel c c'):moves) = PlayRemodel c c' : buildPlays moves
buildPlays ((Play (Card "Cellar" _ _ _)):(Discard c):moves) = PlayCellar c : buildPlays moves
buildPlays ((Play c):moves) = Standard c : buildPlays moves
buildPlays ((Deal _ _):moves) = buildPlays moves -- TODO: skipping these for now
buildPlays (x:xs) = error $ "Non-play move found: " <> show x <> "\nOthers: " <> show xs

buildBuy :: DominionMove -> Maybe BoughtCard
--buildBuy move | trace ("buildBuy " <> show move) False=undefined
buildBuy (Buy c)    = Just $ BoughtCard c
buildBuy (Deal _ _) = Nothing -- TODO: skipping these for now
buildBuy move = error $ "Non-buy move found: " <> (show move)

