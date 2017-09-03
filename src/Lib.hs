module Lib
    ( testDominionDeal
    , testDominionDealResult
    , testDominionCard
    , testDominionBuy
    , testDominionRound
    , testDominionMultipleDeals
    , testDominionGame
    , testDominionTurn
    , testPlayerFind
    , testDominionDeals
    , testDominionGames
    , Result
    ) where

import System.Random
import System.Random.Shuffle
import Data.List (delete, find, sortBy, group, sort, groupBy)
import qualified Data.Map as Map

-- Dominion

data GameState = GameState {
  _players  :: [Player],
  _decks    :: Map.Map Card Int,
  _random   :: StdGen
} deriving Show

--instance Show GameState where
--  show gs = show (_players gs) ++ " " ++ show (_decks gs)

data Card = Card {
  _cardName :: String,
  _cost     :: Int,
  _action   :: Card -> Player -> GameState -> GameState
}

instance Ord Card where
  compare c1 c2 = compare (_cardName c1) (_cardName c2)

instance Eq Card where
  a == b = _cardName a == _cardName b

instance Show Card where
  show c = _cardName c

data Player = Player {
  _playerName :: String,
  _deck       :: [Card],
  _discard    :: [Card],
  _hand       :: [Card],
  _actions    :: Int,
  _buys       :: Int,
  _money      :: Int,
  _victory    :: Int
} deriving Show

instance Eq Player where
  a == b = _playerName a == _playerName b

instance Ord Player where
  compare p1 p2 = compare (_playerName p1) (_playerName p2)

type Result = Either String Int

-- Cards and their actions

basicCardAction :: Int -> Int -> Int -> Int -> Int -> Card -> Player -> GameState -> GameState
basicCardAction draw actions buys money victory c p gs = changeTurn player gs'
  where player    = Player (_playerName p'') (_deck p'') (c : _discard p'') (delete c (_hand p'')) (_actions p'' + actions) (_buys p'' + buys) (_money p'' + money) (_victory p'' + victory)
        Just p'   = find (== p) (_players gs)
        gs'       = deal draw p' gs
        Just p''  = find (== p) (_players gs')

goldCard      = Card "Gold" 6 (basicCardAction 0 0 0 3 0)

silverCard    = Card "Silver" 3 (basicCardAction 0 0 0 2 0)

copperCard    = Card "Copper" 0 (basicCardAction 0 0 0 1 0)

provinceCard  = Card "Province" 8 (basicCardAction 0 0 0 0 6)

duchyCard     = Card "Province" 5 (basicCardAction 0 0 0 0 3)

estateCard    = Card "Estate" 2 (basicCardAction 0 0 0 0 1)

marketCard    = Card "Market" 5 (basicCardAction 1 0 1 1 0)

moatCard      = Card "Moat" 2 (basicCardAction 2 (-1) 0 0 0)

smithyCard    = Card "Smithy" 4 (basicCardAction 3 (-1) 0 0 0)

-- Core Engine

newPlayer :: String -> Player
newPlayer n = Player n [] ((( (take 7) . repeat ) copperCard) ++ (( (take 3) . repeat) estateCard)) [] 1 1 0 0

deal :: Int -> Player -> GameState -> GameState
deal 0   _ gs = gs
deal num p gs = changeTurn player (GameState (_players gs) (_decks gs) (choose (split (_random gs))))
  where (enoughDeck, discard)
          | length (_deck p') >= num = (_deck p', _discard p')
          | otherwise                = ( (_deck p') ++ (shuffle' (_discard p') (length (_discard p')) (_random gs)), [])
        (hand, deck)  = splitAt num enoughDeck
        player        = Player (_playerName p') deck discard hand (_actions p') (_buys p') (_money p') (_victory p')
        Just p'       = find (== p) (_players gs)
        choose (_, g) = g

changeTurn :: Player -> GameState -> GameState
changeTurn p gs = GameState (p : (delete p (_players gs)) ) (_decks gs) (_random gs)

evaluateHand :: Player -> GameState -> GameState
evaluateHand p gs = changeTurn p' newGs
  where newGs     = foldr (\c -> (_action c) c p) gs (_hand p'')
        Just p'   = find (== p) (_players newGs)
        Just p''  = find (== p) (_players gs)

tallyAllPoints :: Player -> GameState -> GameState
tallyAllPoints p gs = evaluateHand player $ changeTurn player gs
  where player  = Player (_playerName p') [] [] ((_deck p') ++ (_discard p') ++ (_hand p')) 1 1 0 0
        Just p' = find (== p) (_players gs)

sortByPoints :: GameState -> GameState
sortByPoints gs = GameState (reverse (sortBy (\p1 p2 -> compare (_victory p1) (_victory p2)) (_players gs))) (_decks gs) (_random gs)

gameResult' :: [Player] -> Result
gameResult' players = result ((length . head) grouped) players
  where grouped = groupBy (\p1 p2 -> (_victory p1) == (_victory p2)) players
        result 1 l = Left $ _playerName $ head l
        result n _ = Right n

gameResult :: GameState -> Result
gameResult gs = gameResult' (_players sorted)
  where sorted = sortByPoints (foldr tallyAllPoints gs (_players gs))

decreaseCards :: Card -> Card -> Int -> Int
decreaseCards  _  _ 0 = 0
decreaseCards c1 c2 n = if (c1 == c2)
                          then n - 1
                          else n

buyCard :: Player -> Maybe Card -> GameState -> GameState
buyCard p Nothing  gs = gs
buyCard p (Just c) gs = changeTurn (player c) (decks c)
  where
    decks c   = GameState (_players gs) (Map.mapWithKey (decreaseCards c) (_decks gs) ) (_random gs)
    player c  = Player (_playerName p') (_deck p') (c : (_discard p') ) (_hand p') (_actions p') (_buys p' - 1) (_money p' - (_cost c)) (_victory p')
    Just p'   = find (== p) (_players gs)

doBuy :: Int -> Int -> [Card] -> [Maybe Card]
doBuy 0 _ _ = []
doBuy n 0 _ = []
doBuy n m cs = findHighCostCard : doBuy (n - 1) (m - (cost findHighCostCard)) cs
  where findHighCostCard = find (\c -> (_cost c) <= m) cs
        cost (Just c)     = (_cost c)
        cost Nothing      = 0

doBuys :: Player -> [Card] -> GameState -> GameState
doBuys p cards gs = foldr (\mc acc -> buyCard p mc acc) gs (doBuy (_buys p) (_money p) (removeEmptyDecks cards gs))
  where removeEmptyDecks cards gs = filter (\c -> (Map.member c (_decks gs)) && (_decks gs) Map.! c > 0) cards

bigMoneyCards :: [Card]
bigMoneyCards = [provinceCard, goldCard, silverCard]

bigMoneyBuy :: Player -> GameState -> GameState
bigMoneyBuy p gs = doBuys p' bigMoneyCards gs
  where Just p' = find (== p) (_players gs)

basicDecks :: Int -> Map.Map Card Int
basicDecks numPlayers
    | numPlayers == 2 = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 8), (duchyCard, 8), (provinceCard, 8) ]
    | otherwise       = Map.fromList [ (copperCard, 60 - (7 * numPlayers)), (silverCard, 40), (goldCard, 30), (estateCard, 12), (duchyCard, 12), (provinceCard, 12) ]

resetTurn :: Player -> GameState -> GameState
resetTurn p gs = changeTurn player gs
  where Just p' = find (== p) (_players gs)
        player  = Player (_playerName p') (_deck p') (_discard p') (_hand p') 1 1 0 0

doTurn :: Bool -> Player -> GameState -> GameState
doTurn True  p gs = gs
doTurn False p gs = ( (resetTurn p) . (deal 5 p) . (bigMoneyBuy p) . (evaluateHand p) ) gs

isGameOver :: GameState -> Bool
isGameOver gs = ((_decks gs) Map.! provinceCard == 0) || numEmptyDecks >= 3
  where numEmptyDecks = length $ Map.filter (== 0) (_decks gs)

runGame' :: Bool -> GameState -> GameState
runGame' True  gs = sortByPoints (foldr tallyAllPoints gs (_players gs))
runGame' False gs = runGame' (isGameOver gs') gs'
  where gs' = foldr (doTurn (isGameOver gs)) gs (_players gs)

runGame :: [Player] -> IO (Result)
runGame players = do
  g <- newStdGen
  return $ gameResult $ runGame' False $ GameState players (basicDecks (length players)) g

runGames :: Int -> [Player] -> IO ([(Result, Int)])
runGames num players = do
  g <- newStdGen
  let seeds = take num $ randoms g
  let gens = map mkStdGen seeds
  let gses = map (GameState players (basicDecks (length players))) gens
  return $ map (\l -> (head l, length l)) $ group $ sort $ map (gameResult . (runGame' False)) gses

-- Dominion Game testing functions

testDominionDeal :: IO (GameState)
testDominionDeal = do
  let a = newPlayer "Player 1"
  g <- newStdGen
  return $ deal 5 a (GameState [a] (basicDecks 2) g)

testDominionMultipleDeals :: IO (GameState)
testDominionMultipleDeals = do
  let a = newPlayer "Player 1"
  g <- newStdGen
  return $ ((deal 5 a) . (resetTurn a) . (evaluateHand a) . (deal 5 a)) (GameState [a] (basicDecks 2) g)

testDominionDealResult :: IO (GameState)
testDominionDealResult = do
  let a = newPlayer "Player 1"
  g <- newStdGen
  return $ ( (evaluateHand a) . (deal 5 a) ) (GameState [a] (basicDecks 2) g)

testDominionCard :: IO (GameState)
testDominionCard = do
  let a = newPlayer "Player 1"
  g <- newStdGen
  let afterDeal = deal 5 a (GameState [a] (basicDecks 2) g)
  let Just newA = find (== a) (_players afterDeal)
  let firstCard = head (_hand newA)
  return $ (_action firstCard) firstCard newA afterDeal

testDominionBuy :: IO (GameState)
testDominionBuy = do
  let a = newPlayer "Player 1"
  g <- newStdGen
  return $ ( (bigMoneyBuy a) . (evaluateHand a) . (deal 5 a) ) (GameState [a] (basicDecks 2) g)

testDominionRound :: IO (GameState)
testDominionRound = do
  g <- newStdGen
  let players = [ newPlayer "Player 1", newPlayer "Player 2"]
  let gs = (GameState players (basicDecks (length players)) g)
  let gs' = foldr (deal 5) gs players
  return $ foldr (doTurn False) gs' players

testDominionTurn :: IO (GameState)
testDominionTurn = do
  g <- newStdGen
  let players = [ newPlayer "Player 1", newPlayer "Player 2"]
  let gs = (GameState players (basicDecks (length players)) g)
  let gs' = foldr (deal 5) gs players
  return $ doTurn False (head players) gs'

testDominionDeals :: IO (GameState)
testDominionDeals = do
  g <- newStdGen
  let players = [ newPlayer "Player 1", newPlayer "Player 2"]
  let gs = (GameState players (basicDecks (length players)) g)
  return $ foldr (deal 5) gs players

testDominionGame :: IO (Result)
testDominionGame = runGame [newPlayer "Player 1", newPlayer "Player 2"]

testDominionGames :: IO ([(Result, Int)])
testDominionGames = runGames 1000 [newPlayer "Player 1", newPlayer "Player 2"]

testPlayerFind :: IO (Player)
testPlayerFind = do
  g <- newStdGen
  let a = newPlayer "Andrew"
  let r = newPlayer "Ruth"
  let gs = GameState [a, r] (basicDecks 2) g
  let Just x = find (== r) (_players (changeTurn a gs))
  return $ x
