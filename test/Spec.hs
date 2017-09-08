module Test
    ( testDominionGames
    , testDominionDeal
    , testDominionMultipleDeals
    , testDominionCard
    , testDominionDealResult
    , testDominionBuy
    , testDominionRound
    , testDominionTurn
    )
    where

import DeckBuilding.Dominion
import DeckBuilding.Dominion.Types


main :: IO ()
main = putStrLn "Test suite not yet implemented"

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
