{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE LambdaCase                #-}

module DeckBuilding.Legendary.Cards.Base where

import DeckBuilding.Types ( PlayerNumber(PlayerNumber, unPlayerNumber) )
import DeckBuilding.Legendary.Types
    ( DrawDiscardChoice(DiscardChoice, DrawChoice),
      VillainCard(VillainCard),
      HeroCard(HeroCard),
      HeroClass(Tech, Ranged),
      HeroTeam(SHIELD),
      CityLocation(location),
      Location(Bridge, Rooftops),
      Scheme(Scheme),
      Mastermind(Mastermind),
      LegendaryState )
import DeckBuilding.Legendary.Cards.Utils
    ( valueCard,
      koNOfTopofDeck,
      gainRecruit,
      koNFromHand,
      adjustNextTurnCards,
      vpPerClass,
      classOrWound,
      emptyWoundPile,
      recruitN )
import DeckBuilding.Legendary.Utils
    ( deal, findPlayer, capturedAction )
import Data.Generics.Product ( HasField(field) )
import Control.Lens ( (^..), folded, (^.), use )
import Control.Monad ( forM_, void )

-- S.H.I.E.L.D.

shieldAgent :: HeroCard
shieldAgent = HeroCard "S.H.I.E.L.D. Agent" 0 (valueCard 1 0) [] SHIELD

shieldTrooper :: HeroCard
shieldTrooper = HeroCard "S.H.I.E.L.D. Trooper" 0 (valueCard 0 1) [] SHIELD

shieldOfficer :: HeroCard
shieldOfficer = HeroCard "S.H.I.E.L.D. Officer" 3 (valueCard 2 0) [] SHIELD

bystander :: VillainCard
bystander = VillainCard "Bystander" 0 False True (\_ _ -> return 1) (Just capturedAction) Nothing Nothing

masterStrike :: VillainCard
masterStrike = VillainCard "Master Strike" 0 False False (\_ _ -> return 0) (Just doMasterStrike) Nothing Nothing

doMasterStrike :: VillainCard -> PlayerNumber -> LegendaryState ()
doMasterStrike _ pnum = do
  mms <- use $ #masterminds
  forM_ mms $ \mm -> (mm ^. #masterStrikeAction) pnum

schemeTwist :: VillainCard
schemeTwist = VillainCard "Scheme Twist" 0 False False (\_ _ -> return 0) (Just doSchemeTwist) Nothing Nothing

doSchemeTwist :: VillainCard -> PlayerNumber -> LegendaryState ()
doSchemeTwist _ pnum = do
  scheme <- use $ #scheme
  (scheme ^. #schemeTwistAction) pnum

-- Henchmen

doombot :: VillainCard
doombot = VillainCard "Doombot Legion" 3 False False (\_ _ -> return 1) Nothing (Just (koNOfTopofDeck 1 2)) Nothing

doombots :: [VillainCard]
doombots = take 15 $ repeat doombot

handNinja :: VillainCard
handNinja = VillainCard "Hand Ninjas" 3 False False (\_ _ -> return 1) Nothing (Just (gainRecruit 1)) Nothing

handNinjas :: [VillainCard]
handNinjas = take 15 $ repeat handNinja

savageLandMutate :: VillainCard
savageLandMutate = VillainCard "Savage Land Mutates" 3 False False (\_ _ -> return 1) Nothing (Just (adjustNextTurnCards 1)) Nothing

savageLandMutates :: [VillainCard]
savageLandMutates = take 15 $ repeat savageLandMutate

sentinel :: VillainCard
sentinel = VillainCard "Sentinel" 3 False False (\_ _ -> return 1) Nothing (Just (koNFromHand 1)) Nothing

sentinels :: [VillainCard]
sentinels = take 15 $ repeat sentinel
 
-- Masters of Evil

whirlwind :: VillainCard
whirlwind = VillainCard "Whirlwind" 4 False False (\_ _ -> return 2) Nothing (Just attack) Nothing
  where attack :: VillainCard -> CityLocation -> PlayerNumber -> LegendaryState PlayerNumber
        attack c loc pnum = if location loc == Rooftops || location loc == Bridge
          then koNFromHand 2 c loc pnum
          else pure pnum

ultron :: VillainCard
ultron = VillainCard "Ultron" 6 False False (vpPerClass Tech) Nothing Nothing (Just $ classOrWound Tech)

mastersOfEvil :: [VillainCard]
mastersOfEvil =    (take 2 $ repeat whirlwind)
                <> (take 2 $ repeat ultron)
--                <> (take 2 $ repeat melter)
--                <> (take 2 $ repeat baronZemo)
                <> (take 2 $ repeat whirlwind)
                <> (take 2 $ repeat ultron)



-- Masterminds

-- Dr. Doom

drdoom :: Mastermind
drdoom =
  Mastermind
    "Dr. Doom"
    9
    5
    (pure False)
    drdoomMasterStrike
    [ darkTechnology
    , monarch'sDecree
    --, secretsOfTimeTravel
    --, treasuresOfLatveria
    ]
    [doombots]

darkTechnology :: VillainCard
darkTechnology = VillainCard "Dark Technology" 9 False False (\_ _ -> pure 5) Nothing (Just $ recruitN (\h -> Tech `elem` (h ^. #heroClass) || Ranged `elem` (h ^. #heroClass)) 1 0) Nothing

monarch'sDecree :: VillainCard
monarch'sDecree = VillainCard "Monarch's Decree" 9 False False (\_ _ -> pure 5) Nothing (Just drawOrDiscard) Nothing
  where
    drawOrDiscard :: VillainCard -> CityLocation -> PlayerNumber -> LegendaryState PlayerNumber
    drawOrDiscard _ _ pnum = do
      ps <- use $ field @"players"
      p <- findPlayer pnum
      ((p ^. #strategy . #othersDrawOrDiscardStrategy) 1 pnum) >>= \case
        DrawChoice -> forM_ [0 .. length ps - 1] $ \opnum -> if opnum == (unPlayerNumber pnum)
            then return ()
            else void $ deal 1 $ PlayerNumber opnum
        DiscardChoice -> forM_ [0 .. length ps - 1] $ \opnum -> if opnum == (unPlayerNumber pnum)
            then return ()
            else do
              op <- findPlayer $ PlayerNumber opnum
              _ <- (op ^. #strategy . #discardStrategy) (1, 1) $ PlayerNumber opnum
              return ()
      return pnum
          

--secretsOfTimeTravel :: VillainCard

--treasuresOfLatveria :: VillainCard


drdoomMasterStrike :: PlayerNumber -> LegendaryState ()
drdoomMasterStrike _ = do
  ps <- use $ field @"players"
  forM_ [0 .. length ps - 1] $ \pnum -> do
    p <- findPlayer $ PlayerNumber pnum
    if (6 == (length $ p ^. #hand)) &&
       (0 < (length $ filter (== Tech) $ (p ^. #hand) ^.. folded . #heroClass . folded))
      then void $ (p ^. #strategy . #discardStrategy) (2, 2) $ PlayerNumber pnum
      else pure ()
  pure ()

-- Schemes

legacyVirus :: Scheme
legacyVirus =
  Scheme
    "The Legacy Virus"
    emptyWoundPile
    (classOrWound Tech bystander)
    8
