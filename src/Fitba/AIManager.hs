{-# LANGUAGE OverloadedStrings     #-}
module Fitba.AIManager (pickFormation) where

import Database.Persist.Postgresql
import qualified Data.List
import qualified Safe
import Control.Monad

import Fitba.Player (playerSkill, playerPositionsList)
import Fitba.Types as Types
import Fitba.Schema

formation_442 :: [Types.FormationPitchPos]
formation_442 = [
    (2,6),
    (0,5), (1,5), (3,5), (4,5),
    (0,3), (1,3), (3,3), (4,3),
    (1,1), (3,1)
  ]
formation_352 :: [Types.FormationPitchPos]
formation_352 = [
  (2,6),
  (1,5),(2,5),(3,5),
  (2,4),
  (1,3),(3,3),
  (0,2),(4,2),
  (1,1),(3,1)
  ]
formation_532 :: [Types.FormationPitchPos]
formation_532 = [
  (2,6),
  (1,5),(2,5),(3,5),
  (0,4),(4,4),
  (1,3),(2,3),(3,3),
  (1,1),(3,1)
  ]
formation_433 :: [Types.FormationPitchPos]
formation_433 = [
  (2,6),
  (0,5),(1,5),(3,5),(4,5),
  (2,3),
  (1,2),(3,2),
  (0,1),(2,1),(4,1)
  ]
formation_451 :: [Types.FormationPitchPos]
formation_451 = [
  (2,6),
  (0,5),(1,5),(3,5),(4,5),
  (2,4),
  (0,3),(1,3),(3,3),(4,3),
  (2,1)
  ]
formation_4231 :: [Types.FormationPitchPos]
formation_4231 = [
    (2,6),
    (0,5),(1,5),(3,5),(4,5),
    (1,3),(3,3),
    (0,2),(2,2),(4,2),
    (2,1)
  ]
formation_4231d :: [Types.FormationPitchPos]
formation_4231d = [
    (2,6),
    (0,5),(1,5),(3,5),(4,5),
    (1,4),(3,4),
    (0,2),(2,2),(4,2),
    (2,1)
  ]
formation_4141 :: [Types.FormationPitchPos]
formation_4141 = [
  (2,6),
  (0,5),(1,5),(3,5),(4,5),
  (2,3),
  (0,2),(1,2),(3,2),(4,2),
  (2,1)
  ]
formation_4141d :: [Types.FormationPitchPos]
formation_4141d = [
  (2,6),
  (0,5),(1,5),(3,5),(4,5),
  (2,4),
  (0,3),(1,3),(3,3),(4,3),
  (2,1)
  ]
  
type Tactic = [Types.FormationPitchPos]
type TacticCost = Int

formations :: [Tactic]
formations = [ formation_442, formation_352, formation_433, formation_4231, formation_4231d,
               formation_4141, formation_4141d, formation_451, formation_532 ]

pickFormation :: [Entity Player] -> [(Entity Player, Maybe Types.FormationPitchPos)]
pickFormation players =
  let formationSquads = map (pickTeam playersBySkill) formations

      playersBySkill = Data.List.sortBy skillSort players

      skillSort a b = let as = playerSkill $ entityVal a
                          bs = playerSkill $ entityVal b
                      in  if as > bs then LT else if as < bs then GT else EQ

  in
    -- find lowest cost tactic
    snd $ Data.List.minimumBy (\a b -> fst a `compare` fst b) formationSquads

pickTeam :: [Entity Player] -> Tactic -> (TacticCost, [(Entity Player, Maybe Types.FormationPitchPos)])
pickTeam rankedPlayers tactic =
  let pick = pickTeam' rankedPlayers tactic []
  in  foldl (\a b -> (fst a + fst b, snd b : snd a)) (0, []) pick

pickTeam' :: [Entity Player] -> [Types.FormationPitchPos] -> [(TacticCost, (Entity Player, Maybe Types.FormationPitchPos))]
          -> [(TacticCost, (Entity Player, Maybe Types.FormationPitchPos))]
pickTeam' _ [] picks = picks
pickTeam' rankedPlayers (pos:poss) picks =
  let pick = pickPlayer pos rankedPlayers 
      pickedPlayer = fst $ snd pick
      remainingPlayers = Data.List.filter ((/= entityKey pickedPlayer) . entityKey) rankedPlayers
  in pickTeam' remainingPlayers poss (pick:picks)

adjacentPositions :: Types.FormationPitchPos -> [Types.FormationPitchPos]
adjacentPositions (px,py) = do { x<-[-1..1]; y<-[-1..1]; guard (x/=0 || y/=0); return (px+x,py+y) }

pickPlayer :: Types.FormationPitchPos -> [Entity Player] -> (TacticCost, (Entity Player, Maybe Types.FormationPitchPos))
pickPlayer pos rankedPlayers =
  let canPlayThere = Safe.headMay $ Data.List.filter (elem pos . playerPositionsList . entityVal) rankedPlayers
      canAlmostPlayThere = Safe.headMay $ Data.List.filter (\p -> 
        not (null (
          (playerPositionsList . entityVal) p `Data.List.intersect` adjacentPositions pos
        ))) rankedPlayers
      anyPlayer = head rankedPlayers
      -- XXX ^^ actually takes most skillful available player, which might be a bad choice since they will be out
      -- of positions
  in case canPlayThere of
    Nothing -> case canAlmostPlayThere of
      Nothing -> (2, (anyPlayer, Just pos))
      Just p  -> (1, (p, Just pos))
    Just p  -> (0, (p, Just pos))
