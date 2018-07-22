module Main where

import Control.Monad
import qualified Control.Monad.Random as Random
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (toSqlKey, get, selectList, entityKey, entityVal)
import Data.Maybe
import qualified Data.Map.Strict as M
import System.Random (newStdGen, split)

import DbTest
import qualified Fitba.Core as Core
import qualified Fitba.DB as DB
import qualified Fitba.Match as Match
import Fitba.Schema

main :: IO ()
main = do
    rng <- newStdGen
    dbTest $ do
        Core.populateSchema
        let team1Id = toSqlKey 1
            team2Id = toSqlKey 2
        team1 <- fromJust <$> get team1Id
        team2 <- fromJust <$> get team2Id
        ppos1 <- Core.getPlayersOrdered team1Id (teamFormationId team1)
        ppos2 <- Core.getPlayersOrdered team2Id (teamFormationId team2)

        let gameState = Match.setupGame Match.Home ppos1 ppos2
            playerMap = M.fromList $ map (\(entity, _) -> (entityKey entity, entityVal entity)) (ppos1 ++ ppos2)

        liftIO $ print gameState

        doTurns gameState playerMap rng 100

    where
        doTurns _ _ _ 0 = return ()
        doTurns gameState playerMap rng n = do
            let (rng', rng'') = System.Random.split rng
            let (gameState', log) = Match.runRandLogger (Match.takeGameTurn playerMap gameState) rng'
            liftIO $ putStrLn log
            liftIO $ print gameState'
            doTurns gameState' playerMap rng'' (n-1)


    --(_, log) <- Match.runRandLogger Match.game <$> newStdGen 
    --putStrLn log
    {-
    dbTest $ do
        liftIO $ return ()
        -}
