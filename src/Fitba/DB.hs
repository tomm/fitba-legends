{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstraintKinds            #-}
module Fitba.DB where
import Control.Monad.Logger (NoLoggingT, runStderrLoggingT, filterLogger, LogLevel(..), MonadLogger)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT, MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)
import Database.Persist.Postgresql
import Control.Monad.Catch (MonadCatch)

import qualified Fitba.Schema as Schema

type MonadDB a = (MonadIO a, MonadCatch a, MonadLogger a)  -- needs ConstraintKinds
type Con a b = ReaderT SqlBackend a b
type ConnectionPool = Database.Persist.Postgresql.ConnectionPool

getPool connString maxConns f =
  runStderrLoggingT
    $ filterLogger (\_ level -> level /= LevelDebug)
    $ withPostgresqlPool connString maxConns f

getFixtures :: MonadDB a => Con a [Entity Schema.Game]
getFixtures = do
    f <- selectList [] []
    return (f :: [Entity Schema.Game])
