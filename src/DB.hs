{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstraintKinds            #-}
module DB where
import Control.Monad.Logger (NoLoggingT, runStderrLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT, MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)
import Database.Persist.Sqlite
import Control.Monad.Catch (MonadCatch)

import qualified Schema

type MonadDB a = (MonadIO a, MonadCatch a)  -- needs ConstraintKinds
type Con a b = ReaderT SqlBackend a b
type ConnectionPool = Database.Persist.Sqlite.ConnectionPool

getPool dbFile maxConns f = runStderrLoggingT $ withSqlitePool dbFile maxConns f

getFixtures :: MonadDB a => Con a [Entity Schema.Game]
getFixtures = do
    f <- selectList [] []
    return (f :: [Entity Schema.Game])
