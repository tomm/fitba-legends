module DB where
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Database.Persist.Sqlite

type Conn a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a
