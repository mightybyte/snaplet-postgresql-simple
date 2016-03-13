{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Snaplet.PostgresqlSimple.Internal where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control (MonadBaseControl(..), control)
import           Data.ByteString (ByteString)
import           Data.Pool
import qualified Database.PostgreSQL.Simple as P

------------------------------------------------------------------------------
-- | The state for the postgresql-simple snaplet. To use it in your app
-- include this in your application state and use pgsInit to initialize it.
data Postgres = PostgresPool (Pool P.Connection)
              | PostgresConn P.Connection


------------------------------------------------------------------------------
-- | Instantiate this typeclass on 'Handler b YourAppState' so this snaplet
-- can find the connection source.  If you need to have multiple instances of
-- the postgres snaplet in your application, then don't provide this instance
-- and leverage the default instance by using \"@with dbLens@\" in front of calls
-- to snaplet-postgresql-simple functions.
class (MonadIO m, MonadBaseControl IO m) => HasPostgres m where
    getPostgresState :: m Postgres
    setLocalPostgresState :: Postgres -> m a -> m a


------------------------------------------------------------------------------
-- | Data type holding all the snaplet's config information.
data PGSConfig = PGSConfig
    { pgsConnStr    :: ByteString
      -- ^ A libpq connection string.
    , pgsNumStripes :: Int
      -- ^ The number of distinct sub-pools to maintain. The smallest
      -- acceptable value is 1.
    , pgsIdleTime   :: Double
      -- ^ Amount of time for which an unused resource is kept open. The
      -- smallest acceptable value is 0.5 seconds.
    , pgsResources  :: Int
      -- ^ Maximum number of resources to keep open per stripe. The smallest
      -- acceptable value is 1.
    }


------------------------------------------------------------------------------
-- | Returns a config object with default values and the specified connection
-- string.
pgsDefaultConfig :: ByteString
                   -- ^ A connection string such as \"host=localhost
                   -- port=5432 dbname=mydb\"
                 -> PGSConfig
pgsDefaultConfig connstr = PGSConfig connstr 1 5 20



------------------------------------------------------------------------------
-- | Function that reserves a single connection for the duration of the given
-- action. Nested calls to withPG will only reserve one connection. For example,
-- the following code calls withPG twice in a nested way yet only results in a single
-- connection being reserved:
-- 
-- > myHandler = withPG $ do
-- >    queryTheDatabase
-- >    commonDatabaseMethod
-- >
-- > commonDatabaseMethod = withPG $ do
-- >    moreDatabaseActions
-- >    evenMoreDatabaseActions
--
-- This is useful in a practical setting because you may often find yourself in a situation
-- where you have common code (that requires a database connection) that you wish to call from 
-- other blocks of code that may require a database connection and you still want to make sure
-- that you are only using one connection through all of your nested methods.
withPG :: (HasPostgres m)
       => m b -> m b
withPG f = do
    s <- getPostgresState
    case s of
      (PostgresPool p) -> withResource p (\c -> setLocalPostgresState (PostgresConn c) f)
      (PostgresConn _) -> f


------------------------------------------------------------------------------
-- | Convenience function for executing a function that needs a database
-- connection.
liftPG :: (HasPostgres m) => (P.Connection -> m a) -> m a
liftPG act = do
   pg <- getPostgresState
   control $ \run ->
     withConnection pg $ \con -> run (act con)


-- | Convenience function for executing a function that needs a database
-- connection specialized to IO.
liftPG' :: (HasPostgres m) => (P.Connection -> IO b) -> m b
liftPG' f = do
    s <- getPostgresState
    withConnection s f


------------------------------------------------------------------------------
-- | Convenience function for executing a function that needs a database
-- connection.
withConnection :: MonadIO m => Postgres -> (P.Connection -> IO b) -> m b
withConnection (PostgresPool p) f = liftIO (withResource p f)
withConnection (PostgresConn c) f = liftIO (f c)
