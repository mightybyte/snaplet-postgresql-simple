{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

{-|

This snaplet makes it simple to use a PostgreSQL database from your Snap
application and is based on the excellent postgresql-simple library
(<http://hackage.haskell.org/package/postgresql-simple>) by Leon Smith
(adapted from Bryan O'Sullivan's mysql-simple).  Now, adding a database
to your web app takes just two simple steps.

First, include this snaplet in your application's state.

> data App = App
>     { ... -- Other state needed in your app
>     , _db :: Snaplet Postgres
>     }

Next, call the pgsInit from your application's initializer.

> appInit = makeSnaplet ... $ do
>     ...
>     d <- nestSnaplet "db" db pgsInit
>     return $ App ... d

Now you can use any of the postgresql-simple wrapper functions defined in this
module anywhere in your application handlers.  For instance:

> postHandler :: Handler App App ()
> postHandler = do
>     posts <- with db $ query_ "select * from blog_post"
>     ...

Optionally, if you find yourself doing many database queries, you can eliminate some of the boilerplate by defining a HasPostgres instance for your application.

> instance HasPostgres (Handler b App) where
>   getPostgresState = with db get

With this code, our postHandler example no longer requires the 'with' function:

> postHandler :: Handler App App ()
> postHandler = do
>     posts <- query_ "select * from blog_post"
>     ...

The first time you run an application with the postgresql-simple snaplet, a
configuration file @snaplet.cfg@ in the @snaplets/postgresql-simple@ directory
underneath your project root.  It specifies how to connect to your PostgreSQL
server and what user, password, and database to use.  Edit this file and modify
the values appropriately and you'll be off and running.

If you want to have out-of-the-box authentication, look at the documentation
for the "Snap.Snaplet.Auth.Backends.PostgresqlSimple" module.

-}

module Snap.Snaplet.PostgresqlSimple (
  -- * The Snaplet
    Postgres(..)
  , HasPostgres(..)
  , pgsInit

  -- * Wrappers and re-exports
  , query
  , query_
  , fold
  , foldWithOptions
  , fold_
  , foldWithOptions_
  , forEach
  , forEach_
  , execute
  , execute_
  , executeMany
  , begin
  , beginLevel
  , beginMode
  , rollback
  , commit
  , withTransaction
  , withTransactionLevel
  , withTransactionMode
  , formatMany
  , formatQuery

  -- Re-exported from postgresql-simple
  , P.ConnectInfo(..)
  , P.Query
  , P.In(..)
  , P.Binary(..)
  , P.Only(..)
  , P.SqlError(..)
  , P.FormatError(..)
  , P.QueryError(..)
  , P.ResultError(..)
  , P.TransactionMode(..)
  , P.IsolationLevel(..)
  , P.ReadWriteMode(..)
  , ToRow(..)
  , FromRow(..)

  , P.defaultConnectInfo
  , P.defaultTransactionMode
  , P.defaultIsolationLevel
  , P.defaultReadWriteMode

  ) where

import           Prelude hiding (catch)

import           Control.Applicative
import           Control.Monad.CatchIO hiding (Handler)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Writer
import           Data.ByteString (ByteString)
import qualified Data.Configurator as C
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Pool
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import qualified Database.PostgreSQL.Simple as P
import           Snap.Snaplet
import           Paths_snaplet_postgresql_simple



------------------------------------------------------------------------------
-- | The state for the postgresql-simple snaplet. To use it in your app
-- include this in your application state and use pgsInit to initialize it.
data Postgres = Postgres
    { pgPool :: Pool P.Connection
    -- ^ Function for retrieving the connection pool
    }


------------------------------------------------------------------------------
-- | Instantiate this typeclass on 'Handler b YourAppState' so this snaplet
-- can find the connection source.
class (MonadCatchIO m) => HasPostgres m where
    getPostgresState :: m Postgres


------------------------------------------------------------------------------
-- | Default instance
instance HasPostgres (Handler b Postgres) where
    getPostgresState = get


logErr :: MonadIO m
       => t -> IO (Maybe a) -> WriterT [t] m (Maybe a)
logErr err m = do
    res <- liftIO m
    when (isNothing res) (tell [err])
    return res

------------------------------------------------------------------------------
-- | Initialize the snaplet
pgsInit :: SnapletInit b Postgres
pgsInit = makeSnaplet "postgresql-simple" description datadir $ do
    config <- getSnapletUserConfig
    (mci,errs) <- runWriterT $ do
        host <- logErr "Must specify postgres host" $ C.lookup config "host"
        port <- logErr "Must specify postgres port" $ C.lookup config "port"
        user <- logErr "Must specify postgres user" $ C.lookup config "user"
        pwd <- logErr "Must specify postgres pass" $ C.lookup config "pass"
        db <- logErr "Must specify postgres db" $ C.lookup config "db"
        return $ P.ConnectInfo <$> host <*> port <*> user <*> pwd <*> db
    let ci = fromMaybe (error $ intercalate "\n" errs) mci

    stripes <- liftIO $ C.lookupDefault 1 config "numStripes"
    idle <- liftIO $ C.lookupDefault 5 config "idleTime"
    resources <- liftIO $ C.lookupDefault 20 config "maxResourcesPerStripe"
    pool <- liftIO $ createPool (P.connect ci) P.close stripes
                                (realToFrac (idle :: Double)) resources
    return $ Postgres pool
  where
    description = "PostgreSQL abstraction"
    datadir = Just $ liftM (++"/resources/db") getDataDir


------------------------------------------------------------------------------
-- | Convenience function for executing a function that needs a database
-- connection.
withPG :: (HasPostgres m)
       => (P.Connection -> IO b) -> m b
withPG f = do
    s <- getPostgresState
    let pool = pgPool s
    liftIO $ withResource pool f


------------------------------------------------------------------------------
-- | See 'P.query'
query :: (HasPostgres m, ToRow q, FromRow r)
      => P.Query -> q -> m [r]
query q params = withPG (\c -> P.query c q params)


------------------------------------------------------------------------------
-- | See 'P.query_'
query_ :: (HasPostgres m, FromRow r) => P.Query -> m [r]
query_ q = withPG (\c -> P.query_ c q)


------------------------------------------------------------------------------
-- | 
fold :: (HasPostgres m,
         FromRow row,
         ToRow params,
         MonadCatchIO m)
     => P.Query -> params -> b -> (b -> row -> IO b) -> m b
fold template qs a f = withPG (\c -> P.fold c template qs a f)


------------------------------------------------------------------------------
-- | 
foldWithOptions :: (HasPostgres m,
                    FromRow row,
                    ToRow params,
                    MonadCatchIO m)
                => P.FoldOptions
                -> P.Query
                -> params
                -> b
                -> (b -> row -> IO b)
                -> m b
foldWithOptions opts template qs a f =
    withPG (\c -> P.foldWithOptions opts c template qs a f)


------------------------------------------------------------------------------
-- | 
fold_ :: (HasPostgres m,
          FromRow row,
          MonadCatchIO m)
      => P.Query -> b -> (b -> row -> IO b) -> m b
fold_ template a f = withPG (\c -> P.fold_ c template a f)


------------------------------------------------------------------------------
-- | 
foldWithOptions_ :: (HasPostgres m,
                     FromRow row,
                     MonadCatchIO m)
                 => P.FoldOptions
                 -> P.Query
                 -> b
                 -> (b -> row -> IO b)
                 -> m b
foldWithOptions_ opts template a f =
    withPG (\c -> P.foldWithOptions_ opts c template a f)


------------------------------------------------------------------------------
-- | 
forEach :: (HasPostgres m,
            FromRow r,
            ToRow q,
            MonadCatchIO m)
        => P.Query -> q -> (r -> IO ()) -> m ()
forEach template qs f = withPG (\c -> P.forEach c template qs f)


------------------------------------------------------------------------------
-- | 
forEach_ :: (HasPostgres m,
             FromRow r,
             MonadCatchIO m)
         => P.Query -> (r -> IO ()) -> m ()
forEach_ template f = withPG (\c -> P.forEach_ c template f)


------------------------------------------------------------------------------
-- | 
execute :: (HasPostgres m, ToRow q, MonadCatchIO m)
        => P.Query -> q -> m Int64
execute template qs = withPG (\c -> P.execute c template qs)


------------------------------------------------------------------------------
-- | 
execute_ :: (HasPostgres m, MonadCatchIO m)
         => P.Query -> m Int64
execute_ template = withPG (\c -> P.execute_ c template)


------------------------------------------------------------------------------
-- | 
executeMany :: (HasPostgres m, ToRow q, MonadCatchIO m)
        => P.Query -> [q] -> m Int64
executeMany template qs = withPG (\c -> P.executeMany c template qs)


begin :: (HasPostgres m, MonadCatchIO m) => m ()
begin = withPG P.begin


beginLevel :: (HasPostgres m, MonadCatchIO m)
           => P.IsolationLevel -> m ()
beginLevel lvl = withPG (P.beginLevel lvl)


beginMode :: (HasPostgres m, MonadCatchIO m)
          => P.TransactionMode -> m ()
beginMode mode = withPG (P.beginMode mode)


rollback :: (HasPostgres m, MonadCatchIO m) => m ()
rollback = withPG P.rollback


commit :: (HasPostgres m, MonadCatchIO m) => m ()
commit = withPG P.commit


withTransaction :: (HasPostgres m, MonadCatchIO m)
                => m a -> m a
withTransaction = withTransactionMode P.defaultTransactionMode


withTransactionLevel :: (HasPostgres m, MonadCatchIO m)
                     => P.IsolationLevel -> m a -> m a
withTransactionLevel lvl
    = withTransactionMode P.defaultTransactionMode { P.isolationLevel = lvl }


withTransactionMode :: (HasPostgres m, MonadCatchIO m)
                    => P.TransactionMode -> m a -> m a
withTransactionMode mode act = do
    beginMode mode
    r <- act `onException` rollback
    commit
    return r


formatMany :: (ToRow q, HasPostgres m, MonadCatchIO m)
           => P.Query -> [q] -> m ByteString
formatMany q qs = withPG (\c -> P.formatMany c q qs)


formatQuery :: (ToRow q, HasPostgres m, MonadCatchIO m)
            => P.Query -> q -> m ByteString
formatQuery q qs = withPG (\c -> P.formatQuery c q qs)


