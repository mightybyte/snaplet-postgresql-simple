{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Snap.Snaplet.PostgresqlSimple
  ( Postgres
  , pgPool
  , HasPostgres(..)
  , pgsInit
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
  , QueryParams(..)
  , QueryResults(..)

  , P.defaultConnectInfo
  , P.defaultTransactionMode
  , P.defaultIsolationLevel
  , P.defaultReadWriteMode

  ) where

import           Prelude hiding (catch)

import           Control.Applicative
import           Control.Monad.CatchIO
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.ByteString (ByteString)
import qualified Data.Configurator as C
import           Data.Int
import           Data.Maybe
import           Data.Pool
import           Database.PostgreSQL.Simple.QueryParams
import           Database.PostgreSQL.Simple.QueryResults
import qualified Database.PostgreSQL.Simple as P
import           Snap.Snaplet


------------------------------------------------------------------------------
-- | 
data Postgres = Postgres
    { pgPool :: Pool P.Connection
    , pgConnection :: Maybe P.Connection
    }


------------------------------------------------------------------------------
-- | Instantiate this typeclass on 'Handler b YourSnapletState' so this snaplet
-- can find the connection source.
class (MonadCatchIO m, MonadState Postgres m) => HasPostgres m where
    getPostgresState :: m Postgres


------------------------------------------------------------------------------
-- | Initialise the snaplet
pgsInit :: SnapletInit b Postgres
pgsInit = makeSnaplet "postgresql-simple" description Nothing $ do
    config <- getSnapletUserConfig
    host <- liftIO $ C.lookup config "host"
    port <- liftIO $ C.lookup config "port"
    user <- liftIO $ C.lookup config "user"
    pass <- liftIO $ C.lookup config "pass"
    db <- liftIO $ C.lookup config "db"
    let mci = P.ConnectInfo <$> host <*> port <*> user <*> pass <*> db
        ci = fromMaybe (error "Must supply database server information in config file") mci

    pool <- liftIO $ createPool (P.connect ci) P.close 1 5 20
    return $ Postgres pool Nothing
  where
    description = "PostgreSQL abstraction"


------------------------------------------------------------------------------
-- | Convenience function for executing a function that needs a database
-- connection.
withPG :: (MonadState Postgres m, MonadCatchIO m)
       => (P.Connection -> IO b) -> m b
withPG f = do
    mConn <- gets pgConnection
    maybe getResource (liftIO . f) mConn
  where
    getResource = do
        pool <- gets pgPool
        withResource pool wrapper
    wrapper c = do
        modify (\p -> p { pgConnection = Just c })
        liftIO $ f c


------------------------------------------------------------------------------
-- | 
query :: (HasPostgres m, QueryParams q, QueryResults r)
      => P.Query -> q -> m [r]
query q params = withPG (\c -> P.query c q params)


------------------------------------------------------------------------------
-- | 
query_ :: (HasPostgres m, QueryResults r) => P.Query -> m [r]
query_ q = withPG (\c -> P.query_ c q)


------------------------------------------------------------------------------
-- | 
fold :: (MonadState Postgres m,
         QueryResults row,
         QueryParams params,
         MonadCatchIO m)
     => P.Query -> params -> b -> (b -> row -> IO b) -> m b
fold template qs a f = withPG (\c -> P.fold c template qs a f)


------------------------------------------------------------------------------
-- | 
foldWithOptions :: (MonadState Postgres m,
                    QueryResults row,
                    QueryParams params,
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
fold_ :: (MonadState Postgres m,
          QueryResults row,
          MonadCatchIO m)
      => P.Query -> b -> (b -> row -> IO b) -> m b
fold_ template a f = withPG (\c -> P.fold_ c template a f)


------------------------------------------------------------------------------
-- | 
foldWithOptions_ :: (MonadState Postgres m,
                     QueryResults row,
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
forEach :: (MonadState Postgres m,
            QueryResults r,
            QueryParams q,
            MonadCatchIO m)
        => P.Query -> q -> (r -> IO ()) -> m ()
forEach template qs f = withPG (\c -> P.forEach c template qs f)


------------------------------------------------------------------------------
-- | 
forEach_ :: (MonadState Postgres m,
             QueryResults r,
             MonadCatchIO m)
         => P.Query -> (r -> IO ()) -> m ()
forEach_ template f = withPG (\c -> P.forEach_ c template f)


------------------------------------------------------------------------------
-- | 
execute :: (MonadState Postgres m, QueryParams q, MonadCatchIO m)
        => P.Query -> q -> m Int64
execute template qs = withPG (\c -> P.execute c template qs)


------------------------------------------------------------------------------
-- | 
execute_ :: (MonadState Postgres m, MonadCatchIO m)
         => P.Query -> m Int64
execute_ template = withPG (\c -> P.execute_ c template)


------------------------------------------------------------------------------
-- | 
executeMany :: (MonadState Postgres m, QueryParams q, MonadCatchIO m)
        => P.Query -> [q] -> m Int64
executeMany template qs = withPG (\c -> P.executeMany c template qs)


begin :: (MonadState Postgres m, MonadCatchIO m) => m ()
begin = withPG P.begin


beginLevel :: (MonadState Postgres m, MonadCatchIO m)
           => P.IsolationLevel -> m ()
beginLevel lvl = withPG (P.beginLevel lvl)


beginMode :: (MonadState Postgres m, MonadCatchIO m)
          => P.TransactionMode -> m ()
beginMode mode = withPG (P.beginMode mode)


rollback :: (MonadState Postgres m, MonadCatchIO m) => m ()
rollback = withPG P.rollback


commit :: (MonadState Postgres m, MonadCatchIO m) => m ()
commit = withPG P.commit


withTransaction :: (MonadState Postgres m, MonadCatchIO m)
                => m a -> m a
withTransaction = withTransactionMode P.defaultTransactionMode


withTransactionLevel :: (MonadState Postgres m, MonadCatchIO m)
                     => P.IsolationLevel -> m a -> m a
withTransactionLevel lvl
    = withTransactionMode P.defaultTransactionMode { P.isolationLevel = lvl }


withTransactionMode :: (MonadState Postgres m, MonadCatchIO m)
                    => P.TransactionMode -> m a -> m a
withTransactionMode mode act = do
    beginMode mode
    r <- act `onException` rollback
    commit
    return r


formatMany :: (QueryParams q, MonadState Postgres m, MonadCatchIO m)
           => P.Query -> [q] -> m ByteString
formatMany q qs = withPG (\c -> P.formatMany c q qs)


formatQuery :: (QueryParams q, MonadState Postgres m, MonadCatchIO m)
            => P.Query -> q -> m ByteString
formatQuery q qs = withPG (\c -> P.formatQuery c q qs)


