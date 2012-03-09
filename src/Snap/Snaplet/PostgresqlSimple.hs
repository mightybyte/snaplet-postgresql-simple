{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Snap.Snaplet.PostgresqlSimple
  ( Postgres
  , HasPostgres(..)
  , pgsInit
  , query
  , query_

  -- Re-exported from postgresql-simple
  , P.ConnectInfo(..)
  , P.Query(..)
  , P.In(..)
  , P.Binary(..)
  , P.Only(..)
  , P.SqlError(..)
  , P.FormatError(..)
  , P.QueryError(..)
  , P.ResultError(..)
  , QueryParams(..)
  , QueryResults(..)

  , P.defaultConnectInfo

  ) where

import            Prelude hiding (catch)

import            Control.Exception (SomeException)
import            Control.Monad.CatchIO
import            Control.Monad.IO.Class
import            Control.Monad.State
import            Data.Int
import            Data.Pool
import            Data.Text (Text)
import            Database.PostgreSQL.Simple.QueryParams
import            Database.PostgreSQL.Simple.QueryResults
import qualified  Database.PostgreSQL.Simple as P
import            Snap.Snaplet


------------------------------------------------------------------------------
-- | 
data Postgres = Postgres
    { pgConnection :: Pool P.Connection
    }


------------------------------------------------------------------------------
-- | Instantiate this typeclass on 'Handler b YourSnapletState' so this snaplet
-- can find the connection source.
class (MonadCatchIO m, MonadState Postgres m) => HasPostgres m where
    getPostgresState :: m Postgres


------------------------------------------------------------------------------
-- | Initialise the snaplet
pgsInit :: P.ConnectInfo -> SnapletInit b Postgres
pgsInit ci = makeSnaplet "postgresql-simple" description Nothing $ do
    pool <- liftIO $ createPool (P.connect ci) P.close 1 5 20
    return $ Postgres pool
  where
    description = "PostgreSQL abstraction"


------------------------------------------------------------------------------
-- | Convenience function for executing a function that needs a database
-- connection.
withPG :: (MonadState Postgres m, MonadCatchIO m)
       => (P.Connection -> IO b) -> m b
withPG f = do
    pool <- gets pgConnection
    withResource pool (liftIO . f)


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


