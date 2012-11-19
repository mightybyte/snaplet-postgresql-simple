{-|

This module provides alternative interface to Postgres Snaplet and is intented
to provide safer usage of transactions.

Setup your snaplet as in "Snap.Snaplet.PostgresqlSimple" and in handler use
'withPostgres' or 'withPgTransaction'.

-}

module Snap.Snaplet.PostgresqlSimple.Action
  ( PgAction
  , withPostgres
  , withPgTransaction
  -- * Query wrappers
  , query
  , query_
  , execute
  , execute_
  , executeMany
  , returning
  -- * Transactions
  , begin
  , beginLevel
  , beginMode
  , rollback
  , commit
  , withTransaction
  , withTransactionLevel
  , withTransactionMode
  -- * Folds
  , fold
  , foldWithOptions
  , fold_
  , foldWithOptions_
  , forEach
  , forEach_
  -- * Debugging helpers
  , formatMany
  , formatQuery
  -- * Re-exported from postgresql-simple
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
  , (P.:.)(..)
  , ToRow
  , FromRow

  , P.defaultConnectInfo
  , P.defaultTransactionMode
  , P.defaultIsolationLevel
  , P.defaultReadWriteMode
  ) where

import           Control.Monad              (liftM)
import           Control.Monad.IO.Class     (liftIO, MonadIO)
import           Control.Monad.Trans.Reader
import           Control.Monad.CatchIO      (MonadCatchIO, onException)
import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.Pool                  (withResource)
import           Database.PostgreSQL.Simple (FromRow, ToRow, Query)
import qualified Database.PostgreSQL.Simple as P

import           Snap.Snaplet.PostgresqlSimple (HasPostgres(..),pgPool)

type PgAction m a = ReaderT P.Connection m a


withPostgres :: HasPostgres m => PgAction m a -> m a
withPostgres act = do
    pool <- liftM pgPool getPostgresState
    withResource pool $ runReaderT act


withPgTransaction :: HasPostgres m => PgAction m a -> m a
withPgTransaction =  withPostgres . withTransaction


query :: (ToRow q, FromRow r, MonadIO m) => Query -> q -> PgAction m [r]
query q p = ask >>= \c -> liftIO $ P.query c q p


query_ :: (FromRow r, MonadIO m) => Query -> PgAction m [r]
query_ q = ask >>= \c -> liftIO $ P.query_ c q


execute :: (ToRow q, MonadIO m) => Query -> q -> PgAction m Int64
execute q p = ask >>= \c -> liftIO $ P.execute c q p


execute_ :: (MonadIO m) => Query -> PgAction m Int64
execute_ q = ask >>= \c -> liftIO $ P.execute_ c q


executeMany :: (ToRow q, MonadIO m) => Query -> [q] -> PgAction m Int64
executeMany q ps = ask >>= \c -> liftIO $ P.executeMany c q ps


returning :: (ToRow q, FromRow r, MonadIO m) => Query -> [q] -> PgAction m [r]
returning q ps = ask >>= \c -> liftIO $ P.returning c q ps


begin :: MonadIO m => PgAction m ()
begin = ask >>= \c -> liftIO $ P.begin c


beginLevel :: MonadIO m => P.IsolationLevel -> PgAction m ()
beginLevel l = ask >>= \c -> liftIO $ P.beginLevel l c


beginMode :: MonadIO m => P.TransactionMode -> PgAction m ()
beginMode m = ask >>= \c -> liftIO $ P.beginMode m c


rollback :: MonadIO m => PgAction m ()
rollback = ask >>= \c -> liftIO $ P.rollback c


commit :: MonadIO m => PgAction m ()
commit = ask >>= \c -> liftIO $ P.commit c


withTransactionMode :: MonadCatchIO m =>
                    P.TransactionMode -> PgAction m a -> PgAction m a
withTransactionMode m act = do
    beginMode m
    r <- act `onException` rollback
    commit
    return r


withTransactionLevel :: MonadCatchIO m =>
                     P.IsolationLevel -> PgAction m a -> PgAction m a
withTransactionLevel l =
    withTransactionMode P.defaultTransactionMode { P.isolationLevel = l }


withTransaction :: MonadCatchIO m => PgAction m a -> PgAction m a
withTransaction = withTransactionMode P.defaultTransactionMode


formatMany :: (ToRow q, MonadIO m) => Query -> [q] -> PgAction m ByteString
formatMany q ps = ask >>= \c -> liftIO $ P.formatMany c q ps


formatQuery :: (ToRow q, MonadIO m) => Query -> q -> PgAction m ByteString
formatQuery q p = ask >>= \c -> liftIO $ P.formatQuery c q p


fold :: (ToRow p, FromRow r, MonadIO m) =>
     Query -> p -> b -> (b -> r -> IO b) -> PgAction m b
fold q ps a f = ask >>= \c -> liftIO $ P.fold c q ps a f


fold_ :: (FromRow r, MonadIO m) =>
      Query -> b -> (b -> r -> IO b) -> PgAction m b
fold_ q a f = ask >>= \c -> liftIO $ P.fold_ c q a f


foldWithOptions :: (ToRow p, FromRow r, MonadIO m) =>
            P.FoldOptions -> Query -> p -> b -> (b -> r -> IO b) -> PgAction m b
foldWithOptions opts q p a f =
    ask >>= \c -> liftIO $ P.foldWithOptions opts c q p a f


foldWithOptions_ :: (FromRow r, MonadIO m) =>
                 P.FoldOptions -> Query -> b -> (b -> r -> IO b) -> PgAction m b
foldWithOptions_ opts q a f =
    ask >>= \c -> liftIO $ P.foldWithOptions_ opts c q a f


forEach :: (ToRow p, FromRow r, MonadIO m) =>
        Query -> p -> (r -> IO ()) -> PgAction m ()
forEach q p f = ask >>= \c -> liftIO $ P.forEach c q p f


forEach_ :: (FromRow r, MonadIO m) =>
        Query -> (r -> IO ()) -> PgAction m ()
forEach_ q f = ask >>= \c -> liftIO $ P.forEach_ c q f