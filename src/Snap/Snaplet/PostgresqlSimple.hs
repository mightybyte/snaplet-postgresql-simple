{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.PostgresqlSimple
  ( Postgresql
  , HasPostgres(..)
  , pgsInit
  ) where

import            Prelude hiding (catch)

import            Control.Exception (SomeException)
import            Control.Monad.CatchIO
import            Control.Monad.IO.Class
import            Data.Pool
import            Database.PostgreSQL.Simple
import            Snap.Snaplet


------------------------------------------------------------------------------
-- | 
data Postgres = Postgres
    { pgConnection :: Pool Connection
    }


------------------------------------------------------------------------------
-- | Instantiate this typeclass on 'Handler b YourSnapletState' so this snaplet
-- can find the connection source.
class HasPostgres m where
  getPostgresState :: m Postgres


description :: Text
description = "PostgreSQL abstraction"


------------------------------------------------------------------------------
-- | Initialise the snaplet
pgsInit :: ConnectInfo -> SnapletInit b Postgres
pgsInit ci = makeSnaplet "postgresql-simple" description Nothing $ do
  pool <- createPool (connect ci) close 1 5 20
  return $ Postgres pool


