{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{-|

This snaplet makes it simple to use a PostgreSQL database from your Snap
application and is based on the excellent postgresql-simple library
(<http://hackage.haskell.org/package/postgresql-simple>) by Leon Smith
(adapted from Bryan O\'Sullivan\'s mysql-simple).  Now, adding a database
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
configuration file @devel.cfg@ is created in the @snaplets/postgresql-simple@
directory underneath your project root.  It specifies how to connect to your
PostgreSQL server and what user, password, and database to use.  Edit this
file and modify the values appropriately and you'll be off and running.

If you want to have out-of-the-box authentication, look at the documentation
for the "Snap.Snaplet.Auth.Backends.PostgresqlSimple" module.

-}

module Snap.Snaplet.PostgresqlSimple (
  -- * The Snaplet
    Postgres(..)
  , HasPostgres(..)
  , pgsInit
  , withPG

  -- * Wrappers and re-exports
  , query
  , query_
  , execute
  , execute_
  , executeMany
  , returning
  , formatMany
  , formatQuery

  -- Re-exported from postgresql-simple
  , P.Query
  , P.In(..)
  , P.Binary(..)
  , P.Only(..)
  , P.SqlError(..)
  , P.FormatError(..)
  , P.QueryError(..)
  , P.ResultError(..)
  , (P.:.)(..)
  , ToRow(..)
  , FromRow(..)

  , field

  ) where

import           Prelude hiding ((++))
import           Control.Applicative
import           Control.Monad.CatchIO (MonadCatchIO)
import qualified Control.Monad.CatchIO as CIO
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Reader
import           Data.ByteString (ByteString)
import           Data.Monoid(Monoid(..))
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import           Data.Int
import           Data.Ratio
import           Data.Pool
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import qualified Database.PostgreSQL.Simple as P
import           Snap
import           Paths_snaplet_postgresql_simple

-- This is actually more portable than using <>
(++) :: Monoid a => a -> a -> a
(++) = mappend
infixr 5 ++

------------------------------------------------------------------------------
-- | The state for the postgresql-simple snaplet. To use it in your app
-- include this in your application state and use pgsInit to initialize it.
data Postgres = Postgres
    { pgPool :: Pool P.Connection
    -- ^ Function for retrieving the connection pool
    }


------------------------------------------------------------------------------
-- | Instantiate this typeclass on 'Handler b YourAppState' so this snaplet
-- can find the connection source.  If you need to have multiple instances of
-- the postgres snaplet in your application, then don't provide this instance
-- and leverage the default instance by using \"@with dbLens@\" in front of calls
-- to snaplet-postgresql-simple functions.
class (MonadCatchIO m) => HasPostgres m where
    getPostgresState :: m Postgres


------------------------------------------------------------------------------
-- | Default instance
instance HasPostgres (Handler b Postgres) where
    getPostgresState = get


------------------------------------------------------------------------------
-- | A convenience instance to make it easier to use this snaplet in the
-- Initializer monad like this:
--
-- > d <- nestSnaplet "db" db pgsInit
-- > count <- liftIO $ runReaderT (execute "INSERT ..." params) d
instance (MonadCatchIO m) => HasPostgres (ReaderT (Snaplet Postgres) m) where
    getPostgresState = asks (^# snapletValue)


------------------------------------------------------------------------------
-- | A convenience instance to make it easier to use functions written for
-- this snaplet in non-snaplet contexts.
instance (MonadCatchIO m) => HasPostgres (ReaderT Postgres m) where
    getPostgresState = ask


------------------------------------------------------------------------------
-- | Produce a connection string from a config
getConnectionString :: C.Config -> IO ByteString
getConnectionString config = do
    let params =
            [ ["host"]
            , ["hostaddr"]
            , ["port"]
            , ["dbname","db"]
            , ["user"]
            , ["password","pass"]
            , ["connection_timeout"]
            , ["client_encoding"]
            , ["options"]
            , ["application_name"]
            , ["fallback_application_name"]
            , ["keepalives"]
            , ["keepalives_idle"]
            , ["keepalives_interval"]
            , ["keepalives_count"]
            , ["sslmode"]
            , ["sslcompression"]
            , ["sslcert"]
            , ["sslkey"]
            , ["sslrootcert"]
            , ["sslcrl"]
            , ["requirepeer"]
            , ["krbsrvname"]
            , ["gsslib"]
            , ["service"]
            ]
    connstr <- mconcat <$> mapM showParam params
    extra <- TB.fromText <$> C.lookupDefault "" config "connectionString"
    return $! T.encodeUtf8 (TL.toStrict (TB.toLazyText (connstr ++ extra)))
  where
    qt = TB.singleton '\''
    bs = TB.singleton '\\'
    sp = TB.singleton ' '
    eq = TB.singleton '='

    lookupConfig = foldr (\name names -> do
                            mval <- C.lookup config name
                            case mval of
                              Nothing -> names
                              Just _  -> return mval)
                         (return Nothing)

    showParam [] = undefined
    showParam names@(name:_) = do
      mval :: Maybe C.Value <- lookupConfig names
      let key = TB.fromText name ++ eq
      case mval of
        Nothing           -> return mempty
        Just (C.Bool   x) -> return (key ++ showBool x ++ sp)
        Just (C.String x) -> return (key ++ showText x ++ sp)
        Just (C.Number x) -> return (key ++ showNum  x ++ sp)
        Just (C.List   _) -> return mempty

    showBool x = TB.decimal (fromEnum x)

    showNum  x = TB.formatRealFloat TB.Fixed Nothing
                   ( fromIntegral (numerator   x)
                   / fromIntegral (denominator x) :: Double )

    showText x = qt ++ loop x
      where
        loop (T.break escapeNeeded -> (a,b))
          = TB.fromText a ++
              case T.uncons b of
                Nothing      ->  qt
                Just (c,b')  ->  escapeChar c ++ loop b'

    escapeNeeded c = c == '\'' || c == '\\'

    escapeChar c = case c of
                     '\'' -> bs ++ qt
                     '\\' -> bs ++ bs
                     _    -> TB.singleton c



------------------------------------------------------------------------------
-- | Initialize the snaplet
pgsInit :: SnapletInit b Postgres
pgsInit = makeSnaplet "postgresql-simple" description datadir $ do
    config <- getSnapletUserConfig
    connstr <- liftIO $ getConnectionString config
    stripes <- liftIO $ C.lookupDefault 1 config "numStripes"
    idle <- liftIO $ C.lookupDefault 5 config "idleTime"
    resources <- liftIO $ C.lookupDefault 20 config "maxResourcesPerStripe"
    pool <- liftIO $ createPool (P.connectPostgreSQL connstr) P.close stripes
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
-- | See 'P.returning'
returning :: (HasPostgres m, ToRow q, FromRow r)
      => P.Query -> [q] -> m [r]
returning q params = withPG (\c -> P.returning c q params)

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


formatMany :: (ToRow q, HasPostgres m, MonadCatchIO m)
           => P.Query -> [q] -> m ByteString
formatMany q qs = withPG (\c -> P.formatMany c q qs)


formatQuery :: (ToRow q, HasPostgres m, MonadCatchIO m)
            => P.Query -> q -> m ByteString
formatQuery q qs = withPG (\c -> P.formatQuery c q qs)
