{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
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
  , PGSConfig(..)
  , pgsDefaultConfig
  , mkPGSConfig 
  , pgsInit
  , pgsInit'
  , getConnectionString 

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
  , returning
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
  , ToRow(..)
  , FromRow(..)

  , P.defaultConnectInfo
  , P.defaultTransactionMode
  , P.defaultIsolationLevel
  , P.defaultReadWriteMode
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
import qualified Database.PostgreSQL.Simple.Transaction as P
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


description :: T.Text
description = "PostgreSQL abstraction"


datadir :: Maybe (IO FilePath)
datadir = Just $ liftM (++"/resources/db") getDataDir


------------------------------------------------------------------------------
-- | Initialize the snaplet
pgsInit :: SnapletInit b Postgres
pgsInit = makeSnaplet "postgresql-simple" description datadir $ do
    config <- mkPGSConfig =<< getSnapletUserConfig
    initHelper config


------------------------------------------------------------------------------
-- | Initialize the snaplet using a specific configuration.
pgsInit' :: PGSConfig -> SnapletInit b Postgres
pgsInit' config = makeSnaplet "postgresql-simple" description datadir $ do
    initHelper config


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
-- | Builds a PGSConfig object from a configurator Config object.  This
-- function uses getConnectionString to construct the connection string.  The
-- rest of the PGSConfig fields are obtained from \"numStripes\",
-- \"idleTime\", and \"maxResourcesPerStripe\".
mkPGSConfig :: MonadIO m => C.Config -> m PGSConfig
mkPGSConfig config = do
    connstr <- liftIO $ getConnectionString config
    stripes <- liftIO $ C.lookupDefault 1 config "numStripes"
    idle <- liftIO $ C.lookupDefault 5 config "idleTime"
    resources <- liftIO $ C.lookupDefault 20 config "maxResourcesPerStripe"
    return $ PGSConfig connstr stripes idle resources


initHelper :: MonadIO m => PGSConfig -> m Postgres
initHelper PGSConfig{..} = do
    pool <- liftIO $ createPool (P.connectPostgreSQL pgsConnStr) P.close
                                pgsNumStripes (realToFrac pgsIdleTime)
                                pgsResources
    return $ Postgres pool


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
withTransactionLevel lvl =
    withTransactionMode P.defaultTransactionMode { P.isolationLevel = lvl }


withTransactionMode :: (HasPostgres m, MonadCatchIO m)
                    => P.TransactionMode -> m a -> m a
withTransactionMode mode act = do
    beginMode mode
    r <- act `CIO.onException` rollback
    commit
    return r


formatMany :: (ToRow q, HasPostgres m, MonadCatchIO m)
           => P.Query -> [q] -> m ByteString
formatMany q qs = withPG (\c -> P.formatMany c q qs)


formatQuery :: (ToRow q, HasPostgres m, MonadCatchIO m)
            => P.Query -> q -> m ByteString
formatQuery q qs = withPG (\c -> P.formatQuery c q qs)
