{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
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
>   setLocalPostgresState s = local (set (db . snapletValue) s)

With this code, our postHandler example no longer requires the 'with' function:

> postHandler :: Handler App App ()
> postHandler = do
>     posts <- query_ "select * from blog_post"
>     ...

If you have code that runs multiple queries but you want to make sure that you only use one database connection then you can use the withPG function, like so:

> postHandler :: Handler App App ()
> postHandler = withPG $ do
>     posts <- query_ "select * from blog_post"
>     links <- query_ "select * from links"
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
  , withPG
  , P.Connection
  , liftPG
  , liftPG'

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
  , withTransaction
  , withTransactionLevel
  , withTransactionMode
  , withTransactionEither
  , withTransactionModeEither
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
  , P.begin
  , P.beginLevel
  , P.beginMode
  , P.rollback
  , P.commit
  , (P.:.)(..)
  , ToRow(..)
  , FromRow(..)

  , P.defaultConnectInfo
  , P.defaultTransactionMode
  , P.defaultIsolationLevel
  , P.defaultReadWriteMode
  , field

  ) where

import           Control.Applicative
import qualified Control.Exception                      as E
import           Control.Lens                           (set, (^#))
import           Control.Monad                          (liftM)
import           Control.Monad.IO.Class                 (MonadIO, liftIO)
import           Control.Monad.Reader                   (ReaderT, ask, asks,
                                                         local)
import           Control.Monad.State                    (get)
import           Control.Monad.Trans.Control            (MonadBaseControl,
                                                         liftBaseWith, restoreM)
import           Data.ByteString                        (ByteString)
import qualified Data.Configurator                      as C
import qualified Data.Configurator.Types                as C
import           Data.Int                               (Int64)
import           Data.Monoid                            (Monoid (..), (<>))
import           Data.Pool                              (createPool)
import           Data.Ratio                             (denominator, numerator)
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import qualified Data.Text.Lazy                         as TL
import qualified Data.Text.Lazy.Builder                 as TB
import qualified Data.Text.Lazy.Builder.Int             as TB
import qualified Data.Text.Lazy.Builder.RealFloat       as TB
import qualified Database.PostgreSQL.Simple             as P
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import qualified Database.PostgreSQL.Simple.Transaction as P
import           Paths_snaplet_postgresql_simple
import           Prelude                                hiding ((++))
import qualified Snap                                   as Snap
import           Snap.Snaplet.PostgresqlSimple.Internal

------------------------------------------------------------------------------
-- | Default instance
instance HasPostgres (Snap.Handler b Postgres) where
    getPostgresState = get
    setLocalPostgresState s = local (const s)


------------------------------------------------------------------------------
-- | A convenience instance to make it easier to use this snaplet in the
-- Initializer monad like this:
--
-- > d <- nestSnaplet "db" db pgsInit
-- > count <- liftIO $ runReaderT (execute "INSERT ..." params) d
instance {-# OVERLAPPING #-} (MonadIO m, MonadBaseControl IO m)
  => HasPostgres (ReaderT (Snap.Snaplet Postgres) m) where
    getPostgresState = asks (^# Snap.snapletValue)
    setLocalPostgresState s = local (set Snap.snapletValue s)

------------------------------------------------------------------------------
-- | A convenience instance to make it easier to use functions written for
-- this snaplet in non-snaplet contexts.
instance {-# OVERLAPPING #-} (MonadIO m, MonadBaseControl IO m)
  => HasPostgres (ReaderT Postgres m) where
    getPostgresState = ask
    setLocalPostgresState s = local (const s)


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
    connstr <- fmap mconcat $ mapM showParam params
    extra   <- fmap TB.fromText $ C.lookupDefault "" config "connectionString"
    return $! T.encodeUtf8 (TL.toStrict (TB.toLazyText (connstr <> extra)))
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
      let key = TB.fromText name <> eq
      case mval of
        Nothing           -> return mempty
        Just (C.Bool   x) -> return (key <> showBool x <> sp)
        Just (C.String x) -> return (key <> showText x <> sp)
        Just (C.Number x) -> return (key <> showNum  x <> sp)
        Just (C.List   _) -> return mempty

    showBool x = TB.decimal (fromEnum x)

    nd ratio = (numerator ratio, denominator ratio)

    showNum (nd -> (n,1)) = TB.decimal n
    showNum x             = TB.formatRealFloat TB.Fixed Nothing
                             ( fromIntegral (numerator   x)
                             / fromIntegral (denominator x) :: Double )

    showText x = qt <> loop x
      where
        loop (T.break escapeNeeded -> (a,b))
          = TB.fromText a <>
              case T.uncons b of
                Nothing     ->  qt
                Just (c,b') ->  escapeChar c <> loop b'

    escapeNeeded c = c == '\'' || c == '\\'

    escapeChar c = case c of
                     '\'' -> bs <> qt
                     '\\' -> bs <> bs
                     _    -> TB.singleton c


description :: T.Text
description = "PostgreSQL abstraction"


datadir :: Maybe (IO FilePath)
datadir = Just $ liftM (<>"/resources/db") getDataDir


------------------------------------------------------------------------------
-- | Initialize the snaplet
pgsInit :: Snap.SnapletInit b Postgres
pgsInit = Snap.makeSnaplet "postgresql-simple" description datadir $ do
    config <- mkPGSConfig =<< Snap.getSnapletUserConfig
    initHelper config


------------------------------------------------------------------------------
-- | Initialize the snaplet using a specific configuration.
pgsInit' :: PGSConfig -> Snap.SnapletInit b Postgres
pgsInit' config = Snap.makeSnaplet "postgresql-simple" description Nothing $
    initHelper config


------------------------------------------------------------------------------
-- | Builds a PGSConfig object from a configurator Config object.  This
-- function uses getConnectionString to construct the connection string.  The
-- rest of the PGSConfig fields are obtained from \"numStripes\",
-- \"idleTime\", and \"maxResourcesPerStripe\".
mkPGSConfig :: MonadIO m => C.Config -> m PGSConfig
mkPGSConfig config = liftIO $ do
    connstr <- getConnectionString config
    stripes <- C.lookupDefault 1 config "numStripes"
    idle <- C.lookupDefault 5 config "idleTime"
    resources <- C.lookupDefault 20 config "maxResourcesPerStripe"
    return $ PGSConfig connstr stripes idle resources


initHelper :: MonadIO m => PGSConfig -> m Postgres
initHelper PGSConfig{..} = do
    pool <- liftIO $ createPool (P.connectPostgreSQL pgsConnStr) P.close
                                pgsNumStripes (realToFrac pgsIdleTime)
                                pgsResources
    return $ PostgresPool pool


------------------------------------------------------------------------------
-- | See 'P.query'
query :: (HasPostgres m, ToRow q, FromRow r)
      => P.Query -> q -> m [r]
query q params = liftPG' (\c ->  P.query c q params)


------------------------------------------------------------------------------
-- | See 'P.query_'
query_ :: (HasPostgres m, FromRow r) => P.Query -> m [r]
query_ q = liftPG' (`P.query_` q)


------------------------------------------------------------------------------
-- | See 'P.returning'
returning :: (HasPostgres m, ToRow q, FromRow r)
      => P.Query -> [q] -> m [r]
returning q params = liftPG' (\c -> P.returning c q params)


------------------------------------------------------------------------------
-- |
fold :: (HasPostgres m,
         FromRow row,
         ToRow params)
     => P.Query -> params -> b -> (b -> row -> IO b) -> m b
fold template qs a f = liftPG' (\c -> P.fold c template qs a f)


------------------------------------------------------------------------------
-- |
foldWithOptions :: (HasPostgres m,
                    FromRow row,
                    ToRow params)
                => P.FoldOptions
                -> P.Query
                -> params
                -> b
                -> (b -> row -> IO b)
                -> m b
foldWithOptions opts template qs a f =
    liftPG' (\c -> P.foldWithOptions opts c template qs a f)


------------------------------------------------------------------------------
-- |
fold_ :: (HasPostgres m,
          FromRow row)
      => P.Query -> b -> (b -> row -> IO b) -> m b
fold_ template a f = liftPG' (\c -> P.fold_ c template a f)


------------------------------------------------------------------------------
-- |
foldWithOptions_ :: (HasPostgres m,
                     FromRow row)
                 => P.FoldOptions
                 -> P.Query
                 -> b
                 -> (b -> row -> IO b)
                 -> m b
foldWithOptions_ opts template a f =
    liftPG' (\c -> P.foldWithOptions_ opts c template a f)


------------------------------------------------------------------------------
-- |
forEach :: (HasPostgres m,
            FromRow r,
            ToRow q)
        => P.Query -> q -> (r -> IO ()) -> m ()
forEach template qs f = liftPG' (\c -> P.forEach c template qs f)


------------------------------------------------------------------------------
-- |
forEach_ :: (HasPostgres m,
             FromRow r)
         => P.Query -> (r -> IO ()) -> m ()
forEach_ template f = liftPG' (\c -> P.forEach_ c template f)


------------------------------------------------------------------------------
-- |
execute :: (HasPostgres m, ToRow q)
        => P.Query -> q -> m Int64
execute template qs = liftPG' (\c -> P.execute c template qs)


------------------------------------------------------------------------------
-- |
execute_ :: (HasPostgres m)
         => P.Query -> m Int64
execute_ template = liftPG' (`P.execute_` template)


------------------------------------------------------------------------------
-- |
executeMany :: (HasPostgres m, ToRow q)
        => P.Query -> [q] -> m Int64
executeMany template qs = liftPG' (\c -> P.executeMany c template qs)


------------------------------------------------------------------------------
-- | Be careful that you do not call Snap's `finishWith` function anywhere
-- inside the function that you pass to `withTransaction`.  Doing so has been
-- known to cause DB connection leaks.
withTransaction :: (HasPostgres m)
                => m a -> m a
withTransaction = withTransactionMode P.defaultTransactionMode


------------------------------------------------------------------------------
-- | Be careful that you do not call Snap's `finishWith` function anywhere
-- inside the function that you pass to `withTransactionLevel`.  Doing so has
-- been known to cause DB connection leaks.
withTransactionLevel :: (HasPostgres m)
                     => P.IsolationLevel -> m a -> m a
withTransactionLevel lvl =
    withTransactionMode P.defaultTransactionMode { P.isolationLevel = lvl }


------------------------------------------------------------------------------
-- | Be careful that you do not call Snap's `finishWith` function anywhere
-- inside the function that you pass to `withTransactionMode`.  Doing so has
-- been known to cause DB connection leaks.
withTransactionMode :: (HasPostgres m)
                    => P.TransactionMode -> m a -> m a
withTransactionMode mode act = withPG $ do
    pg <- getPostgresState
    r <- liftBaseWith $ \run -> E.mask
                      $ \unmask -> withConnection pg
                      $ \con -> do
        P.beginMode mode con
        r <- unmask (run act) `E.onException` P.rollback con
        P.commit con
        return r
    restoreM r

------------------------------------------------------------------------------
-- | Be careful that you do not call Snap's `finishWith` function anywhere
-- inside the function that you pass to `withTransactionMode`.  Doing so has
-- been known to cause DB connection leaks.
withTransactionEither :: (HasPostgres m)
                      => m (Either a b) -> m (Either a b)
withTransactionEither = withTransactionModeEither P.defaultTransactionMode

------------------------------------------------------------------------------
-- | Be careful that you do not call Snap's `finishWith` function anywhere
-- inside the function that you pass to `withTransactionMode`.  Doing so has
-- been known to cause DB connection leaks.
withTransactionModeEither :: (HasPostgres m)
                          => P.TransactionMode -> m (Either a b) -> m (Either a b)
withTransactionModeEither mode act = withPG $ do
    pg <- getPostgresState
    r <- liftBaseWith $ \run -> E.mask
                      $ \unmask -> withConnection pg
                      $ \con -> do
        P.beginMode mode con
        r <- unmask (run act) `E.onException` P.rollback con
        either (const $ P.rollback con) (const $ P.commit con) $ restoreM r
        return r
    restoreM r

formatMany :: (ToRow q, HasPostgres m)
           => P.Query -> [q] -> m ByteString
formatMany q qs = liftPG' (\c -> P.formatMany c q qs)


formatQuery :: (ToRow q, HasPostgres m)
            => P.Query -> q -> m ByteString
formatQuery q qs = liftPG' (\c -> P.formatQuery c q qs)
