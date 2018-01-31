{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|

This module allows you to use the auth snaplet with your user database stored
in a PostgreSQL database.  When you run your application with this snaplet, a
config file will be copied into the the @snaplets/postgresql-auth@ directory.
This file contains all of the configurable options for the snaplet and allows
you to change them without recompiling your application.

To use this snaplet in your application enable the session, postgres, and auth
snaplets as follows:

> data App = App
>     { ... -- your own application state here
>     , _sess :: Snaplet SessionManager
>     , _db   :: Snaplet Postgres
>     , _auth :: Snaplet (A.AuthManager App)
>     }

Then in your initializer you'll have something like this:

> d <- nestSnaplet "db" db pgsInit
> a <- nestSnaplet "auth" auth $ initPostgresAuth sess d

If you have not already created the database table for users, it will
automatically be created for you the first time you run your application.

-}

module Snap.Snaplet.Auth.Backends.PostgresqlSimple
  ( initPostgresAuth
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import qualified Control.Exception                      as E
import           Control.Lens                           ((^#))
import           Control.Monad                          (liftM, void, when)
import           Control.Monad.Trans                    (liftIO)
import qualified Data.Configurator                      as C
import qualified Data.HashMap.Lazy                      as HM
import           Data.Maybe                             (fromMaybe, listToMaybe)
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import qualified Database.PostgreSQL.Simple             as P
import           Database.PostgreSQL.Simple.FromField   (FromField, fromField)
import qualified Database.PostgreSQL.Simple.ToField     as P
import           Database.PostgreSQL.Simple.Types       (Query (Query))
import           Paths_snaplet_postgresql_simple
import           Prelude
import           Snap                                   (Snaplet, SnapletInit,
                                                         SnapletLens,
                                                         getSnapletUserConfig,
                                                         makeSnaplet,
                                                         snapletValue)
import qualified Snap.Snaplet.Auth                      as A
import           Snap.Snaplet.PostgresqlSimple          (FromRow, Only, ToRow,
                                                         field, fromRow)
import           Snap.Snaplet.PostgresqlSimple.Internal (Postgres,
                                                         withConnection)
import           Snap.Snaplet.Session                   (SessionManager, mkRNG)
import           Web.ClientSession                      (getKey)
------------------------------------------------------------------------------


data PostgresAuthManager = PostgresAuthManager
    { pamTable :: AuthTable
    , pamConn  :: Postgres
    }


------------------------------------------------------------------------------
-- | Initializer for the postgres backend to the auth snaplet.
--
initPostgresAuth
  :: SnapletLens b SessionManager  -- ^ Lens to the session snaplet
  -> Snaplet Postgres  -- ^ The postgres snaplet
  -> SnapletInit b (A.AuthManager b)
initPostgresAuth sess db = makeSnaplet "postgresql-auth" desc datadir $ do
    config <- getSnapletUserConfig
    authTable <- liftIO $ C.lookupDefault "snap_auth_user" config "authTable"
    authSettings <- A.authSettingsFromConfig
    key <- liftIO $ getKey (A.asSiteKey authSettings)
    let tableDesc = defAuthTable { tblName = authTable }
    let manager = PostgresAuthManager tableDesc $ db ^# snapletValue
    liftIO $ createTableIfMissing manager
    rng <- liftIO mkRNG
    return A.AuthManager
      { backend = manager
      , session = sess
      , activeUser = Nothing
      , minPasswdLen = A.asMinPasswdLen authSettings
      , rememberCookieName = A.asRememberCookieName authSettings
      , rememberCookieDomain = Nothing
      , rememberPeriod = A.asRememberPeriod authSettings
      , siteKey = key
      , lockout = A.asLockout authSettings
      , randomNumberGenerator = rng
      }
  where
    desc = "A PostgreSQL backend for user authentication"
    datadir = Just $ liftM (++"/resources/auth") getDataDir


------------------------------------------------------------------------------
-- | Create the user table if it doesn't exist.
createTableIfMissing :: PostgresAuthManager -> IO ()
createTableIfMissing PostgresAuthManager{..} = do
    withConnection pamConn $ \conn -> do
        res <- P.query_ conn $ Query $ T.encodeUtf8 $
          "select relname from pg_class where relname='"
          `T.append` schemaless (tblName pamTable) `T.append` "'"
        when (null (res :: [Only T.Text])) $
          void (P.execute_ conn (Query $ T.encodeUtf8 q))
    return ()
  where
    schemaless = T.reverse . T.takeWhile (/='.') . T.reverse
    q = T.concat
          [ "CREATE TABLE \""
          , tblName pamTable
          , "\" ("
          , T.intercalate "," (map (fDesc . ($pamTable) . fst) colDef)
          , "); "
          , "CREATE INDEX email_idx ON \""
          , tblName pamTable
          , "\" (email);"
          ]

buildUid :: Int -> A.UserId
buildUid = A.UserId . T.pack . show


instance FromField A.UserId where
    fromField f v = buildUid <$> fromField f v

instance FromField A.Password where
    fromField f v = A.Encrypted <$> fromField f v

instance FromRow A.AuthUser where
    fromRow =
        A.AuthUser
        <$> _userId
        <*> _userLogin
        <*> _userEmail
        <*> _userPassword
        <*> _userActivatedAt
        <*> _userSuspendedAt
        <*> _userRememberToken
        <*> _userLoginCount
        <*> _userFailedLoginCount
        <*> _userLockedOutUntil
        <*> _userCurrentLoginAt
        <*> _userLastLoginAt
        <*> _userCurrentLoginIp
        <*> _userLastLoginIp
        <*> _userCreatedAt
        <*> _userUpdatedAt
        <*> _userResetToken
        <*> _userResetRequestedAt
        <*> _userRoles
        <*> _userMeta
      where
        !_userId               = field
        !_userLogin            = field
        !_userEmail            = field
        !_userPassword         = field
        !_userActivatedAt      = field
        !_userSuspendedAt      = field
        !_userRememberToken    = field
        !_userLoginCount       = field
        !_userFailedLoginCount = field
        !_userLockedOutUntil   = field
        !_userCurrentLoginAt   = field
        !_userLastLoginAt      = field
        !_userCurrentLoginIp   = field
        !_userLastLoginIp      = field
        !_userCreatedAt        = field
        !_userUpdatedAt        = field
        !_userResetToken       = field
        !_userResetRequestedAt = field
        !_userRoles            = pure []
        !_userMeta             = pure HM.empty


querySingle :: (ToRow q, FromRow a)
            => Postgres -> Query -> q -> IO (Maybe a)
querySingle pc q ps = withConnection pc $ \conn -> return . listToMaybe =<<
    P.query conn q ps

authExecute :: ToRow q
            => Postgres -> Query -> q -> IO ()
authExecute pc q ps = do
    withConnection pc $ \conn -> P.execute conn q ps
    return ()

instance P.ToField A.Password where
    toField (A.ClearText bs) = P.toField bs
    toField (A.Encrypted bs) = P.toField bs


-- | Datatype containing the names of the columns for the authentication table.
data AuthTable
  =  AuthTable
  {  tblName             :: Text
  ,  colId               :: (Text, Text)
  ,  colLogin            :: (Text, Text)
  ,  colEmail            :: (Text, Text)
  ,  colPassword         :: (Text, Text)
  ,  colActivatedAt      :: (Text, Text)
  ,  colSuspendedAt      :: (Text, Text)
  ,  colRememberToken    :: (Text, Text)
  ,  colLoginCount       :: (Text, Text)
  ,  colFailedLoginCount :: (Text, Text)
  ,  colLockedOutUntil   :: (Text, Text)
  ,  colCurrentLoginAt   :: (Text, Text)
  ,  colLastLoginAt      :: (Text, Text)
  ,  colCurrentLoginIp   :: (Text, Text)
  ,  colLastLoginIp      :: (Text, Text)
  ,  colCreatedAt        :: (Text, Text)
  ,  colUpdatedAt        :: (Text, Text)
  ,  colResetToken       :: (Text, Text)
  ,  colResetRequestedAt :: (Text, Text)
  ,  rolesTable          :: Text
  }

-- | Default authentication table layout
defAuthTable :: AuthTable
defAuthTable
  =  AuthTable
  {  tblName             = "snap_auth_user"
  ,  colId               = ("uid", "SERIAL PRIMARY KEY")
  ,  colLogin            = ("login", "text UNIQUE NOT NULL")
  ,  colEmail            = ("email", "text")
  ,  colPassword         = ("password", "text")
  ,  colActivatedAt      = ("activated_at", "timestamptz")
  ,  colSuspendedAt      = ("suspended_at", "timestamptz")
  ,  colRememberToken    = ("remember_token", "text")
  ,  colLoginCount       = ("login_count", "integer NOT NULL")
  ,  colFailedLoginCount = ("failed_login_count", "integer NOT NULL")
  ,  colLockedOutUntil   = ("locked_out_until", "timestamptz")
  ,  colCurrentLoginAt   = ("current_login_at", "timestamptz")
  ,  colLastLoginAt      = ("last_login_at", "timestamptz")
  ,  colCurrentLoginIp   = ("current_login_ip", "text")
  ,  colLastLoginIp      = ("last_login_ip", "text")
  ,  colCreatedAt        = ("created_at", "timestamptz")
  ,  colUpdatedAt        = ("updated_at", "timestamptz")
  ,  colResetToken       = ("reset_token", "text")
  ,  colResetRequestedAt = ("reset_requested_at", "timestamptz")
  ,  rolesTable          = "user_roles"
  }

fDesc :: (Text, Text) -> Text
fDesc f = fst f `T.append` " " `T.append` snd f

-- | List of deconstructors so it's easier to extract column names from an
-- 'AuthTable'.
colDef :: [(AuthTable -> (Text, Text), A.AuthUser -> P.Action)]
colDef =
  [ (colId              , P.toField . fmap A.unUid . A.userId)
  , (colLogin           , P.toField . A.userLogin)
  , (colEmail           , P.toField . A.userEmail)
  , (colPassword        , P.toField . A.userPassword)
  , (colActivatedAt     , P.toField . A.userActivatedAt)
  , (colSuspendedAt     , P.toField . A.userSuspendedAt)
  , (colRememberToken   , P.toField . A.userRememberToken)
  , (colLoginCount      , P.toField . A.userLoginCount)
  , (colFailedLoginCount, P.toField . A.userFailedLoginCount)
  , (colLockedOutUntil  , P.toField . A.userLockedOutUntil)
  , (colCurrentLoginAt  , P.toField . A.userCurrentLoginAt)
  , (colLastLoginAt     , P.toField . A.userLastLoginAt)
  , (colCurrentLoginIp  , P.toField . A.userCurrentLoginIp)
  , (colLastLoginIp     , P.toField . A.userLastLoginIp)
  , (colCreatedAt       , P.toField . A.userCreatedAt)
  , (colUpdatedAt       , P.toField . A.userUpdatedAt)
  , (colResetToken      , P.toField . A.userResetToken)
  , (colResetRequestedAt, P.toField . A.userResetRequestedAt)
  ]

saveQuery :: AuthTable -> A.AuthUser -> (Text, [P.Action])
saveQuery atable u@A.AuthUser{..} = maybe insertQuery updateQuery userId
  where
    insertQuery =  (T.concat [ "INSERT INTO "
                             , tblName atable
                             , " ("
                             , T.intercalate "," cols
                             , ") VALUES ("
                             , T.intercalate "," vals
                             , ") RETURNING "
                             , T.intercalate "," (map (fst . ($atable) . fst) colDef)
                             ]
                   , params)
    qval f  = fst (f atable) `T.append` " = ?"
    updateQuery uid =
        (T.concat [ "UPDATE "
                  , tblName atable
                  , " SET "
                  , T.intercalate "," (map (qval . fst) $ tail colDef)
                  , " WHERE "
                  , fst (colId atable)
                  , " = ? RETURNING "
                  , T.intercalate "," (map (fst . ($atable) . fst) colDef)
                  ]
        , params ++ [P.toField $ A.unUid uid])
    cols = map (fst . ($atable) . fst) $ tail colDef
    vals = map (const "?") cols
    params = map (($u) . snd) $ tail colDef


onFailure :: Monad m => E.SomeException -> m (Either A.AuthFailure a)
onFailure e = return $ Left $ A.AuthError $ show e

------------------------------------------------------------------------------
-- |
instance A.IAuthBackend PostgresAuthManager where
    save PostgresAuthManager{..} u@A.AuthUser{..} = do
        let (qstr, params) = saveQuery pamTable u
        let q = Query $ T.encodeUtf8 qstr
        let action = withConnection pamConn $ \conn -> do
                res <- P.query conn q params
                return $ Right $ fromMaybe u $ listToMaybe res
        E.catch action onFailure


    lookupByUserId PostgresAuthManager{..} uid = do
        let q = Query $ T.encodeUtf8 $ T.concat
                [ "select ", T.intercalate "," cols, " from "
                , tblName pamTable
                , " where "
                , fst (colId pamTable)
                , " = ?"
                ]
        querySingle pamConn q [A.unUid uid]
      where cols = map (fst . ($pamTable) . fst) colDef

    lookupByLogin PostgresAuthManager{..} login = do
        let q = Query $ T.encodeUtf8 $ T.concat
                [ "select ", T.intercalate "," cols, " from "
                , tblName pamTable
                , " where "
                , fst (colLogin pamTable)
                , " = ?"
                ]
        querySingle pamConn q [login]
      where cols = map (fst . ($pamTable) . fst) colDef

#if MIN_VERSION_snap(1,1,0)
    lookupByEmail PostgresAuthManager{..} email = do
        let q = Query $ T.encodeUtf8 $ T.concat
                [ "select ", T.intercalate "," cols, " from "
                , tblName pamTable
                , " where "
                , fst (colEmail pamTable)
                , " = ?"
                ]
        querySingle pamConn q [email]
      where cols = map (fst . ($pamTable) . fst) colDef
#endif

    lookupByRememberToken PostgresAuthManager{..} token = do
        let q = Query $ T.encodeUtf8 $ T.concat
                [ "select ", T.intercalate "," cols, " from "
                , tblName pamTable
                , " where "
                , fst (colRememberToken pamTable)
                , " = ?"
                ]
        querySingle pamConn q [token]
      where cols = map (fst . ($pamTable) . fst) colDef

    destroy PostgresAuthManager{..} A.AuthUser{..} = do
        let q = Query $ T.encodeUtf8 $ T.concat
                [ "delete from "
                , tblName pamTable
                , " where "
                , fst (colLogin pamTable)
                , " = ?"
                ]
        authExecute pamConn q [userLogin]

