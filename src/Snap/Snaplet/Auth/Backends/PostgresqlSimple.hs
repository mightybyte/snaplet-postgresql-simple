{-# LANGUAGE BangPatterns      #-}
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
>     , _auth :: Snaplet (AuthManager App)
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
import           Prelude
import           Control.Applicative
import           Control.Error
import qualified Control.Exception as E
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.Configurator as C
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Database.PostgreSQL.Simple as P
import qualified Database.PostgreSQL.Simple.ToField as P
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.PostgresqlSimple.Internal
import           Snap.Snaplet.Session
import           Web.ClientSession
import           Paths_snaplet_postgresql_simple
------------------------------------------------------------------------------


data PostgresAuthManager = PostgresAuthManager
    { pamTable    :: AuthTable
    , pamConn     :: Postgres
    }


------------------------------------------------------------------------------
-- | Initializer for the postgres backend to the auth snaplet.
--
initPostgresAuth
  :: SnapletLens b SessionManager  -- ^ Lens to the session snaplet
  -> Snaplet Postgres  -- ^ The postgres snaplet
  -> SnapletInit b (AuthManager b)
initPostgresAuth sess db = makeSnaplet "postgresql-auth" desc datadir $ do
    config <- getSnapletUserConfig
    authTable <- liftIO $ C.lookupDefault "snap_auth_user" config "authTable"
    authSettings <- authSettingsFromConfig
    key <- liftIO $ getKey (asSiteKey authSettings)
    let tableDesc = defAuthTable { tblName = authTable }
    let manager = PostgresAuthManager tableDesc $ db ^# snapletValue
    liftIO $ createTableIfMissing manager
    rng <- liftIO mkRNG
    return AuthManager
      { backend = manager
      , session = sess
      , activeUser = Nothing
      , minPasswdLen = asMinPasswdLen authSettings
      , rememberCookieName = asRememberCookieName authSettings
      , rememberPeriod = asRememberPeriod authSettings
      , siteKey = key
      , lockout = asLockout authSettings
      , randomNumberGenerator = rng
      }
  where
    desc = "A PostgreSQL backend for user authentication"
    datadir = Just $ liftM (++"/resources/auth") getDataDir


------------------------------------------------------------------------------
-- | Create the user table if it doesn't exist.
createTableIfMissing :: PostgresAuthManager -> IO ()
createTableIfMissing PostgresAuthManager{..} = do
    liftPG' pamConn $ \conn -> do
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
          , ")"
          ]

buildUid :: Int -> UserId
buildUid = UserId . T.pack . show


instance FromField UserId where
    fromField f v = buildUid <$> fromField f v

instance FromField Password where
    fromField f v = Encrypted <$> fromField f v

instance FromRow AuthUser where
    fromRow =
        AuthUser
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
querySingle pc q ps = liftPG' pc $ \conn -> return . listToMaybe =<<
    P.query conn q ps

authExecute :: ToRow q
            => Postgres -> Query -> q -> IO ()
authExecute pc q ps = do
    liftPG' pc $ \conn -> P.execute conn q ps
    return ()

instance P.ToField Password where
    toField (ClearText bs) = P.toField bs
    toField (Encrypted bs) = P.toField bs


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
colDef :: [(AuthTable -> (Text, Text), AuthUser -> P.Action)]
colDef =
  [ (colId              , P.toField . fmap unUid . userId)
  , (colLogin           , P.toField . userLogin)
  , (colEmail           , P.toField . userEmail)
  , (colPassword        , P.toField . userPassword)
  , (colActivatedAt     , P.toField . userActivatedAt)
  , (colSuspendedAt     , P.toField . userSuspendedAt)
  , (colRememberToken   , P.toField . userRememberToken)
  , (colLoginCount      , P.toField . userLoginCount)
  , (colFailedLoginCount, P.toField . userFailedLoginCount)
  , (colLockedOutUntil  , P.toField . userLockedOutUntil)
  , (colCurrentLoginAt  , P.toField . userCurrentLoginAt)
  , (colLastLoginAt     , P.toField . userLastLoginAt)
  , (colCurrentLoginIp  , P.toField . userCurrentLoginIp)
  , (colLastLoginIp     , P.toField . userLastLoginIp)
  , (colCreatedAt       , P.toField . userCreatedAt)
  , (colUpdatedAt       , P.toField . userUpdatedAt)
  , (colResetToken      , P.toField . userResetToken)
  , (colResetRequestedAt, P.toField . userResetRequestedAt)
  ]

saveQuery :: AuthTable -> AuthUser -> (Text, [P.Action])
saveQuery atable u@AuthUser{..} = maybe insertQuery updateQuery userId
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
        , params ++ [P.toField $ unUid uid])
    cols = map (fst . ($atable) . fst) $ tail colDef
    vals = map (const "?") cols
    params = map (($u) . snd) $ tail colDef


onFailure :: Monad m => E.SomeException -> m (Either AuthFailure a)
onFailure e = return $ Left $ AuthError $ show e

------------------------------------------------------------------------------
-- |
instance IAuthBackend PostgresAuthManager where
    save PostgresAuthManager{..} u@AuthUser{..} = do
        let (qstr, params) = saveQuery pamTable u
        let q = Query $ T.encodeUtf8 qstr
        let action = liftPG' pamConn $ \conn -> do
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
        querySingle pamConn q [unUid uid]
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

    destroy PostgresAuthManager{..} AuthUser{..} = do
        let q = Query $ T.encodeUtf8 $ T.concat
                [ "delete from "
                , tblName pamTable
                , " where "
                , fst (colLogin pamTable)
                , " = ?"
                ]
        authExecute pamConn q [userLogin]

