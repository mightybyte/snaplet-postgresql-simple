{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Snap.Snaplet.Auth.Backends.PostgresqlSimple where

------------------------------------------------------------------------------
import           Control.Arrow
import qualified Data.ByteString as B
import qualified Data.Configurator as C
import qualified Data.HashMap.Lazy as HM
import           Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Maybe
import           Data.Pool
import qualified Database.PostgreSQL.Simple as P
import qualified Database.PostgreSQL.Simple.Param as P
import           Database.PostgreSQL.Simple.Result
import           Database.PostgreSQL.Simple.QueryResults
import           Database.PostgreSQL.Simple.Types
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Common
import           Web.ClientSession


data PostgresAuthManager = PostgresAuthManager
    { pamTable    :: AuthTable
    , pamConnPool :: Pool P.Connection
    }


------------------------------------------------------------------------------
-- | Simple function to get auth settings from a config file.  All options
-- are optional and default to what's in defAuthSettings if not supplied.
settingsFromConfig :: Initializer b (AuthManager b) AuthSettings
settingsFromConfig = do
    config <- getSnapletUserConfig
    minPasswordLen <- liftIO $ C.lookup config "minPasswordLen"
    let pw = maybe id (\x s -> s { asMinPasswdLen = x }) minPasswordLen
    rememberCookie <- liftIO $ C.lookup config "rememberCookie"
    let rc = maybe id (\x s -> s { asRememberCookieName = x }) rememberCookie
    rememberPeriod <- liftIO $ C.lookup config "rememberPeriod"
    let rp = maybe id (\x s -> s { asRememberPeriod = Just x }) rememberPeriod
    lockout <- liftIO $ C.lookup config "lockout"
    let lo = maybe id (\x s -> s { asLockout = Just (second fromInteger x) }) lockout
    siteKey <- liftIO $ C.lookup config "siteKey"
    let sk = maybe id (\x s -> s { asSiteKey = x }) siteKey
    return $ (pw . rc . rp . lo . sk) defAuthSettings


------------------------------------------------------------------------------
-- | 
initPostgresAuth
  :: Lens b (Snaplet SessionManager)  -- ^ Lens to the session snaplet
  -> Snaplet Postgres  -- ^ The postgres snaplet
  -> SnapletInit b (AuthManager b)
initPostgresAuth sess db = makeSnaplet "PostgresAuth" desc Nothing $ do
    config <- getSnapletUserConfig
    authTable <- liftIO $ C.lookupDefault "snap_auth_user" config "authTable"
    authSettings <- settingsFromConfig
    key <- liftIO $ getKey (asSiteKey authSettings)
    let tableDesc = defAuthTable { tblName = authTable }
    let manager = PostgresAuthManager tableDesc $
                                      pgPool $ getL snapletValue db
    liftIO $ createTableIfMissing manager
    rng <- liftIO mkRNG
    return $ AuthManager
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


------------------------------------------------------------------------------
-- | Create the user table if it doesn't exist.
createTableIfMissing :: PostgresAuthManager -> IO ()
createTableIfMissing PostgresAuthManager{..} = do
    withResource pamConnPool $ \conn -> do
        res <- P.query_ conn $ Query $ T.encodeUtf8 $
          "select relname from pg_class where relname='"
          `T.append` (T.pack $ tblName pamTable) `T.append` "'"
        when (null (res :: [Only T.Text])) $ P.execute_ conn (Query q) >> return ()
    return ()
  where
    q = T.encodeUtf8 $ "CREATE TABLE " `T.append`
                       (T.pack (tblName pamTable)) `T.append`
                       " (" `T.append`
      T.intercalate ","
      ["id SERIAL PRIMARY KEY"
      ,"login text UNIQUE NOT NULL"
      ,"password text"
      ,"activated_at timestamp"
      ,"suspended_at timestamp"
      ,"remember_token text"
      ,"login_count integer NOT NULL"
      ,"failed_login_count integer NOT NULL"
      ,"locked_out_until timestamp"
      ,"current_login_at timestamp"
      ,"last_login_at timestamp"
      ,"current_login_ip text"
      ,"last_login_ip text"
      ,"created_at timestamp"
      ,"updated_at timestamp)"
      ]

buildUid :: Int -> UserId
buildUid = UserId . T.pack . show


instance Result UserId where
    convert f v = buildUid <$> convert f v

instance Result Password where
    convert f v = Encrypted <$> convert f v

instance QueryResults AuthUser where
    convertResults (fa:fb:fc:fd:fe:ff:fg:fh:fi:fj:fk:fl:fm:fn:fo:_)
                   (va:vb:vc:vd:ve:vf:vg:vh:vi:vj:vk:vl:vm:vn:vo:_) =
        AuthUser
        <$> _userId
        <*> _userLogin
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
        <*> _userRoles
        <*> _userMeta
      where
        !_userId               = convert fa va
        !_userLogin            = convert fb vb
        !_userPassword         = convert fc vc
        !_userActivatedAt      = convert fd vd
        !_userSuspendedAt      = convert fe ve
        !_userRememberToken    = convert ff vf
        !_userLoginCount       = convert fg vg
        !_userFailedLoginCount = convert fh vh
        !_userLockedOutUntil   = convert fi vi
        !_userCurrentLoginAt   = convert fj vj
        !_userLastLoginAt      = convert fk vk
        !_userCurrentLoginIp   = convert fl vl
        !_userLastLoginIp      = convert fm vm
        !_userCreatedAt        = convert fn vn
        !_userUpdatedAt        = convert fo vo
        !_userRoles            = Right []
        !_userMeta             = Right HM.empty
    convertResults fs vs = convertError fs vs 15


querySingle :: (QueryParams q, QueryResults a)
            => Pool P.Connection -> Query -> q -> IO (Maybe a)
querySingle pool q ps = withResource pool $ \conn -> return . listToMaybe =<<
    P.query conn q ps

authExecute :: QueryParams q
            => Pool P.Connection -> Query -> q -> IO ()
authExecute pool q ps = do
    withResource pool $ \conn -> P.execute conn q ps
    return ()

instance P.Param Password where
    render (ClearText bs) = P.render bs
    render (Encrypted bs) = P.render bs


-- | Datatype containing the names of the columns for the authentication table.
data AuthTable
  =  AuthTable
  {  tblName :: String
  ,  colId :: String
  ,  colLogin :: String
  ,  colPassword :: String
  ,  colActivatedAt :: String
  ,  colSuspendedAt :: String
  ,  colRememberToken :: String
  ,  colLoginCount :: String
  ,  colFailedLoginCount :: String
  ,  colLockedOutUntil :: String
  ,  colCurrentLoginAt :: String
  ,  colLastLoginAt :: String
  ,  colCurrentLoginIp :: String
  ,  colLastLoginIp :: String
  ,  colCreatedAt :: String
  ,  colUpdatedAt :: String
  ,  rolesTable :: String
  }

-- | Default authentication table layout
defAuthTable :: AuthTable
defAuthTable
  =  AuthTable
  {  tblName = "user"
  ,  colId = "uid"
  ,  colLogin = "login"
  ,  colPassword = "password"
  ,  colActivatedAt = "activated_at"
  ,  colSuspendedAt = "suspended_at"
  ,  colRememberToken = "remember_token"
  ,  colLoginCount = "login_count"
  ,  colFailedLoginCount = "failed_login_count"
  ,  colLockedOutUntil = "locked_out_until"
  ,  colCurrentLoginAt = "current_login_at"
  ,  colLastLoginAt = "last_login_at"
  ,  colCurrentLoginIp = "current_login_ip"
  ,  colLastLoginIp = "last_login_ip"
  ,  colCreatedAt = "created_at"
  ,  colUpdatedAt = "updated_at"
  ,  rolesTable = "user_roles"
  }

-- | List of deconstructors so it's easier to extract column names from an
-- 'AuthTable'.
colDef :: [(AuthTable -> String, AuthUser -> P.Action)]
colDef =
  [ (colLogin           , P.render . userLogin)
  , (colPassword        , P.render . userPassword)
  , (colActivatedAt     , P.render . userActivatedAt)
  , (colSuspendedAt     , P.render . userSuspendedAt)
  , (colRememberToken   , P.render . userRememberToken)
  , (colLoginCount      , P.render . userLoginCount)
  , (colFailedLoginCount, P.render . userFailedLoginCount)
  , (colLockedOutUntil  , P.render . userLockedOutUntil)
  , (colCurrentLoginAt  , P.render . userCurrentLoginAt)
  , (colLastLoginAt     , P.render . userLastLoginAt)
  , (colCurrentLoginIp  , P.render . userCurrentLoginIp)
  , (colLastLoginIp     , P.render . userLastLoginIp)
  , (colCreatedAt       , P.render . userCreatedAt)
  , (colUpdatedAt       , P.render . userUpdatedAt)
  ]

saveQuery :: AuthTable -> AuthUser -> ([Char], [P.Action])
saveQuery at u@AuthUser{..} = maybe insertQuery updateQuery userId
  where
    insertQuery =  ("INSERT INTO " ++ tblName at ++ " (" ++
                   intercalate "," cols
                   ++ ") VALUES (" ++
                   intercalate "," vals
                   ++ ")", params)
    qval f  = f at ++ " = ?"
    updateQuery uid =  ("UPDATE " ++ tblName at ++ " SET " ++
                       intercalate "," (map (qval . fst) colDef)
                       ++ " WHERE " ++ colId at ++ " = ?"
                       , params ++ [P.render $ unUid uid])
    cols = map (($at) . fst) colDef
    vals = map (const "?") cols
    params = map (($u) . snd) colDef
            

------------------------------------------------------------------------------
-- | 
instance IAuthBackend PostgresAuthManager where
    --save :: PostgresAuthManager -> AuthUser -> IO AuthUser
    save PostgresAuthManager{..} u@AuthUser{..} = do
        let (qstr, params) = saveQuery pamTable u
        let q = Query $ T.encodeUtf8 $ T.pack qstr
        withResource pamConnPool $ \conn -> do
            P.begin conn
            P.execute conn q params
            res <- P.query conn (Query $ T.encodeUtf8 $ T.pack $ "select * from " ++ tblName pamTable ++ " where login = ?")
                    [userLogin]
            P.commit conn
            return $ fromMaybe u $ listToMaybe res

    --lookupByUserId :: PostgresAuthManager -> UserId -> IO (Maybe AuthUser)
    lookupByUserId PostgresAuthManager{..} uid =
        querySingle pamConnPool (Query $ T.encodeUtf8 $ T.pack $ "select * from " ++ tblName pamTable ++ " where id = ?")
                    [unUid uid]

    --lookupByLogin :: PostgresAuthManager -> Text -> IO (Maybe AuthUser)
    lookupByLogin PostgresAuthManager{..} login =
        querySingle pamConnPool (Query $ T.encodeUtf8 $ T.pack $ "select * from " ++ tblName pamTable ++ " where login = ?")
                    [login]

    --lookupByRememberToken :: PostgresAuthManager -> Text -> IO (Maybe AuthUser)
    lookupByRememberToken PostgresAuthManager{..} token =
        querySingle pamConnPool (Query $ T.encodeUtf8 $ T.pack $ "select * from " ++ tblName pamTable ++ " where remember_token = ?")
                    [token]

    --destroy :: PostgresAuthManager -> AuthUser -> IO ()
    destroy PostgresAuthManager{..} AuthUser{..} =
        authExecute pamConnPool (Query $ T.encodeUtf8 $ T.pack $ "delete from " ++ tblName pamTable ++ " where login = ?")
                    [userLogin]

