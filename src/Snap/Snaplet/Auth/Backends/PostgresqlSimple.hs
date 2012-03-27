{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Snap.Snaplet.Auth.Backends.PostgresqlSimple where

------------------------------------------------------------------------------
import           Control.Arrow
import qualified Data.ByteString as B
import qualified Data.Configurator as C
import qualified Data.HashMap.Lazy as HM
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
    { pamAuthTable :: String
    , pamConnPool  :: Pool P.Connection
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
    let manager = PostgresAuthManager authTable $
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
    liftIO $ print q
    withResource pamConnPool $ \conn -> P.execute_ conn (Query q)
    return ()
  where
    q = T.encodeUtf8 $ "CREATE TABLE " `T.append`
                       (T.pack pamAuthTable) `T.append`
                       " (" `T.append`
      T.intercalate ","
      ["user_id SERIAL PRIMARY KEY"
      ,"user_login text NOT NULL"
      ,"user_password text"
      ,"user_activated_at date"
      ,"user_suspended_at date"
      ,"user_remember_token text"
      ,"user_login_count integer NOT NULL"
      ,"user_failed_login_count integer NOT NULL"
      ,"user_locked_out_until date"
      ,"user_current_login_at date"
      ,"user_last_login_at date"
      ,"user_current_login_ip text"
      ,"user_last_login_ip text"
      ,"user_created_at date"
      ,"user_updated_at date)"
      ]

instance Result UserId where
    convert f v = UserId <$> convert f v

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

------------------------------------------------------------------------------
-- | 
instance IAuthBackend PostgresAuthManager where
    --save :: PostgresAuthManager -> AuthUser -> IO AuthUser
    save PostgresAuthManager{..} u@AuthUser{..} = do
        let q = "insert into ?  (userId,userLogin,userPassword,userActivatedAt,userSuspendedAt,userRememberToken,userLoginCount,userFailedLoginCount,userLockedOutUntil,userCurrentLoginAt,userLastLoginAt,userCurrentLoginIp,userLastLoginIp,userCreatedAt,userUpdatedAt) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        withResource pamConnPool $ \conn -> do
            P.begin conn
            P.execute conn q
              [P.render $ fmap unUid userId
              ,P.render userLogin
              ,P.render userPassword
              ,P.render userActivatedAt
              ,P.render userSuspendedAt
              ,P.render userRememberToken
              ,P.render userLoginCount
              ,P.render userFailedLoginCount
              ,P.render userLockedOutUntil
              ,P.render userCurrentLoginAt
              ,P.render userLastLoginAt
              ,P.render userCurrentLoginIp
              ,P.render userLastLoginIp
              ,P.render userCreatedAt
              ,P.render userUpdatedAt]
            res <- P.query conn "select * from ? where userLogin = ?"
                    (pamAuthTable, userLogin)
            P.commit conn
            return $ fromMaybe u $ listToMaybe res

    --lookupByUserId :: PostgresAuthManager -> UserId -> IO (Maybe AuthUser)
    lookupByUserId PostgresAuthManager{..} uid =
        querySingle pamConnPool "select * from ? where userId = ?"
                    (pamAuthTable, unUid uid)

    --lookupByLogin :: PostgresAuthManager -> Text -> IO (Maybe AuthUser)
    lookupByLogin PostgresAuthManager{..} login =
        querySingle pamConnPool "select * from ? where userLogin = ?"
                    (pamAuthTable, login)

    --lookupByRememberToken :: PostgresAuthManager -> Text -> IO (Maybe AuthUser)
    lookupByRememberToken PostgresAuthManager{..} token =
        querySingle pamConnPool "select * from ? where userRememberToken = ?"
                    (pamAuthTable, token)

    --destroy :: PostgresAuthManager -> AuthUser -> IO ()
    destroy PostgresAuthManager{..} AuthUser{..} =
        authExecute pamConnPool "delete from ? where userLogin = ?"
                    (pamAuthTable, userLogin)

