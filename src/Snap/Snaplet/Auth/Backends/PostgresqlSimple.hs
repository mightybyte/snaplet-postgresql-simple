{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Snap.Snaplet.Auth.Backends.PostgresqlSimple where

------------------------------------------------------------------------------
import           Control.Arrow
--import           Database.PostgreSQL.Simple
import qualified Data.ByteString as B
import qualified Data.Configurator as C
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe
import           Data.Pool
import qualified Database.PostgreSQL.Simple as P
import qualified Database.PostgreSQL.Simple.Param as P
import           Database.PostgreSQL.Simple.Result
import           Database.PostgreSQL.Simple.QueryResults
import           Database.PostgreSQL.Simple.Types
import           Snap
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session
import           Web.ClientSession

data PostgresAuthManager = PostgresAuthManager
    { pamAuthTable :: String
    , pamConnPool  :: Pool P.Connection
    }


------------------------------------------------------------------------------
-- | Simple function to get auth settings from a config file.  All options
-- are optional and default to what's in defAuthSettings if not supplied.
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
--initPostgresAuth
--  :: Lens b (Snaplet SessionManager)  -- ^ Lens to the session snaplet
--  -> Snaplet Postgres  -- ^ The postgres snaplet
--  -> SnapletInit b (AuthManager b)
initPostgresAuth sess db = makeSnaplet "PostgresAuth" desc Nothing $ do
    config <- getSnapletUserConfig
    authTable <- liftIO $ C.lookupDefault "snap_auth_user" config "authTable"
    authSettings <- settingsFromConfig
    key <- liftIO $ getKey (asSiteKey authSettings)
    let manager = PostgresAuthManager authTable $ pgPool $ getL snapletValue db
    liftIO $ createTableIfMissing manager
    return $ AuthManager
      { backend = manager
      , session = sess
      , activeUser = Nothing
      , minPasswdLen = asMinPasswdLen authSettings
      , rememberCookieName = asRememberCookieName authSettings
      , rememberPeriod = asRememberPeriod authSettings
      , siteKey = key
      , lockout = asLockout authSettings }
  where
    desc = "A PostgreSQL backend for user authentication"

createTableIfMissing :: PostgresAuthManager -> IO ()
createTableIfMissing PostgresAuthManager{..} = do
    withResource pamConnPool $ \conn -> P.execute conn q
      (Only pamAuthTable)
    return ()
  where
    q = Query $ "CREATE TABLE IF NOT EXISTS ? (" `B.append`
      B.intercalate ","
      ["userId text PRIMARY KEY"
      ,"userLogin text NOT NULL"
      ,"userPassword text"
      ,"userActivatedAt date"
      ,"userSuspendedAt date"
      ,"userRememberToken text"
      ,"userLoginCount integer NOT NULL"
      ,"userFailedLoginCount integer NOT NULL"
      ,"userLockedOutUntil date"
      ,"userCurrentLoginAt date"
      ,"userLastLoginAt date"
      ,"userCurrentLoginIp text"
      ,"userLastLoginIp text"
      ,"userCreatedAt date"
      ,"userUpdatedAt date"
      ,"userRoles text)"
      ]

instance Result UserId where
    convert f v = UserId <$> convert f v

instance Result Password where
    convert f v = Encrypted <$> convert f v

instance QueryResults AuthUser where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi,fj,fk,fl,fm,fn,fo,fp,fq]
                   [va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk,vl,vm,vn,vo,vp,vq] =
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


querySingle pool q ps = withResource pool $ \conn -> return . listToMaybe =<<
    P.query conn q ps

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
        let query = "insert into ?  (userId,userLogin,userPassword,userActivatedAt,userSuspendedAt,userRememberToken,userLoginCount,userFailedLoginCount,userLockedOutUntil,userCurrentLoginAt,userLastLoginAt,userCurrentLoginIp,userLastLoginIp,userCreatedAt,userUpdatedAt) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        withResource pamConnPool $ \conn -> do
            P.begin conn
            P.execute conn query
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

