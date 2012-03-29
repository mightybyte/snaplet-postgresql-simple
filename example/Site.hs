{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.Lens.Template
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import qualified Database.PostgreSQL.Simple as P
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)


------------------------------------------------------------------------------
data App = App
    { _sess :: Snaplet SessionManager
    , _db :: Snaplet Postgres
    , _auth :: Snaplet (AuthManager App)
    }

makeLens ''App

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",            writeText "hello")
         , ("foo", fooHandler)
         , ("add/:uname", addHandler)
         ]

instance IAuthBackend (AuthManager b) where
    save AuthManager{..} u = save backend u
    lookupByUserId AuthManager{..} u = lookupByUserId backend u
    lookupByLogin AuthManager{..} u = lookupByLogin backend u
    lookupByRememberToken AuthManager{..} u = lookupByRememberToken backend u
    destroy AuthManager{..} u = destroy backend u

instance HasPostgres (Handler App Postgres) where
    getPostgresState = get

fooHandler = do
    results <- with db $ query_ "select * from snap_auth_user"
    liftIO $ print (results :: [AuthUser])

addHandler = do
    mname <- getParam "uname"
    let name = maybe "guest" T.decodeUtf8 mname
    u <- with auth $ createUser name ""
    liftIO $ print u

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    s <- nestSnaplet "" sess $
         initCookieSessionManager "site_key.txt" "_cookie" Nothing
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    addRoutes routes
    return $ App s d a


main :: IO ()
main = serveSnaplet defaultConfig app

