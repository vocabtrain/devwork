module Foundation
    ( App (..)
    , Route (..)
    , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
	, isTrustedUser
	, isAdmin
    , Form
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    , getExtra
	, widgetToHtmlUrlI
	, hamletToHtmlUrlI
	, appExtra
--	, DominikSub (..)
--    , resourcesDominikSub
    ) where

import Prelude
import Data.Time
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Yesod.Auth.Dummy
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
--import Settings.StaticFiles
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
-- import Web.ClientSession (getKey)
import Generated (TatoebaLanguage (..), BeamerSlidePrivate (..), BeamerSlidePublic (..) )
import Yesod.Core.Types (Logger)


import qualified Data.Text as Text
import Data.Text (Text)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.

data App = App
	{ settings :: AppConfig DefaultEnv Extra
	, getStatic :: Static -- ^ Settings for static file serving.
	, connPool :: Database.Persist.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
	, httpManager :: Manager
	, persistConfig :: Settings.PersistConfig
	, appLogger :: Logger
--	, getDominikSub :: DominikSub
	}

--data DominikSub = DominikSub App
--mkYesodSubData "DominikSub" [] $(parseRoutesFile "config/routes.dominik")

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

--type Form x = Html -> MForm App App (FormResult x, Widget)
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)


isUser :: YesodAuth s => HandlerT s IO AuthResult
isUser = do
	mu <- maybeAuthId
	return $ case mu of
		Nothing -> AuthenticationRequired
		Just _ -> Authorized

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "config/client_session_key.aes"

--   makeSessionBackend _ = do
--       key <- getKey "config/client_session_key.aes"
--       let timeout = 120 * 60 -- 120 minutes
--       (getCachedDate, _closeDateCache) <- clientSessionDateCacher timeout
--       return . Just $ clientSessionBackend2 key getCachedDate
--        return . Just $ clientSessionBackend key timeout
	
    --defaultLayout = globalLayout' "/dev/work"
    --defaultLayout = globalLayout . (SheetLayout "/dev/work" Nothing Nothing)

{-    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")
-}
    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR
	
    -- route name, then a boolean indicating if it's a write request
    isAuthorized VocabtrainMobileBooksR _ = return Authorized
    isAuthorized VocabtrainMobileDownloadR _ = return Authorized
    isAuthorized VocabtrainMobileDeltaR _ = return Authorized
    isAuthorized VocabtrainMobileFilingUploadR _ = return Authorized
    isAuthorized VocabtrainCardSearchR _ = return Authorized
    isAuthorized VocabtrainMobileFilingDownloadR _ = return Authorized
    isAuthorized _ True = isUser
    isAuthorized _ False = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
-- instance YesodPersist App where
-- 	type YesodPersistBackend App = SqlPersistT
-- 	runDB f = do
-- 		master <- getYesod
-- 		Database.Persist.runPool
-- 			(persistConfig master)
-- 			f
-- 			(connPool master)
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool
isAdmin :: HandlerT App IO AuthResult
isAdmin = do
	mu <- maybeAuth
	msgShow <- getMessageRender
	return $ case mu of
		Nothing -> AuthenticationRequired
		Just (Entity _ u) -> hasTrustedMail . userEmail $ u
			where
				hasTrustedMail :: Text -> AuthResult
				hasTrustedMail "transitiv@googlemail.com" = Authorized
				hasTrustedMail _ = Unauthorized $ msgShow MsgNotTrustedUser



isTrustedUser :: HandlerT App IO AuthResult
isTrustedUser = do
	mu <- maybeAuth
	msgShow <- getMessageRender
	return $ case mu of
		Nothing -> AuthenticationRequired
		Just (Entity _ u) -> hasTrustedMail . userEmail $ u
			where
				hasTrustedMail :: Text -> AuthResult
				hasTrustedMail "transitiv@googlemail.com" = Authorized
				hasTrustedMail _ = Unauthorized $ msgShow MsgNotTrustedUser
--		_ -> Unauthorized $ msgShow MsgNotTrustedUser
	
instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR


-- TODO: These are dummy settings!!
    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser "transitiv@googlemail.com"
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing Nothing Nothing
    maybeAuthId = runDB $ do
        x <- getBy $ UniqueUser "transitiv@googlemail.com"
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User "transitiv@googlemail.com" Nothing Nothing Nothing
    authPlugins _ = [authDummy]
-- END TODO

    -- getAuthId creds = runDB $ do
    --     x <- getBy $ UniqueUser $ credsIdent creds
    --     case x of
    --         Just (Entity uid _) -> return $ Just uid
    --         Nothing -> do
    --             fmap Just $ insert $ User (credsIdent creds) Nothing Nothing Nothing
    -- You can add other plugins like BrowserID, email or OAuth here
    -- authPlugins _ = [authBrowserId def, authGoogleEmail, authDummy]


    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod


-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

--staticServer :: Text -> Text
--staticServer str = Data.Text.concat [ "http://web403.webbox555.server-home.org/drake/", str ]

{--
data SheetLayout sub url = SheetLayout {
	  sheetTitle :: String
	, sheetNav :: Maybe (HtmlUrl url)
	, sheetBanner :: Maybe (HtmlUrl url)
	, sheetContent :: GWidget sub App ()
}
--}

widgetToHtmlUrlI :: WidgetT App IO () -> msg -> url -> WidgetT App IO ()
widgetToHtmlUrlI hu _msgRender _urlRender = hu
hamletToHtmlUrlI :: (t2 -> t1) -> t -> t2 -> t1
hamletToHtmlUrlI hu _msgRender _urlRender = hu _urlRender

