module Foundation
    ( App (..)
    , Route (..)
    , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
	, isTrustedUser
    , Form
	, globalLayout
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    , getExtra
	, widgetToHtmlUrlI
	, hamletToHtmlUrlI
	, SheetLayout (..)
--	, DominikSub (..)
--    , resourcesDominikSub
    ) where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist.Store
import Settings.StaticFiles
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile, ihamletFile, HtmlUrlI18n)
import Generated (TatoebaLanguage (..), BeamerSlidePrivate (..), BeamerSlidePublic (..) )

import qualified Data.Text as Text
import Data.Text (Text)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.

data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
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

type Form x = Html -> MForm App App (FormResult x, Widget)

isUser :: YesodAuth m => GHandler s m AuthResult
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
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120
	
    --defaultLayout = globalLayout' "/dev/work"
    defaultLayout = globalLayout . (SheetLayout "/dev/work" Nothing Nothing)

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

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

isTrustedUser :: GHandler App App AuthResult
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

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing Nothing

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId, authGoogleEmail]

    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

instance YesodBreadcrumbs App where
	breadcrumb HomeR = return("Start", Nothing)

	breadcrumb DKHomeR = return("Kurshomepage", Just HomeR)
	breadcrumb DKProjectHomeR = return("Projekthomepage", Just HomeR)
	breadcrumb QtDescR = return("Qt Kurs", Just DKHomeR)
	breadcrumb QtDossierR = return("Qt Unterlagen", Just QtDescR)
	breadcrumb QtProjectListR = return("Qt Projekte", Just QtDescR)
	breadcrumb QtGalleryR = return("Qt Galerie", Just QtDescR)
	breadcrumb QtOpenGLR = return("Qt OpenGL", Just QtDescR)
	breadcrumb OpenGLR = return("OpenGL", Just DKHomeR)
	breadcrumb JavaDescR = return("Java Kurs", Just DKHomeR)
	breadcrumb JavaDossierR = return("Java Unterlagen", Just JavaDescR)
	breadcrumb JavaProjectListR = return("Java Projekte", Just JavaDescR)
	breadcrumb (JavaProjectR _) = return("", Just JavaProjectListR) 

	breadcrumb ProjectAnnualR = return("Annual", Just DKProjectHomeR)
	breadcrumb ProjectFritzContactR = return("FritzContact", Just DKProjectHomeR)
	breadcrumb TatoebaAppR = return("Tatoeba App", Just DKProjectHomeR)
	breadcrumb TatoebaWebServiceR = return("Tatoeba Webservice", Just DKProjectHomeR)
	breadcrumb BeamerSlidesR = return("Slides", Just DKProjectHomeR)
	
	breadcrumb VocabtrainBooksR = return("Vocabtrain", Just HomeR)
	breadcrumb (VocabtrainBookUpdateR bookId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbBookUpdate . fromRightText . fromPersistValue . unKey $ bookId, Just VocabtrainBooksR)
	breadcrumb (VocabtrainBookDeleteR bookId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbBookDelete . fromRightText . fromPersistValue . unKey $ bookId, Just VocabtrainBooksR)

	breadcrumb (VocabtrainChapterR chapterId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbChapter . fromRightText . fromPersistValue . unKey $ chapterId, Just VocabtrainBooksR)
	breadcrumb (VocabtrainChapterInsertR bookId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbChapterInsert . fromRightText . fromPersistValue . unKey $ bookId, Just VocabtrainBooksR)
	breadcrumb (VocabtrainChapterUpdateR chapterId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbChapterUpdate . fromRightText . fromPersistValue . unKey $ chapterId, Just (VocabtrainChapterR chapterId))
	breadcrumb (VocabtrainChapterDeleteR chapterId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbChapterDelete . fromRightText . fromPersistValue . unKey $ chapterId, Just (VocabtrainChapterR chapterId))

	breadcrumb (VocabtrainTranslationInsertR cardId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbTranslationInsert . fromRightText . fromPersistValue . unKey $ cardId, Just (VocabtrainCardR cardId))
	breadcrumb (VocabtrainTranslationUpdateR translationId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbTranslationUpdate . fromRightText . fromPersistValue . unKey $ translationId, Just VocabtrainBooksR)
	breadcrumb (VocabtrainTranslationDeleteR translationId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbTranslationDelete . fromRightText . fromPersistValue . unKey $ translationId, Just VocabtrainBooksR)

	breadcrumb (VocabtrainCardChapterAddR chapterId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbCardChapterAdd . fromRightText . fromPersistValue . unKey $ chapterId, Just (VocabtrainChapterR chapterId))
	breadcrumb (VocabtrainCardChapterInsertR cardId chapterId) = getMessageRender >>= \msg -> return(msg $ MsgBreadcrumbCardChapterInsert (fromRightText . fromPersistValue . unKey $ cardId) (fromRightText . fromPersistValue . unKey $ chapterId), Just (VocabtrainChapterR chapterId))
	breadcrumb (VocabtrainCardChaptersDeleteR cardId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbCardChaptersDelete . fromRightText . fromPersistValue . unKey $ cardId, Just (VocabtrainCardR cardId))

	breadcrumb (VocabtrainCardR cardId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbCard . fromRightText . fromPersistValue . unKey $ cardId, Just VocabtrainBooksR)
	breadcrumb (VocabtrainCardInsertR chapterId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbCardInsert . fromRightText . fromPersistValue . unKey $ chapterId, Just (VocabtrainChapterR chapterId))
	breadcrumb (VocabtrainCardUpdateR cardId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbCardUpdate . fromRightText . fromPersistValue . unKey $ cardId, Just (VocabtrainCardR cardId))
	breadcrumb (VocabtrainCardDeleteR cardId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbCardDelete . fromRightText . fromPersistValue . unKey $ cardId, Just (VocabtrainCardR cardId))


	breadcrumb _ = return("", Nothing)

fromRightText :: Either a Text -> Text
fromRightText (Right c) = c
fromRightText _ = ""

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

widgetToHtmlUrlI :: GWidget App App () -> msg -> url -> GWidget App App ()
widgetToHtmlUrlI hu _msgRender _urlRender = hu
hamletToHtmlUrlI :: (t2 -> t1) -> t -> t2 -> t1
hamletToHtmlUrlI hu _msgRender _urlRender = hu _urlRender

data SheetLayout sub url = SheetLayout {
	  sheetTitle :: Text
	, sheetNav :: Maybe (HtmlUrlI18n AppMessage url)
	, sheetBanner :: Maybe (HtmlUrlI18n AppMessage url)
	, sheetContent :: GWidget sub App ()
}

globalLayout' :: Text -> GWidget sub App () -> GHandler sub App RepHtml
globalLayout' title widget = globalLayout $ SheetLayout title Nothing Nothing widget

globalLayout :: SheetLayout sub (Route App) -> GHandler sub App RepHtml
--globalLayout subtitle contents = do
globalLayout sheet = do
	master <- getYesod
	mmsg <- getMessage
	pc <- widgetToPageContent $ sheetContent sheet
	maid <- maybeAuthId
	(page_title, page_parents) <- breadcrumbs

	PageContent title headTags bodyTags <- widgetToPageContent $ do
		toWidgetHead $(hamletFile "templates/skeleton/head.hamlet")
		setTitle . toHtml $ sheetTitle sheet
		addStylesheet $ StaticR css_bootstrap_css
		addStylesheet $ StaticR css_bootstrap_docs_css
		addStylesheet $ StaticR css_normalize_css
		addStylesheet $ StaticR css_main_css
	ihamletToRepHtml $(ihamletFile "templates/skeleton/overall.hamlet")

