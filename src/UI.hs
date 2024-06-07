{-
Jasso is Copyright (c) 2021-2024 Homebrew Holdings Pty Ltd.
Contributors retain the copyright for their contributions.

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along
with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE OverloadedStrings, TypeOperators, DataKinds, MultiParamTypeClasses, DeriveGeneric #-}

module UI(UIAPI, uiapi, uiServer) where

import Config
import UITypes
import JassoState
import UICommon
import Session
import Logger
import Otp
import SinkCommon

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Aeson (ToJSON(..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Data.Text (Text, intercalate, isPrefixOf, pack, unpack)
import qualified Data.Text as T (null)
import GHC.Generics (Generic)
import Lucid
import Servant
import Servant.Multipart
import System.Random (randomRIO)

data Login = LoginGood | LoginBad deriving (Show, Eq, Read, Generic)
instance ToJSON Login
data LoginRequest  = LoginRequest {lruid :: Maybe Text, lrpasswd :: Maybe Text} deriving (Eq, Show)

instance FromMultipart Mem LoginRequest where
  fromMultipart multipartData = Right $ LoginRequest (eitherToMaybe $ lookupInput "uid" multipartData) (eitherToMaybe $ lookupInput "passwd" multipartData)

type UIAPI  = Header "Accept-Language" String :> Header "cookie" Text :> UIAPI'
type UIAPI' =           Get '[HTML] UISPA
  :<|> "login"       :> MultipartForm Mem LoginRequest :> Header "referer" Text :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] Login)
  :<|> "ui.js"       :> Get '[OctetStream] ByteString
  :<|> "favicon.ico" :> Get '[OctetStream] ByteString
  :<|> "robots.txt"  :> Get '[OctetStream] ByteString
  :<|> UIAPI''

uiapi :: Proxy UIAPI
uiapi = Proxy

-- FIXME: this should initiate a login screen
toLogin :: JassoT Handler r
toLogin = throwError err302 { errHeaders = [("Location", "/")] }

sessionGuard_ :: JassoSource s => s -> Maybe Text -> JassoT Handler a -> JassoT Handler a -> JassoT Handler a
sessionGuard_ src cookies e f = sessionGuard' src cookies toLogin $ \(Session u _ _ _) -> do
  if allowAccess u (Just ["staff"]) then f else e

sessionGuard' :: JassoSource s => s -> Maybe Text -> JassoT Handler r -> (Session -> JassoT Handler r) -> JassoT Handler r
sessionGuard' src cookies e x = do
  st   <- get
  sess <- checkSessionM src cookies
  case sess of
    Nothing    -> e
    Just sess' -> do
      put $ st { session = sess }
      x sess'

sessionGuard :: JassoSource s => s -> Maybe Text -> JassoT Handler r -> r -> JassoT Handler r
sessionGuard src cookies e x = sessionGuard' src cookies e (return . const x)

jassoToHandler :: JassoState -> JassoT Handler a -> Handler a
jassoToHandler = flip evalStateT

uiServer :: JassoSource src => src -> JassoIO (ServerT UIAPI Handler)
uiServer src = (\conf -> hoistServer uiapi (jassoToHandler conf) $ uiServer' src (jassoConfig conf)) <$> get

uiServer' :: JassoSource src => src -> JassoConfig -> ServerT UIAPI (JassoT Handler)
uiServer' src conf lang' cookies = let lang = bestLang lang' in
       sessionGuard src cookies (return $ NotAuthenticated lang $ siteName conf) (UISPA lang $ siteName conf)
  :<|>  getLogin src
  :<|> (sessionGuard src cookies toLogin =<< uijs)
  :<|>  favico
  :<|>  robots
  :<|>  getUsers' cookies src
  :<|>  sessionGuard' src cookies toLogin (return . suser)
  :<|>  sessionGuard' src cookies toLogin (getUserMFA' (siteName conf) . suser)
  :<|>  getUser' cookies src
  :<|>  getGroups' cookies src
  :<|>  getGroup' cookies src
  :<|>  throwError err302 { errHeaders = [("Location", "/"), ("Set-Cookie", "piglet-jasso-session=; Path=/; Max-Age=-1; Secure; HttpOnly")]}

logUI :: MonadIO m => LogLevel -> Text -> JassoT m ()
logUI v s = do
  sess <- session <$> get
  let user = maybe "anonymous" (uid . suser) sess
  logIt v $ "UI: " <> user <> ": " <> s

getLogin :: JassoSource src => src -> LoginRequest -> Maybe Text -> JassoT Handler (Headers '[Header "Set-Cookie" Text] Login)
getLogin src (LoginRequest uid' passwd') referer' = do
  conf    <- getConf
  let v = verbosity conf
  (symkey, _)  <- getSymKey
  uid''   <- lift $ jsonerror' v "bad uid" uid'
  passwd  <- lift $ jsonerror' v "bad uid" passwd'
  referer <- lift $ jsonerror' v "bad uid" referer'
  if ("https://" <> domain conf) `isPrefixOf` referer then return () else lift $ throw401 "UI" v "Bad referer" (jsonerror "bad uid")

  userM <- verifyUser src uid'' passwd
  case userM of
        Just (user, token) -> do
          liftIO $ logItIO v Debug $ "UI: Login successful for " <> uid user
          lift $ mkSession symkey (assertSecs conf) (sessionSecs conf) token $ \_ _ -> return LoginGood
        _                  -> do
          delayms <- randomRIO (5000000, 9000000) -- TODO: record failed login attempts by username and IP address for rate-limiting purposes
          liftIO $ logItIO v Debug $ "UI: Wrong password for " <> unpack uid'' <> ", delaying " <> show delayms <> "ms"
          liftIO $ threadDelay delayms
          return $ noHeader LoginBad
  where
    maybeThrow401' = maybeThrow401 "UI"
    jsonerror s = "{\"tag\": \"LoginBad\", \"contents\": \"" <> s <> "\"}"
    jsonerror' v = maybeThrow401' v . jsonerror

getUser' :: JassoSource src => Maybe Text -> src -> [Text] -> JassoT Handler User
getUser' cookies src u = do
  sessionGuard_ src cookies (throwError err404) $ do
    let u' = intercalate "/" u
    logUI Detail $ "getUser " <> u'
    usr <- getUser src u'
    case usr of
      Just usr' -> return usr'
      Nothing   -> lift $ throwError err404

getUserMFA' :: Text -> User -> JassoT Handler MFAStatus
getUserMFA' sn u = do
  let u' = uid u
  logUI Detail $ "getUserMFA " <> u'
  (_, s) <- getSymKey
  let (_, _, img) = getTotpKey s (if sn == "" then Nothing else Just sn) u'
  return $ MFAStatus True True $ BSL.toString img

getGroup' :: JassoSource src => Maybe Text -> src -> [Text] -> JassoT Handler Group
getGroup' cookies src g = do
  sessionGuard_ src cookies (throwError err404) $ do
    let g' = intercalate "/" g
    logUI Detail $ "getGroup " <> g'
    grp <- getGroup src g'
    case grp of
      Just grp' -> return grp'
      Nothing   -> lift $ throwError err404

getGroups' :: JassoSource src => Maybe Text -> src -> JassoT Handler [Group]
getGroups' cookies src = do
  sessionGuard_ src cookies (throwError err404) $ do
    logUI Detail "getGroups"
    getGroups src

getUsers' :: JassoSource src => Maybe Text -> src -> JassoT Handler [User]
getUsers' cookies src = do
  sessionGuard_ src cookies (throwError err404) $ do
    logUI Detail "getUsers"
    users' <- getUsers src
    case users' of
      [] -> lift $ throwError err404
      _  -> return users'

loginforms :: Monad m => String -> HtmlT m ()
loginforms lang = do
  form_ [name_ "login", id_ "login", onsubmit_ "dologin(); return false;"] $ do
    input_ [type_ "text",     name_ "uid",    id_ "uid",    placeholder_ $ tr lang "Username or Email Address", autofocus_]
    input_ [type_ "password", name_ "passwd", id_ "passwd", placeholder_ $ tr lang "Password"]
    div_ [style_ "flex-flow: row wrap; margin: 0;"] $ a_ [id_ "forgot", onclick_ "doforgot(); return false;"] "Forgot password?"
    div_ [style_ "flex-flow: row-reverse wrap;"] $ input_ [type_ "submit", name_ "submit", value_ "Login"]

loginscript :: String
loginscript = "\
\const dologin = async() => {\n\
\  els = document.login.elements;\n\
\  msg = document.getElementById('msgbox');\n\
\  msg.innerText = 'Logging in';\n\
\  if (!msg.open) msg.showModal();\n\
\  els['submit'].disabled = true;\n\
\  els['submit'].style.cursor = 'wait';\n\
\  els['uid'].readOnly = true;\n\
\  els['passwd'].readOnly = true;\n\
\  els['uid'].style.cursor = 'wait';\n\
\  els['passwd'].style.cursor = 'wait';\n\
\  document.body.style.cursor = 'wait';\n\
\  const response = await fetch('login', {method:'POST', body: new FormData(document.login)});\n\
\  const myJson = await response.json();\n\
\  if (myJson === 'LoginGood') {\n\
\    location.reload(true);\n\
\  } else {\n\
\    msg.innerText = 'Login failed, please try again';\n\
\    if (!msg.open) msg.showModal();\n\
\    setTimeout(() => { msg.close(); }, 2000);\n\
\    document.body.style.cursor = '';\n\
\    els['submit'].disabled = false;\n\
\    els['submit'].style.cursor = '';\n\
\    els['uid'].style.cursor = '';\n\
\    els['uid'].readOnly = false;\n\
\    els['passwd'].style.cursor = '';\n\
\    els['passwd'].value = '';\n\
\    els['passwd'].readOnly = false;\n\
\    els['passwd'].focus();\n\
\  }\n\
\}\n\
\const doforgot = async() => {\n\
\}\n"

instance ToHtml UISPA where
  toHtml (NotAuthenticated lang sitename) = doctypehtml_ $ do
    head__ True $ do
      meta_ [charset_ "utf-8"]
      title_ $ toHtmlRaw $ "Sign in" <> if T.null sitename then "" else " - " <> sitename
      script_ [defer_ "true"] loginscript
    main__ (do
      loginforms lang
      term "dialog" [id_ "msgbox"] mempty
      logo True) mempty
  toHtml (UISPA lang sitename) = doctypehtml_ $ do
    head__ False $ do
      title_ $ toHtmlRaw $ (if T.null sitename then sitename <> " " else "") <> "Jasso Single Sign-On from Piglet Engineering"
      link_ [href_ "https://fonts.googleapis.com/css?family=Roboto:300,400,500|Material+Icons", rel_ "stylesheet"]
      link_ [rel_ "stylesheet", href_ "https://unpkg.com/material-components-web-elm@9.0.0/dist/material-components-web-elm.min.css"]
      script_ [src_ "https://unpkg.com/material-components-web-elm@9.0.0/dist/material-components-web-elm.min.js"] ("" :: ByteString)
      style_ ".mdc-top-app-bar { position: initial !important; } \
             \.mdc-card { margin: 2em auto !important; width: calc(100dvw - 2em); max-width: 50em; } \
             \.mdc-card__actions { flex-direction: row-reverse; } \
             \.mdc-card__action-buttons { flex-wrap: wrap; } \
             \.mdc-deprecated-list-group { max-width: 40em !important; } \
             \.mdc-deprecated-list-item { height: auto !important; padding: 1em 0; } \
             \.mdc-deprecated-list-item__graphic { color: rgba(0, 0, 0, 0.87); color: var(--mdc-theme-text-primary-on-background, rgba(0, 0, 0, 0.87)); } \
             \.mdc-deprecated-list-item__primary-text,.mdc-deprecated-list-item__secondary-text { white-space: pre-wrap; margin: 0.2em 0; }"
    main__ (do
      term "dialog" [id_ "msgbox", open_ "open"] $ p_ [id_ "msg"] $ toHtml ("Loading" :: String)
      logo False
      ) (do
      script_ [src_ "ui.js"] ("" :: ByteString)
      script_ $ "var app = Elm.UI.init({node: document.querySelector('main'), flags: '" <> pack lang <> "'});")
  toHtmlRaw = toHtml
