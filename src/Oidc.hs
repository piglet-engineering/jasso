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

{-# LANGUAGE DataKinds, TypeOperators, MultiParamTypeClasses, OverloadedStrings, DeriveGeneric, TypeApplications, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Oidc(OidcAPI, oidcSink) where

import Certs
import Config
import UITypes
import JassoState
import SinkCommon
import UICommon
import Logger
import Session
import qualified OidcTypes as OT

import Control.Concurrent
import Control.Exception (catch)
import Control.Lens
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Crypto.Cipher.Types
import Crypto.JOSE
import Crypto.JWT
import Crypto.PubKey.RSA.Types (toPrivateKey)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AT
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BU
import Data.Char
import Data.List (isSuffixOf, nub, sort)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as DM
import Data.Maybe
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T (drop, dropWhileEnd, intercalate, isPrefixOf, map, null, splitOn, words)
import qualified Data.Text.Encoding as TE
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Strict.Lens (utf8)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Vector (fromList, singleton, toList)
import Data.Yaml hiding (encode)
import Data.Yaml.Config
import GHC.Generics (Generic)
import GHC.IO.Exception (IOException(..))
import Lucid hiding (svg_)
import qualified Network.URI as N
import Servant hiding (URI)
import Servant.Multipart
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Random.Stateful
import Text.Read (readMaybe)

data Discovery = Discovery
  { issuer :: N.URI
  , authorization_endpoint :: N.URI
  , token_endpoint :: Maybe N.URI
  , userinfo_endpoint :: Maybe N.URI
  , jwks_uri :: N.URI
  , registration_endpoint :: Maybe N.URI
  , scopes_supported :: Maybe (NonEmpty Text)
  , response_types_supported :: NonEmpty Text
  , response_modes_supported :: Maybe (NonEmpty Text)
  , grant_types_supported :: Maybe (NonEmpty Text)
  , acr_values_supported :: Maybe (NonEmpty Text)
  , subject_types_supported :: NonEmpty Text
  , id_token_signing_alg_values_supported :: NonEmpty Text
  , id_token_encryption_alg_values_supported :: Maybe (NonEmpty Text)
  , id_token_encryption_enc_values_supported :: Maybe (NonEmpty Text)
  , userinfo_signing_alg_values_supported :: Maybe (NonEmpty Text)
  , userinfo_encryption_alg_values_supported :: Maybe (NonEmpty Text)
  , userinfo_encryption_enc_values_supported :: Maybe (NonEmpty Text)
  , request_object_signing_alg_values_supported :: Maybe (NonEmpty Text)
  , request_object_encryption_alg_values_supported :: Maybe (NonEmpty Text)
  , request_object_encryption_enc_values_supported :: Maybe (NonEmpty Text)
  , token_endpoint_auth_methods_supported :: Maybe (NonEmpty Text)
  , token_endpoint_auth_signing_alg_values_supported :: Maybe (NonEmpty Text)
  , display_values_supported :: Maybe (NonEmpty Text)
  , claim_types_supported :: Maybe (NonEmpty Text)
  , claims_supported :: Maybe (NonEmpty Text)
  , service_documentation :: Maybe Text
  , claims_locales_supported :: Maybe (NonEmpty Text)
  , claims_parameter_supported :: Maybe Bool
  , request_parameter_supported :: Maybe Bool
  , request_uri_parameter_supported :: Maybe Bool
  , require_request_uri_registration :: Maybe Bool
  , op_policy_uri :: Maybe N.URI
  , op_tos_uri :: Maybe N.URI
  , end_session_endpoint :: Maybe N.URI
  , code_challenge_methods_supported :: Maybe (NonEmpty Text)
  } deriving (Generic, Show)
instance ToJSON Discovery
instance FromJSON Discovery

data TokenResponse = TokenResponse
  { access_token :: Text
  , token_type :: Text
  , expires_in :: Maybe Int
  , refresh_token :: Maybe Text
  , scope :: Maybe Text
  , id_token :: Text
  , at_hash :: Maybe Text
  } deriving (Generic, Show)
instance ToJSON TokenResponse
instance FromJSON TokenResponse

data BasePage      = BasePage String Text
data LoginPage     = LoginPage String Text (Maybe Text) (Maybe Text) (Maybe Text) N.URI (Maybe Text) | OktaWebMessage String N.URI Text (Maybe Text) | OktaWebMessageError String N.URI Text Text (Maybe Text) deriving Show
data Login         = LoginBad | LoginGood Text | LoginGoodNoAccess deriving (Eq, Show, Generic)
instance ToJSON Login
data LoginRequest  = LoginRequest {ldata :: Maybe Text, lcid :: Maybe Text, lnonce :: Maybe Text, lru :: Maybe Text, lruid :: Maybe Text, lrpasswd :: Maybe Text} deriving (Eq, Show)
data TokenRequest  = TokenRequest {trgranttype :: Maybe Text, trcode :: Maybe Text, trredirecturi :: Maybe Text, trclientid :: Maybe Text, trclientsecret :: Maybe Text, trcodeverifier :: Maybe Text} deriving (Eq, Show)
data LogoutPage    = LogoutPage String (Maybe Text)

newtype PublicJWKSet = PublicJWKSet JWK deriving (Eq, Show)

instance FromJSON PublicJWKSet where
  parseJSON = withObject "JWK Set" (\o -> PublicJWKSet <$> o .: "keys")

instance ToJSON PublicJWKSet where
  toJSON (PublicJWKSet ks) = case toJSON ks of
    Object ks' -> Object $ KM.singleton "keys" $ Array $ singleton $ Object $ KM.insert "alg" "RS256" $ KM.insert "use" "sig" $ KM.filterWithKey (\k _ -> k `notElem` ["d", "dp", "dq", "p", "q", "qi"]) ks'
    _          -> Object $ KM.singleton "keys" Null

instance FromMultipart Mem LoginRequest where
  fromMultipart multipartData = Right $
    LoginRequest (eitherToMaybe $ lookupInput "data" multipartData)
                 (eitherToMaybe $ lookupInput "client_id" multipartData)
                 (eitherToMaybe $ lookupInput "nonce" multipartData)
                 (eitherToMaybe $ lookupInput "redirect_uri" multipartData)
                 (eitherToMaybe $ lookupInput "uid" multipartData)
                 (eitherToMaybe $ lookupInput "passwd" multipartData)

instance FromMultipart Mem TokenRequest where
  fromMultipart multipartData = Right $
    TokenRequest (eitherToMaybe $ lookupInput "grant_type" multipartData)
                 (eitherToMaybe $ lookupInput "code" multipartData)
                 (eitherToMaybe $ lookupInput "redirect_uri" multipartData)
                 (eitherToMaybe $ lookupInput "client_id" multipartData)
                 (eitherToMaybe $ lookupInput "client_secret" multipartData)
                 (eitherToMaybe $ lookupInput "code_verifier" multipartData)

uri' :: Text -> N.URI
uri' = fromMaybe N.nullURI . muri

muri :: Text -> Maybe N.URI
muri = N.parseURI . txtToStr

type OidcAPI = Header "Accept-Language" String :> (
  (".well-known" :> "openid-configuration" :> Get '[JSON] Discovery) :<|>
  ("oidc" :> (
    (".well-known" :> "openid-configuration" :> Get '[JSON] Discovery) :<|>
    (("certs" :> Get '[JSON] PublicJWKSet) :<|>
    ("login" :> MultipartForm Mem LoginRequest :> Header "referer" Text :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] Login)) :<|>
    ("token" :> MultipartForm Mem TokenRequest :> Header "authorization" Text :> Post '[JSON] (Headers '[Header "Cache-Control" Text, Header "Pragma" Text] TokenResponse)) :<|>
    ("userinfo" :> Header "authorization" Text :> Get '[JSON] OT.UserInfo) :<|>
    ("logout" :> QueryParam "id_token_hint" Text :> QueryParam "post_logout_redirect_uri" Text :> Header "cookie" Text :> Get '[HTML] (Headers '[Header "Set-Cookie" Text] LogoutPage)) :<|>
    (Get '[HTML] BasePage :<|>
      "auth" :> Header "authorization" Text :> QueryParam "response_type" Text :> QueryParam "scope" Text :> QueryParam "client_id" Text
             :> QueryParam "state" Text :> QueryParam "redirect_uri" Text :> QueryParam "response_mode" Text
             :> QueryParam "display" Text:> QueryParam "prompt" Text :> QueryParam "max_age" Text :> QueryParam "code_challenge" Text
             :> QueryParam "code_challenge_method" Text :> QueryParam "ui_locales" Text :> QueryParam "id_token_hint" Text :> QueryParam "login_hint" Text
             :> QueryParam "acr_values" Text :> QueryParam "nonce" Text :> Header "cookie" Text :> Get '[HTML] LoginPage)))) :<|>
  ("oauth2" :> ((
    (".well-known" :> "openid-configuration" :> Get '[JSON] Discovery) :<|>
    ("v1" :>
      ("keys" :> Get '[JSON] PublicJWKSet :<|>
      ("login" :> MultipartForm Mem LoginRequest :> Header "referer" Text :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] Login)) :<|>
      ("token" :> MultipartForm Mem TokenRequest :> Header "authorization" Text :> Post '[JSON] (Headers '[Header "Cache-Control" Text, Header "Pragma" Text] TokenResponse)) :<|>
      ("userinfo" :> Header "authorization" Text :> Get '[JSON] OT.UserInfo) :<|>
      ("logout" :> QueryParam "id_token_hint" Text :> QueryParam "post_logout_redirect_uri" Text :> Header "cookie" Text :> Get '[HTML] (Headers '[Header "Set-Cookie" Text] LogoutPage)) :<|>
      (Get '[HTML] BasePage :<|>
         "authorize" :> Header "authorization" Text :> QueryParam "response_type" Text :> QueryParam "scope" Text :> QueryParam "client_id" Text
                     :> QueryParam "state" Text :> QueryParam "redirect_uri" Text :> QueryParam "response_mode" Text
                     :> QueryParam "display" Text:> QueryParam "prompt" Text :> QueryParam "max_age" Text :> QueryParam "code_challenge" Text
                     :> QueryParam "code_challenge_method" Text :> QueryParam "ui_locales" Text :> QueryParam "id_token_hint" Text :> QueryParam "login_hint" Text
                     :> QueryParam "acr_values" Text :> QueryParam "nonce" Text :> Header "cookie" Text :> Get '[HTML] LoginPage)))) :<|>
    (Capture "namespace" Text :> (
      (".well-known" :> "openid-configuration" :> Get '[JSON] Discovery) :<|>
      ("v1" :>
        ("keys" :> Get '[JSON] PublicJWKSet :<|>
        ("login" :> MultipartForm Mem LoginRequest :> Header "referer" Text :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] Login)) :<|>
        ("token" :> MultipartForm Mem TokenRequest :> Header "authorization" Text :> Post '[JSON] (Headers '[Header "Cache-Control" Text, Header "Pragma" Text] TokenResponse)) :<|>
        ("userinfo" :> Header "authorization" Text :> Get '[JSON] OT.UserInfo) :<|>
        ("logout" :> QueryParam "id_token_hint" Text :> QueryParam "post_logout_redirect_uri" Text :> Header "cookie" Text :> Get '[HTML] (Headers '[Header "Set-Cookie" Text] LogoutPage)) :<|>
        (Get '[HTML] BasePage :<|>
          "authorize" :> Header "authorization" Text :> QueryParam "response_type" Text :> QueryParam "scope" Text :> QueryParam "client_id" Text
                      :> QueryParam "state" Text :> QueryParam "redirect_uri" Text :> QueryParam "response_mode" Text
                      :> QueryParam "display" Text:> QueryParam "prompt" Text :> QueryParam "max_age" Text :> QueryParam "code_challenge" Text
                      :> QueryParam "code_challenge_method" Text :> QueryParam "ui_locales" Text :> QueryParam "id_token_hint" Text :> QueryParam "login_hint" Text
                      :> QueryParam "acr_values" Text :> QueryParam "nonce" Text :> Header "cookie" Text :> Get '[HTML] LoginPage))))))))

loginforms :: Monad m => String -> HtmlT m ()
loginforms lang = do
  form_ [name_ "login", id_ "login", onsubmit_ "dologin(); return false;"] $ do
    input_ [type_ "text",     name_ "uid",    id_ "uid",    placeholder_ $ tr lang "Username or Email Address", autofocus_]
    input_ [type_ "password", name_ "passwd", id_ "passwd", placeholder_ $ tr lang "Password"]
    input_ [type_ "text", name_ "data", id_ "data", hidden_ "true"]
    input_ [type_ "text", name_ "client_id", id_ "client_id", hidden_ "true"]
    input_ [type_ "text", name_ "nonce", id_ "nonce", hidden_ "true"]
    input_ [type_ "text", name_ "redirect_uri", id_ "redirect_uri", hidden_ "true"]
    div_ [style_ "flex-flow: row-reverse wrap;"] $ input_ [type_ "submit", value_ "Login"]

loginscript :: Text
loginscript = "\
\oidcnonce = '';\n\
\oidcru = '';\n\
\oidcstate = '';\n\
\oidcdata = '';\n\
\oidccid = '';\n\
\const dologin = async() => {\n\
\  if (oidcdata) {\n\
\    document.login.elements['data'].value = oidcdata;\n\
\  }\n\
\  if (oidccid) {\n\
\    document.login.elements['client_id'].value = oidccid;\n\
\  }\n\
\  if (oidcnonce) {\n\
\    document.login.elements['nonce'].value = oidcnonce;\n\
\  }\n\
\  document.login.elements['redirect_uri'].value = oidcru;\n\
\  document.login.elements['uid'].readOnly = true;\n\
\  document.login.elements['passwd'].readOnly = true;\n\
\  document.login.elements['uid'].style.cursor = 'wait';\n\
\  document.login.elements['passwd'].style.cursor = 'wait';\n\
\  document.body.style.cursor = 'wait';\n\
\  const response = await fetch('login', {method:'POST', body: new FormData(document.login)});\n\
\  const myJson = await response.json();\n\
\  switch (myJson.tag) {\n\
\    case 'LoginGood':\n\
\      if (oidcru) {\n\
\        if (oidcstate) {\n\
\          location = oidcru + '?code=' + myJson.contents + '&state=' + oidcstate;\n\
\        } else {\n\
\          location = oidcru + '?code=' + myJson.contents;\n\
\        }\n\
\      }\n\
\      break;\n\
\    case 'LoginGoodNoAccess':\n\
\      if (oidcru) {\n\
\        if (oidcstate) {\n\
\          location = oidcru + '?error=access_denied&error_description=No%20Access&state=' + oidcstate;\n\
\        } else {\n\
\          location = oidcru + '?error=access_denied&error_description=No%20Access';\n\
\        }\n\
\      }\n\
\      break;\n\
\    default:\n\
\      document.body.style.cursor = '';\n\
\      document.login.elements['uid'].style.cursor = '';\n\
\      document.login.elements['uid'].readOnly = false;\n\
\      document.login.elements['passwd'].style.cursor = '';\n\
\      document.login.elements['passwd'].value = '';\n\
\      document.login.elements['passwd'].readOnly = false;\n\
\      document.login.elements['passwd'].focus();\n\
\  }\n\
\}\n"

oktapostmessagescript :: N.URI -> Text -> Maybe Text -> Text
oktapostmessagescript ru code st = "\
\(function(window, document, undefined) {\n\
\  var redirectURI = '" <> pack (show ru) <> "';\n\
\  var webMessageRequest = {};\n\
\  var authorizationResponse = {\n\
\    type: 'authorization_response',\n\
\    response: {\n\
\      code: '" <> code <> "',\n" <> st' "  " <> "\
\    }\n\
\  };\n\
\  var mainWin = (window.opener && (window.opener != window)) ? window.opener : window.parent;\n\
\  mainWin?.postMessage({\n\
\    type: 'authorization_response',\n\
\    response: authorizationResponse,\n\
\    code: '" <> code <> "',\n" <> st' "" <> "\
\  }, redirectURI);\n\
\})(this, this.document);\n"
  where
    st' s = case st of
              Just st'' -> s <> "    state: '" <> st'' <> "',\n"
              _         -> ""

oktapostmessageerrorscript :: N.URI -> Text -> Text -> Maybe Text -> Text
oktapostmessageerrorscript ru er msg st = "\
\(function(window, document, undefined) {\n\
\  var redirectURI = '" <> pack (show ru) <> "';\n\
\  var webMessageRequest = {};\n\
\  var authorizationResponse = {\n\
\    type: 'authorization_response',\n\
\    response: {\n\
\      error: '" <> er <> "',\n\
\      error_description: '" <> msg <> "',\n" <> st' "  " <> "\
\    }\n\
\  };\n\
\  var mainWin = (window.opener && (window.opener != window)) ? window.opener : window.parent;\n\
\  mainWin?.postMessage({\n\
\    type: 'authorization_response',\n\
\    response: authorizationResponse,\n\
\    error: '" <> er <> "',\n\
\    error_description: '" <> msg <> "',\n" <> st' "" <> "\
\  }, redirectURI);\n\
\})(this, this.document);\n"
  where
    st' s = case st of
              Just st'' -> s <> "      state: '" <> st'' <> "',\n"
              _         -> ""

instance ToHtml BasePage where
  toHtml (BasePage lang sitename) = doctypehtml lang $ do
    head__ True $ do
      title_ $ toHtmlRaw $ "Sign in" <> if T.null sitename then "" else " - " <> sitename
      script_ [defer_ "true"] loginscript
    main__ (loginforms lang >> logo True) $
      script_ [defer_ "true"] ("oidcnonce = history.state.no;\n\
                               \oidcru = history.state.ru;\n\
                               \oidcstate = history.state.st;\n\
                               \oidcdata = history.state.da;\n\
                               \oidccid = history.state.ci;\n\
                               \\n" :: String)
  toHtmlRaw = toHtml

instance ToHtml LoginPage where
  toHtml (LoginPage lang sitename data' cid nonce ru st) = doctypehtml lang $ do
    head__ True $ do
      meta_ [charset_ "utf-8"]
      title_ $ toHtmlRaw $ "Sign in" <> if T.null sitename then "" else " - " <> sitename
      script_ [defer_ "true"] $ loginscript <> "history.pushState({'da': '" <> fromMaybe "" data' <> "', 'ci': '" <> fromMaybe "" cid <> "', 'no': '" <> fromMaybe "" nonce <> "', 'ru': '" <> pack (show ru) <> "', 'st': '" <> fromMaybe "" st <> "'}, '', './');\n\
                                             \oidcru = '" <> pack (show ru) <> "';\n"
                                          <> maybe "" (\n -> "oidcnonce = '" <> n <> "';\n") nonce
                                          <> maybe "" (\s -> "oidcstate = '" <> s <> "';\n") st
                                          <> maybe "" (\d -> "oidcdata = '" <> d <> "';\n") data'
                                          <> maybe "" (\c -> "oidccid = '" <> c <> "';\n") cid
    main__ (loginforms lang >> logo True) mempty
  toHtml (OktaWebMessage lang ru code st) = doctypehtml lang $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ "Authorization Repsonse"
    body_ $ script_ $ oktapostmessagescript ru code st
  toHtml (OktaWebMessageError lang ru er msg st) = doctypehtml lang $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ "Authorization Repsonse"
    body_ $ script_ $ oktapostmessageerrorscript ru er msg st
  toHtmlRaw = toHtml

instance ToHtml LogoutPage where
  toHtml (LogoutPage lang plru) = doctypehtml lang $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [httpEquiv_ "refresh", content_ $ "0; url='" <> fromMaybe "/" plru <> "'"]
      title_ "Logout"
    body_ mempty
  toHtmlRaw = toHtml

oidcAuthError :: N.URI -> Text -> Text -> Maybe Text -> Handler a
oidcAuthError ru e msg st = redirect ru $ [("error", Just e), ("error_description", Just msg)] ++ [("state", st) | isJust st]

server :: (BlockCipher c, JassoSource s) => JassoState -> JWK -> c -> s -> [((Text, Text), (Text, Text, Maybe [Text], [(Text, Text)]))] -> Server OidcAPI
server jassoState privkey' symkey src rps lang'' = getOidcMeta "" :<|> oidchandlers "" :<|> (oidchandlers "/oauth2" :<|> oidchandlers . (strToTxt . ("/oauth2" </>) . txtToStr))
  where
    oidchandlers namespace = getOidcMeta namespace :<|> (getCerts :<|> getLogin :<|> getToken namespace :<|> getUserInfo :<|> getLogout lang' :<|> (getBasePage lang' :<|> getLoginPage lang'))
    lang' = bestLang lang''
    conf = jassoConfig jassoState
    v = verbosity conf
    assertSecs' = assertSecs conf
    sessSecs = sessionSecs conf
    baseuri = "https://" <> domain conf
    oidcuri = baseuri <> "/oidc/"

    ln :: [a] -> Maybe (NonEmpty a)
    ln []     = Nothing
    ln (x:xs) = Just $ x :| xs

    getOidcMeta namespace = do
      return $ Discovery
        { issuer = uri' (baseuri <> namespace)
        , authorization_endpoint = uri' $ oidcuri <> "auth"
        , token_endpoint = muri $ oidcuri <> "token"
        , userinfo_endpoint = muri $ oidcuri <> "userinfo"
        , jwks_uri = uri' $ oidcuri <> "certs"
        , registration_endpoint = Nothing
        , scopes_supported = ln ["openid", "email", "profile"]
        , response_types_supported = "code" :| []
        , response_modes_supported = Just ("query" :| ["fragment", "form_post", "okta_post_message"])
        , grant_types_supported = ln ["authorization_code"]
        , acr_values_supported = Nothing
        , subject_types_supported = "public" :| []
        , id_token_signing_alg_values_supported = "RS256" :| []
        , id_token_encryption_alg_values_supported = Nothing
        , id_token_encryption_enc_values_supported = Nothing
        , userinfo_signing_alg_values_supported = Nothing
        , userinfo_encryption_alg_values_supported = Nothing
        , userinfo_encryption_enc_values_supported = Nothing
        , request_object_signing_alg_values_supported = Nothing
        , request_object_encryption_alg_values_supported = Nothing
        , request_object_encryption_enc_values_supported = Nothing
        , token_endpoint_auth_methods_supported = ln ["client_secret_basic", "client_secret_post"]
        , token_endpoint_auth_signing_alg_values_supported = Nothing
        , display_values_supported = Nothing
        , claim_types_supported = Nothing
        , claims_supported = ln ["aud", "email", "email_verified", "exp", "family_name", "given_name", "iat", "iss", "name", "preferred_usernam", "sub", "organization", "organization_id", "organization_name", "groups", "roles", "permissions"]
        , service_documentation = Nothing
        , claims_locales_supported = Nothing
        , claims_parameter_supported = Nothing
        , request_parameter_supported = Nothing
        , request_uri_parameter_supported = Nothing
        , require_request_uri_registration = Nothing
        , op_policy_uri = Nothing
        , op_tos_uri = Nothing
        , end_session_endpoint = muri $ oidcuri <> "logout"
        , code_challenge_methods_supported = ln ["S256"]
        }

    getCerts = do
      return $ PublicJWKSet privkey'

    getBasePage lang = return $ BasePage lang $ siteName conf

    getLoginPage :: String -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Handler LoginPage
    getLoginPage lang auth' response_type scope' client_id' state' redirect_uri' response_mode _display prompt _max_age code_challenge code_challenge_method _ui_locales _id_token_hint _login_hint _acr_values nonce cookies = do
      liftIO $ logItIO v Debug $ "loginPage: lang '" <> show lang <> "' auth '" <> show auth' <> "' response_type '" <> show response_type <> "' scope '" <> show scope'
                              <> "' client_id '" <> show client_id' <> "' state '" <> show state' <> "' response_mode '" <> show response_mode
                              <> "' display '" <> show _display <> "' max_age '" <> show _max_age <> "' code_challenge '" <> show code_challenge
                              <> "' code_challenge_method '" <> show code_challenge_method <> "' ui_locales '" <> show _ui_locales
                              <> "' id_token_hint '" <> show _id_token_hint <> "' login_hint '" <> show _login_hint <> "' acr_values '" <> show _acr_values
                              <> "' nonce '" <> show nonce <> "' cookies '" <> show cookies
                              <> "' client_id '" <> show client_id' <> "' redirect_uri '" <> show redirect_uri' <> "' prompt '" <> show prompt <> "'"

      ru' <- maybeThrow401 "OIDC" v "no redirect_uri" redirect_uri'
      ci' <- maybeThrow401 "OIDC" v "no client_id" client_id'
      (_name, _cs, access, _rm) <- maybeThrow401 "OIDC" v ("unknown redirect_uri: " <> ru' <> " or client_id: " <> ci') $ lookup (ru', ci') rps
      ru <- maybeThrow401 "OIDC" v "no redirect_uri" $ N.parseURI $ txtToStr ru' -- FIXME: should ensure scheme is https but this hinders local dev and test deployments, so this should be configurable
--       ru <- ensure (fromMaybe N.nullURI $ N.parseURI ru') state' "redirect_uri" ((== "https:") . uriScheme) (N.parseURI ru')

      _  <- ensure ru state' "response_type" (== "code") response_type
      _  <- ensure ru state' "scope" (("openid" `elem`) . T.words) scope'

      sess <- checkSession src jassoState symkey assertSecs' cookies
      liftIO $ logItIO v Debug $ "loginPage: session '" <> show sess <> "'"

      liftIO $ logItIO v Debug $ show (code_challenge, client_id')
      cc <- liftIO $ mapM (encrypt symkey) code_challenge
      cid <- liftIO $ mapM (encrypt symkey) client_id'
      when (isJust cc) $ void $ ensure ru state' "code_challenge_method" (== "S256") code_challenge_method

      case response_mode of
        Just "okta_post_message" -> do
          case sess of
            Nothing -> do
              liftIO $ logItIO v Debug ("OIDC: Requested okta_post_message but there is no existing session" :: String)
              return $ OktaWebMessageError lang ru "login_required" "no existing session" state'
            Just (u, n, et, _ss) -> do
              if allowAccess u access
                then do
                  code <- mkCode nonce ru n et u code_challenge
                  liftIO $ logItIO v Debug $ "OIDC: Existing session for " <> uid u
                  return $ OktaWebMessage lang ru code state'
                else do
                  liftIO $ logItIO v Debug $ "OIDC: Existing session for " <> uid u <> ", but user not in access list"
                  return $ OktaWebMessageError lang ru "access_denied" "not in access list" state'

        _ -> do
          let pr = case prompt of
                     Nothing -> []
                     Just ps -> sort $ T.words ps

          let implicit_consent = True -- consent is assumed in the case that all RP usage is already consented to by all accounts on
                                      -- the OP (e.g. prior consent is made at account creation time, or RP and OP are the same company)

          let loginpage = return $ LoginPage lang (siteName conf) cc cid nonce ru state'
          liftIO $ logItIO v Debug $ show $ LoginPage lang (siteName conf) cc cid nonce ru state'
          liftIO $ logItIO v Debug $ show (lang, siteName conf, decrypt symkey =<< cc, decrypt symkey =<< cid, nonce, ru, state')

          case ("none" `elem` pr, "login" `elem` pr, "consent" `elem` pr && not implicit_consent, "select_account" `elem` pr, sess) of
            (True, False, False, False, Nothing) -> do
              liftIO $ logItIO v Debug ("OIDC: Requested no interaction but there is no existing session" :: String)
              oidcAuthError ru "login_required" "requested no interaction but there is no existing session" state'

            (False, False, _, True, Nothing) -> do
              liftIO $ logItIO v Debug ("OIDC: Requested account selection only but there is no existing session" :: String)
              oidcAuthError ru "login_required" "requested account selection only but there is no existing session" state'

            (False, True, _, _, _) -> do
              when (isJust sess) $ liftIO $ logItIO v Debug ("OIDC: Already logged in but creating login dialog anyway, as requested" :: String)
              loginpage

            (False, _, _, True, _) -> do
              when (isJust sess) $ liftIO $ logItIO v Debug ("OIDC: Already logged in but creating account selection dialog anyway, as requested" :: String)
              loginpage

            (False, False, True, False, _) -> loginpage

            (False, False, False, False, Nothing) -> loginpage

            (_, False, False, False, Just (u, n, et, _ss)) -> do
              if allowAccess u access
                then do
                  code <- mkCode nonce ru n et u code_challenge
                  liftIO $ logItIO v Debug $ "OIDC: Existing session for " <> uid u
                  redirect ru [("code", Just code), ("state", state')]
                else do
                  liftIO $ logItIO v Debug $ "OIDC: Existing session for " <> uid u <> ", but user not in access list"
                  oidcAuthError ru "access_denied" "not in access list" state'

            _ -> do
              liftIO $ logItIO v Debug ("OIDC: Unsupported prompt" :: String)
              oidcAuthError ru "invalid_request" "unsupported prompt" state'

        where
          ensure :: Show a => N.URI -> Maybe Text -> Text -> (a -> Bool) -> Maybe a -> Handler a
          ensure ru state'' name' p mx = do
            x <- maybeOidcAuthError ru "invalid_request" ("no " <> name') mx
            if p x then return x else do
              liftIO $ logItIO v Debug ("OIDC: Unsupported " <> show name' <> ": " <> show x)
              oidcAuthError ru "invalid_request" ("unsupported " <> name') state''

          maybeOidcAuthError :: N.URI -> Text -> Text -> Maybe a -> Handler a
          maybeOidcAuthError ru e msg = maybe throwit return
            where
              throwit = do
                liftIO $ logItIO v Debug $ "OIDC: Error " <> e <> ": " <> msg
                oidcAuthError ru e msg state'

    getLogin (LoginRequest data' client_id' nonce' ru' uid' passwd') referer' = do
      let nonce = nonce'
      let cc = decrypt symkey =<< data'
      let ci = decrypt symkey =<< client_id'
      liftIO $ logItIO v Debug $ "OIDC: getLogin " <> show data' <> ", " <> show client_id' <> ", " <> show ci <> ", " <> show nonce' <> ", " <> show ru' <> ", " <> show uid' <> ", " <> show referer'

      ru      <- jsonerror' "bad uid" ru' -- purposefully misleading error messages (TODO: should debug log the real reason)
      ci'     <- jsonerror' "bad uid" ci
      uid''   <- jsonerror' "bad uid" uid'
      passwd  <- jsonerror' "bad uid" passwd'
      referer <- jsonerror' "bad uid" referer'
      (_name, _cs, access, _rm) <- jsonerror' "bad uid" $ lookup (ru, ci') rps
      if baseuri `T.isPrefixOf` referer then return () else throw401 "OIDC" v "Referer invalid" (jsonerror "bad uid")

      userM <- liftIO $ hoistJassoT jassoState $ verifyUser src uid'' passwd
      case userM of
        Just (user, token) -> do
          let allowed = allowAccess user access
          liftIO $ logItIO v Debug $ "OIDC: Login successful for " <> uid user <> if allowed then "" else ", but user not in access list"
          mkSession symkey assertSecs' sessSecs token $ \n et -> if allowed
            then LoginGood <$> mkCode nonce (fromMaybe N.nullURI $ N.parseURI $ txtToStr ru) n et user cc
            else return LoginGoodNoAccess
        _                  -> do
          delayms <- randomRIO (5000000, 9000000) -- TODO: record failed login attempts by username and IP address for rate-limiting purposes
          liftIO $ logItIO v Debug $ "OIDC: Wrong password for " <> unpack uid'' <> ", delaying " <> show delayms <> "ms"
          liftIO $ threadDelay delayms
          return $ noHeader LoginBad
      where
        maybeThrow401' = maybeThrow401 "OIDC" v
        jsonerror s = "{\"tag\": \"LoginBad\", \"contents\": \"" <> s <> "\"}"
        jsonerror' = maybeThrow401' . jsonerror

    mkCode :: MonadIO m => Maybe Text -> N.URI -> UTCTime -> UTCTime -> User -> Maybe Text -> m Text
    mkCode nonce ru n et user cc = liftIO $ encrypt symkey $ strToTxt $ show (fromMaybe "" nonce, show ru, uuid user, round @_ @Int $ utcTimeToPOSIXSeconds n, round @_ @Int $ utcTimeToPOSIXSeconds et, cc)

    unpackCode :: Text -> Maybe Text -> Handler (Maybe (Maybe Text, Text, User, UTCTime, UTCTime))
    unpackCode c cv = do
      let d = (readMaybe . txtToStr) =<< decrypt symkey c
      case d of
        Nothing -> do
          liftIO $ logItIO v Debug ("OIDC: Couldn't decode code" :: String)
          return Nothing
        Just (no, r, u, n, et, cc :: Maybe Text) -> do
          case cv of
            Nothing  -> do
              liftIO $ logItIO v Debug ("OIDC: cc is not required" :: String)
              return ()
            Just cv' -> case cc of
                Nothing  -> do
                  throw401 "OIDC" v ("cc is not present: cv: " <> cv') "code_challenge"
                Just cc' -> when (T.dropWhileEnd (== '=') cc' /= T.dropWhileEnd (== '=') (TE.decodeUtf8 $ B64U.encode $ BA.convert $ hash @BU.ByteString @SHA256 $ TE.encodeUtf8 cv')) $ do
                  throw401 "OIDC" v ("OIDC: cc and cv not matching: cc: " <> T.dropWhileEnd (== '=') cc'
                            <> " cv: " <> cv' <> " hash cv: " <> T.dropWhileEnd (== '=') (TE.decodeUtf8 $ B64U.encode $ BA.convert $ hash @BU.ByteString @SHA256 $ TE.encodeUtf8 cv')) "code_challenge"
          user <- liftIO $ hoistJassoT jassoState $ getUserByUUID src u
          case user of
            Nothing -> return Nothing
            Just u' -> do
              return $ Just (if T.null no then Nothing else Just no, r, u', posixSecondsToUTCTime $ fromInteger n, posixSecondsToUTCTime $ fromInteger et)

    mkAccessToken :: MonadIO m => Text -> Bool -> UTCTime -> UTCTime -> UTCTime -> User -> Text -> [(Text, Text)] -> Maybe Text -> m (Maybe Text)
    mkAccessToken namespace isAPI cn n et u cid rm no = if isAPI then (do
      let claims = emptyClaimsSet & claimIss       ?~ ((baseuri <> namespace) ^.re string)
                                  & claimSub       ?~ (mail u ^.re string)
                                  & claimAud       ?~ Audience ["api://default"]
                                  & claimIat       ?~ NumericDate n
                                  & claimExp       ?~ NumericDate et
      let claims' = addNonce
                  $ addClaim "cid" (AT.String cid)
                  $ addClaim "uid" (AT.String $ uuid u)
                  $ addClaim "auth_time" (AT.Number $ fromIntegral $ round @_ @Int $ utcTimeToPOSIXSeconds cn)
                  $ addClaim "name" (AT.String $ fullName u)
                  $ addClaim "given_name" (AT.String $ givenName u)
                  $ addClaim "firstName" (AT.String $ givenName u)
                  $ addClaim "family_name" (AT.String $ familyName u)
                  $ addClaim "lastName" (AT.String $ familyName u)
                  $ addClaim "preferred_username" (AT.String $ uid u)
                  $ addClaim "email" (AT.String $ mail u)
                  $ addClaim "email_verified" (AT.Bool True)
                  $ addClaim "organization" (AT.String $ ouId u)
                  $ addClaim "organization_id" (AT.String $ ouId u)
                  $ addClaim "organization_name" (AT.String $ ouName u)
                  $ addClaim "groups" (AT.Array $ fromList ps)
                  $ addClaim "roles" (AT.Array $ fromList ps)
                  $ addClaim "permissions" (AT.Array $ fromList ps)
                  $ addClaim "amr" (AT.String "pwd") -- FIXME make it actually reflect the login method used
                    claims
      r <- liftIO ((runJOSE $ do
        let alg' = RS256
        liftIO $ logItIO v Debug $ "OIDC: AT: JWK alg chosen: " <> show alg'
        let joseHeader = newJWSHeader ((), alg') & kid .~ (HeaderParam () <$> privkey' ^. jwkKid)
        signClaims privkey' joseHeader claims') :: IO (Either JWTError SignedJWT))
      case r of
        Left e  -> do
          logItIO v Debug $ "OIDC: AT: Couldn't sign claims set: " <> show e
          return Nothing
        Right j -> do
          logItIO v Debug $ "OIDC: AT: Signed claims set: " <> BLU.toString (A.encode $ toJSON claims')
          return $ Just $ toStrict $ decodeUtf8 $ encodeCompact j
     ) else Just <$> liftIO (encrypt symkey $ strToTxt $ show (round @_ @Int $ utcTimeToPOSIXSeconds n, uuid u, ps'))
      where
        addNonce = case no of
          Nothing  -> id
          Just ""  -> id
          Just no' -> addClaim "nonce" $ AT.String no'
        ps = map AT.String ps'
        ps' = concatMap getPermissions $ groups u
        getPermissions g = case lookup g rm of
                             Nothing -> []
                             Just r  -> [r]

    unpackAccessToken :: MonadIO m => Text -> m (Maybe (UTCTime, Text, [Text]))
    unpackAccessToken t = do
      logItIO v Debug $ "OIDC: Unpacking access token: " <> t
      let dc' = (decodeCompact $ BL.fromStrict $ TE.encodeUtf8 t) :: Either JWTError (CompactJWS JWSHeader)
      case dc' of
        Left _e   -> return $ do
          d <- txtToStr <$> decrypt symkey t
          case readMaybe d of
            Nothing         -> Nothing
            Just (n, u, ps) -> Just (posixSecondsToUTCTime $ fromInteger n, u, ps)
        Right tok -> do
          logItIO v Debug $ "OIDC: decodeCompact good: " <> show tok
          d :: Either JWTError ClaimsSet <- liftIO $ runJOSE $ verifyClaims (defaultJWTValidationSettings (== "api://default") & allowedSkew .~ 2) privkey' tok
          case d of
            Left e' -> do
              logItIO v Detail $ "OIDC: verifyJWS failed: " <> show e'
              return Nothing
            Right r -> do
              logItIO v Debug $ "OIDC: verifyJWS succeded: " <> show r
              case (r ^. claimIat, (r ^. unregisteredClaims) DM.!? "uid", (r ^. unregisteredClaims) DM.!? "permissions") of
                (Just (NumericDate iat'), Just (String uid'), Just (Array ps)) -> return $ Just (iat', uid', mapMaybe unpack' $ toList ps)
                _ -> do
                  logItIO v Debug ("OIDC: No iat or no uid" :: String)
                  return Nothing
      where
        unpack' (String s) = Just s
        unpack' _ = Nothing

    mkIdToken :: Text -> Maybe Text -> UTCTime -> UTCTime -> UTCTime -> User -> Text -> Text -> [(Text, Text)] -> IO (Maybe SignedJWT)
    mkIdToken namespace no cn n et u a atok rm = do
      let claims = emptyClaimsSet & claimIss       ?~ ((baseuri <> namespace) ^.re string)
                                  & claimSub       ?~ (uuid u ^.re string)
                                  & claimAud       ?~ Audience [a ^.re string]
                                  & claimIat       ?~ NumericDate n
                                  & claimExp       ?~ NumericDate et
      let claims' = addNonce
                  $ addClaim "auth_time" (AT.Number $ fromIntegral $ round @_ @Int $ utcTimeToPOSIXSeconds cn)
                  $ addClaim "name" (AT.String $ fullName u)
                  $ addClaim "given_name" (AT.String $ givenName u)
                  $ addClaim "family_name" (AT.String $ familyName u)
                  $ addClaim "preferred_username" (AT.String $ uid u)
                  $ addClaim "email" (AT.String $ mail u)
                  $ addClaim "email_verified" (AT.Bool True)
                  $ addClaim "organization" (AT.String $ ouId u)
                  $ addClaim "organization_id" (AT.String $ ouId u)
                  $ addClaim "organization_name" (AT.String $ ouName u)
                  $ addClaim "groups" (AT.Array $ fromList $ concatMap getPermissions $ groups u)
                  $ addClaim "roles" (AT.Array $ fromList $ concatMap getPermissions $ groups u)
                  $ addClaim "permissions" (AT.Array $ fromList $ concatMap getPermissions $ groups u)
                  $ addClaim "amr" (AT.String "pwd") -- FIXME make it actually reflect the login method used
                  $ addClaim "at_hash" (AT.String $ TE.decodeUtf8 $ C8.filter (/= '=') $ B64U.encode $ C8.take 16 $ BA.convert $ hash @_ @SHA256 $ TE.encodeUtf8 atok)
                    claims
      r <- (runJOSE $ do
        let alg' = RS256
        liftIO $ logItIO v Debug $ "OIDC: JWK alg chosen: " <> show alg'
        let joseHeader = newJWSHeader ((), alg') & kid .~ (HeaderParam () <$> privkey' ^. jwkKid)
        signClaims privkey' joseHeader claims') :: IO (Either JWTError SignedJWT)
      case r of
        Left e  -> do
          logItIO v Debug $ "OIDC: Couldn't sign claims set: " <> show e
          return Nothing
        Right j -> do
          logItIO v Debug $ "OIDC: Signed claims set: " <> BLU.toString (A.encode $ toJSON claims')
          return $ Just j
      where
        addNonce = case no of
          Nothing  -> id
          Just ""  -> id
          Just no' -> addClaim "nonce" $ AT.String no'
        getPermissions g = case lookup g rm of
                             Nothing -> []
                             Just r  -> [AT.String r]

    getUserInfo auth' = do
      basicauth <- maybeThrow401 "OIDC" v "invalid_token" auth'
      liftIO $ logItIO v Debug $ "OIDC: userinfo request: " <> basicauth
      unless ("bearer " `T.isPrefixOf` T.map toLower basicauth) $ throw401 "OIDC" v "Auth not a bearer token" "invalid_token"
      atok <- unpackAccessToken $ T.drop 7 basicauth
      (t, u, ps) <- maybeThrow401 "OIDC" v "invalid_token" atok
      now <- posixSecondsToUTCTime . fromIntegral . round @_ @Int . utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
      unless (diffUTCTime now t < assertSecs') $ throw401 "OIDC" v "Expired token" "invalid_token"
      unless (diffUTCTime now t >= (-2)) $ throw401 "OIDC" v "Not yet valid token" "invalid_token"
      user' <- liftIO $ hoistJassoT jassoState $ getUserByUUID src u
      user  <- maybeThrow401 "OIDC" v "invalid_token" user'
      let gs = ps
      let rs = ps
      let u' = OT.UserInfo (uuid user) (uid user) (fullName user) (givenName user) (familyName user) (mail user) "1" gs rs ps (ouId user) (ouName user)
      liftIO $ logItIO v Debug $ "OIDC: Sending userinfo: " <> BLU.toString (A.encode $ toJSON u')
      return u'

    getToken namespace tq@(TokenRequest gt' code' ru' cid' cpw' cv') basicauth = do
      liftIO $ logItIO v Debug $ "OIDC: Token request: " <> show (tq, basicauth)
      gt   <- jsonerror' gt'
      code <- jsonerror' code'
      ru   <- jsonerror' ru'
      (client_id, cv, rm) <- case basicauth of
        Nothing -> do
          cid  <- jsonerror' cid'
          (_name, cs, _access, rm') <- jsonerror' $ lookup (ru, cid) rps
          case cpw' of
            Just cpw -> do
              liftIO $ logItIO v Debug $ "OIDC: Post auth: client_id: " <> cid <> " client_secret: ****"
              ensure "client_secret" $ cpw == cs
              return (cid, Nothing, rm')
            Nothing -> do
              liftIO $ logItIO v Debug $ "OIDC: Post no auth (public client): client_id: " <> cid
              liftIO $ logItIO v Debug $ "OIDC: code_verifier " <> show cv'
              cv'' <- jsonerror' cv'
              return (cid, Just cv'', rm')
        Just ba -> case T.words ba of
          ["Basic", ba'] -> case T.splitOn ":" $ TE.decodeUtf8 $ B64.decodeLenient $ TE.encodeUtf8 ba' of
                              [client_id', client_secret] -> do
                                liftIO $ logItIO v Debug $ "OIDC: Basic auth: client_id: " <> client_id' <> " client_secret: ****"
                                (_name, cs, _access, rm') <- jsonerror' $ lookup (ru, client_id') rps
                                ensure "client_secret" $ client_secret == cs
                                return (client_id', Nothing, rm')
                              _                           -> throw400 jsonerror
          _              -> throw401 "OIDC" v ("non-basic auth?" <> ba) "invalid_token"
      ensure "gt" $ gt == "authorization_code"
      liftIO $ logItIO v Debug ("OIDC: Code decoded: " <> show (decrypt symkey $ strToTxt $ N.unEscapeString $ txtToStr code))
      code'' <- unpackCode (strToTxt <$> N.unEscapeString $ txtToStr code) cv
      (nonce, cru, user, cn, et) <- jsonerror' code''
      ensure "ru == cru" $ ru == cru
      now <- posixSecondsToUTCTime . fromIntegral . round @_ @Int . utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
      liftIO $ logItIO v Debug $ "OIDC: Checking times: now: " <> show now <> ", cn: " <> show cn <> ", et: " <> show et
      ensure "now - cn < 30"    $ diffUTCTime now cn < 30
      ensure "now - cn >= (-2)" $ diffUTCTime now cn >= (-2)
      ensure "et - now > 0"     $ diffUTCTime et now > 0
      atok'  <- mkAccessToken namespace (isJust cv) cn now et user client_id rm nonce
      atok   <- jsonerror' atok'
      idtok' <- liftIO $ mkIdToken namespace nonce cn now et user client_id atok rm
      idtok  <- jsonerror' idtok'

      let tokr = TokenResponse {
          access_token = atok
        , token_type = "Bearer"
        , expires_in = Just $ round assertSecs'
        , refresh_token = Nothing
        , scope = Just "openid profile email"
        , id_token = toStrict $ decodeUtf8 $ encodeCompact idtok
        , at_hash = Nothing
        }
      liftIO $ logItIO v Debug $ "OIDC: TokenResponse: " <> BLU.toString (A.encode $ toJSON tokr)
      liftIO $ logItIO v Info $ "OIDC: Assertion created for " <> pack (show ru) <> ": " <> uid user <> " (" <> fullName user <> " <" <> mail user <> ">) [" <> T.intercalate ", " (ps' user rm) <> "]"
      return $ addHeader "no-cache" $ addHeader "no-store" tokr
      where
        ensure :: Text -> Bool -> Handler ()
        ensure msg x = if x then return () else do
          liftIO $ logItIO v Debug $ "OIDC: Ensure failed: " <> msg
          throw400 jsonerror
        jsonerror' = maybeThrow400 "OIDC" v jsonerror
        jsonerror = "{\"error\": \"invalid_request\"}" :: BLU.ByteString
        ps' u rm = concatMap (getPermissions rm) $ groups u
        getPermissions rm g = case lookup g rm of
                                Nothing -> []
                                Just r  -> [r]

    getLogout :: String -> Maybe Text -> Maybe Text -> Maybe Text -> Handler (Headers '[Header "Set-Cookie" Text] LogoutPage)
    getLogout l _id_token_hint' post_logout_redirect_uri' _cookies' = do
      liftIO $ logItIO v Debug $ "OIDC: Logout: " <> show _id_token_hint' <> " " <> show post_logout_redirect_uri' <> " " <> show _cookies'
      -- FIXME: Should validate the request by checking id_token_hint and session cookie
      endSession $ return $ LogoutPage l post_logout_redirect_uri'  -- FIXME: Should check it's in the list

data RPConf = RPConf { name :: Text, redirect_uris :: [Text], client_secrets :: [(Text, Text)], allow :: Maybe [Text], mapping :: Maybe [RoleMap] } deriving (Show, Eq, Generic)
instance FromJSON RPConf

getRPs :: MonadIO m => LogLevel -> FilePath -> m [((Text, Text), (Text, Text, Maybe [Text], [(Text, Text)]))]
getRPs v rpDir = liftIO $ catch (do
  rpdirContents <- getDirectoryContents rpDir
  let rpconfs = filter (".conf" `isSuffixOf`) rpdirContents
  rpconf <- mapM getRPconfs rpconfs
  return $ concat rpconf) ioCatcherD
  where
    getRPconfs filename = getRPconf =<< loadYamlSettings [rpDir </> filename] [] useEnv
      where
        getRPconf (RPConf n rus cs a rm) = return $ do
          ru <- rus
          (clientid, secret) <- cs
          return ((ru, clientid), (n, secret, a, getRMs rm))
        getRMs Nothing  = []
        getRMs (Just rms) = map (\(RoleMap r p) -> (r, p)) rms
    ioCatcherD :: IOError -> IO [a]
    ioCatcherD e = do
      logItIO v Error $ "OIDC: Couldn't read RP config directory" ++ maybe "" (\s -> " \"" ++ s ++ "\"") (ioe_filename e) ++ ": " ++ show (ioe_type e) ++ " (" ++ ioe_description e ++ "), ignoring"
      return []

oidcSink :: JassoSource s => s -> JassoIO (Server OidcAPI)
oidcSink src = do
  logIt Normal ("OIDC: Starting" :: String)
  (symkey, _) <- getSymKey
  (privkey', _) <- getKeyAndCert
  jassoState <- get
  conf <- getConf
  rps <- getRPs (verbosity conf) (varDir conf </> "oidc")
  case rps of
    [] -> logIt Warning ("OIDC: No RPs configured" :: String)
    _  -> do
      logIt Info $ "OIDC: Configured RPs: " <> T.intercalate ", " (nub $ map desc rps)
      logIt Debug $ "OIDC: Configured RPs: " <> T.intercalate ", " (map (pack . show) rps)
  let pk = fromRSA $ toPrivateKey privkey'
  let h = view thumbprint pk :: Digest SHA256
  let kid' = view (re (base64url . digest) . utf8) h
  let pk' = set jwkKid (Just kid') pk
  return $ server jassoState pk' symkey src rps
  where
    desc (_, (n, _, _, _)) = n
