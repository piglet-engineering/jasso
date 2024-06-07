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

{-# LANGUAGE DataKinds, TypeOperators, MultiParamTypeClasses, OverloadedStrings, DeriveGeneric #-}

module Saml(SamlAPI, samlSink) where

import Certs
import Config
import UITypes
import JassoState
import SinkCommon
import UICommon
import Logger
import Session

import Codec.Compression.Zlib.Raw (decompress)
import Control.Concurrent
import Control.Exception (catch)
import Control.Lens ((?~))
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Crypto.Cipher.Types
import qualified Crypto.PubKey.RSA.Types as RSA
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as BL64
import qualified Data.ByteString.Lazy as BL (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Either
import Data.List
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Maybe
import Data.Text (Text, pack)
import qualified Data.Text as T (intercalate, isPrefixOf, null)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock
import Data.Tree.NTree.TypeDefs
import Data.UUID.Types hiding (null)
import Data.X509 hiding (Extensions)
import Data.Yaml
import Data.Yaml.Config
import GHC.Generics (Generic)
import GHC.IO.Exception (IOException(..))
import Lucid hiding (svg_)
import Network.URI (nullURI, parseURI, parseURIReference)
import SAML2 hiding (entityID, ns)
import qualified SAML2 as SAML
import SAML2.Core.Signature
import SAML2.XML
import SAML2.XML.Canonical
import SAML2.XML.Schema.Datatypes (UnsignedShort)
import SAML2.XML.Signature
import Servant
import Servant.Multipart
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Random.Stateful
import Text.XML.HXT.Core hiding (app, getChildren, trace)

data BasePage     = BasePage String Text
data LoginPage    = DirectLogin String Text URI Text Text | LoginPage String Text Text URI URI Text deriving (Eq, Show, Generic)
data Login        = LoginBad | LoginGood Text deriving (Eq, Show, Generic)
data LoginRequest = LoginRequest { lrreqid :: Maybe Text, lrspid :: Maybe URI, lracsep :: Maybe URI, lruid :: Maybe Text, lrpasswd :: Maybe Text} deriving (Eq, Show)

instance ToJSON Login

instance FromMultipart Mem LoginRequest where
  fromMultipart multipartData = Right $
    LoginRequest (eitherToMaybe (lookupInput "reqid" multipartData))
                 (parseURI . txtToStr =<< eitherToMaybe (lookupInput "spid" multipartData))
                 (parseURI . txtToStr =<< eitherToMaybe (lookupInput "acsep" multipartData))
                 (eitherToMaybe $ lookupInput "uid" multipartData)
                 (eitherToMaybe $ lookupInput "passwd" multipartData)

type SamlAPI = "saml" :> Header "Accept-Language" String :>
               (    Get '[HTML] BasePage
               :<|> "Redirect" :> QueryParam "SAMLRequest" Text :> QueryParam "RelayState" Text :> QueryParam "SigAlg" Text
                               :> QueryParam "Signature" Text :> Header "cookie" Text :> Get '[HTML] LoginPage
               :<|> "Login" :> MultipartForm Mem LoginRequest :> Header "referer" Text :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] Login))

loginforms :: Monad m => String -> Text -> URI -> URI -> Text -> HtmlT m ()
loginforms lang reqid spid acsep rs = do
  form_ [name_ "login", id_ "login", onsubmit_ "dologin(); return false;"] $ do
    input_ [type_ "text",     name_ "uid",    id_ "uid",    placeholder_ $ tr lang "Username or Email Address", autofocus_]
    input_ [type_ "password", name_ "passwd", id_ "passwd", placeholder_ $ tr lang "Password"]
    input_ [type_ "hidden",   name_ "reqid",  id_ "reqid",  value_ reqid]
    input_ [type_ "hidden",   name_ "spid",   id_ "spid",   value_ $ pack $ show spid]
    input_ [type_ "hidden",   name_ "acsep",  id_ "acsep",  value_ $ pack $ show acsep]
    div_ [style_ "flex-flow: row-reverse wrap;"] $ input_ [type_ "submit", value_ "Login"]
  form_ [name_ "redirect", method_ "post", action_ $ pack $ show acsep, style_ "visibility: hidden;"] $ do
    input_ [type_ "hidden", name_ "SAMLResponse"]
    input_ [type_ "hidden", name_ "RelayState", id_ "rs", value_ rs]
    input_ [type_ "submit", style_ "visibility: hidden; position: absolute;"]

loginscript :: Text
loginscript = "const dologin = async() => {\n\
            \  document.login.elements['uid'].readOnly = true;\n\
            \  document.login.elements['passwd'].readOnly = true;\n\
            \  document.login.elements['uid'].style.cursor = 'wait';\n\
            \  document.login.elements['passwd'].style.cursor = 'wait';\n\
            \  document.body.style.cursor = 'wait';\n\
            \  const response = await fetch('Login', {method:'POST', body: new FormData(document.login)});\n\
            \  const myJson = await response.json();\n\
            \  if (myJson.tag === 'LoginGood') {\n\
            \    document.redirect.elements['SAMLResponse'].value = myJson.contents;\n\
            \    document.redirect.submit();\n\
            \  } else {\n\
            \    document.body.style.cursor = '';\n\
            \    document.login.elements['uid'].style.cursor = '';\n\
            \    document.login.elements['uid'].readOnly = false;\n\
            \    document.login.elements['passwd'].style.cursor = '';\n\
            \    document.login.elements['passwd'].readOnly = false;\n\
            \    document.login.elements['passwd'].focus();\n\
            \  }\n\
            \}\n"

instance ToHtml BasePage where
  toHtml (BasePage lang sitename) = doctypehtml lang $ do
    head__ True $ do
      title_ $ toHtmlRaw $ "Sign in" <> if T.null sitename then "" else " - " <> sitename
      script_ [defer_ "true"] loginscript
    main__ (do
      loginforms lang "" nullURI nullURI ""
      logo True)
      (script_ [defer_ "true"] ("document.login.elements['reqid'].value = history.state.reqid;\n\
                                \document.login.elements['spid'].value = history.state.spid;\n\
                                \document.redirect.action = document.login.elements['acsep'].value = history.state.acsep;\n\
                                \document.redirect.elements['rs'].value = history.state.rs;\n\
                                \\n" :: Text))
  toHtmlRaw = toHtml

instance ToHtml LoginPage where
  toHtml (DirectLogin lang sitename acsep rs r) = doctypehtml lang $ do
    head_ $ do
      title_ $ toHtmlRaw $ "Sign in" <> if T.null sitename then "" else " - " <> sitename
    body_ $ do
      main_ $ form_ [name_ "redirect", method_ "post", action_ $ pack $ show acsep, hidden_ "true"] $ do
        input_ [type_ "hidden", name_ "SAMLResponse", value_ r]
        input_ [type_ "hidden", name_ "RelayState", value_ rs]
        input_ [type_ "submit"]
      script_ "document.redirect.submit();"
  toHtml (LoginPage lang sitename reqid spid acsep rs) = doctypehtml lang $ do
    head__ True $ do
      meta_ [charset_ "utf-8"]
      title_ $ toHtmlRaw $ "Sign in" <> if T.null sitename then "" else " - " <> sitename
      script_ [defer_ "true"] $ loginscript <> "history.pushState({'reqid': '" <> reqid <> "', 'spid': '" <> pack (show spid) <>
                                                               "', 'acsep': '" <> pack (show acsep) <> "', 'rs': '" <> rs <> "'}, '', './');\n"
    main__ (loginforms lang reqid spid acsep rs >> logo True) mempty
  toHtmlRaw = toHtml

parseXML :: XmlPickler a => LogLevel -> (SysConfigList -> String -> IOStateArrow () XmlTree XmlTree) -> String -> IO [Either String a]
parseXML v readFunction u = fmap (map (unpickleDoc' xpickle)) $
  runX $ setErrorMsgHandler False (logItIO v Info) >>> readFunction [withCheckNamespaces yes, withRemoveWS yes] u >>> processBottomUp (processAttrl (neg isNamespaceDeclAttr))

sign :: (XmlPickler a, Signable a) => (a -> Maybe Signature) -> RSA.KeyPair -> SignedExact Certificate -> a -> IO a
sign getSig sk x509cert m = do
  let sk' = SigningKeyRSA sk
  r <- generateReference Reference
    { referenceId = Nothing
    , referenceURI = Just nullURI{ uriFragment = '#':signedID m }
    , referenceType = Nothing
    , referenceTransforms = Just $ Transforms
      $  simpleTransform TransformEnvelopedSignature
      :| [simpleTransform (TransformCanonicalization $ CanonicalXMLExcl10 False)]
    , referenceDigestMethod = simpleDigest DigestSHA256
    , referenceDigestValue = error "sign: referenceDigestValue"
    } $ samlToDoc m
  s' <- generateSignature sk' $ maybe SignedInfo
    { signedInfoId = Nothing
    , signedInfoCanonicalizationMethod = simpleCanonicalization $ CanonicalXMLExcl10 False
    , signedInfoSignatureMethod = SignatureMethod
      { signatureMethodAlgorithm = Identified $ signingKeySignatureAlgorithm sk'
      , signatureMethodHMACOutputLength = Nothing
      , signatureMethod = []
      }
    , signedInfoReference = r :| []
    } signatureSignedInfo (getSig m)
  let s'' = s' { signatureKeyInfo = Just $ KeyInfo Nothing $ X509Data (X509Certificate x509cert :| []) :| []  }
  return $ signature' ?~ s'' $ m

mkAssertion :: LogLevel -> Text -> RSA.KeyPair -> SignedExact Certificate -> Text -> URI -> EntityID -> User -> UTCTime -> UTCTime -> UTCTime -> Bool -> [(Text, Text)] -> Handler Text
mkAssertion v eid privkey x509cert reqid spid acsep user now expiryTime sessionStart allowed rm = liftIO $ do
  let signAssertions = True  -- whether or not to sign assertions (MUST be True to meet spec)
  let signResponses  = False -- whether or not to sign the entire document, not just the assertions

  let entityid = Issuer $ simpleNameID NameIDFormatEntity $ txtToStr eid
  respid' <- applyAtomicGen uniform globalStdGen :: IO UUID
  let respid = filter (/= '-') $ "PIGLETID-" <> show respid'
  let proto = ProtocolType respid SAML20 now (Just acsep) (Identified ConsentUnspecified) (Just entityid) Nothing [] Nothing
  let resp statuscode = Response (StatusResponseType proto (Just $ txtToStr reqid) (Status statuscode Nothing Nothing))
  let subj = Just $ NotEncrypted $ IdentifierName $ simpleNameID NameIDFormatEmail $ txtToStr (mail user)
  let subjconf = SubjectConfirmation (Identified ConfirmationMethodBearer) Nothing
               $ Just $ SubjectConfirmationData Nothing (Just expiryTime) (Just acsep) (Just $ txtToStr reqid) Nothing [] []
  let attribs = mkAttrib "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/upn" . uuid :| 
               [mkAttrib "uid" . uid, mkAttrib "fullName" . fullName, mkAttrib "mail" . mail, mkGroupAttrib . groups
               ,mkAttrib "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/givenname" . givenName
               ,mkAttrib "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/surname" . familyName]
  let assertion = Assertion {assertionVersion = SAML20
    , assertionID = respid <> "0001"
    , assertionIssueInstant = now
    , assertionIssuer = entityid
    , assertionSignature = Nothing
    , assertionSubject = Subject subj [subjconf]
    , assertionConditions = Just $ Conditions (Just now) (Just expiryTime) [AudienceRestriction (Audience spid :| [])]
    , assertionAdvice = Nothing
    , assertionStatement =
      [ StatementAuthn $ AuthnStatement sessionStart Nothing (Just expiryTime) Nothing
        $ AuthnContext (parseURIReference "urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport") Nothing []
      , StatementAttribute $ AttributeStatement $ fmap ($ user) attribs]}
  resp' <- if signAssertions then NotEncrypted <$> sign assertionSignature privkey x509cert assertion
                             else return $ NotEncrypted assertion
  let resp'' = if allowed then resp (StatusCode StatusSuccess []) [resp'] else resp (StatusCode StatusResponder [Identified StatusRequestDenied]) []
  r <- if signResponses then liftIO $ signSAMLProtocol (SigningKeyRSA privkey) resp''
                        else return resp''
  logItIO v Info $ if allowed
    then "SAML: Assertion created for " <> pack (show spid) <> ": " <> uid user <> " (" <> fullName user <> " <" <> mail user <> ">) [" <> T.intercalate ", " (concatMap getPermissions $ groups user) <> "]"
    else "SAML: Negative assertion created for " <> pack (show spid) <> ": " <> uid user <> " (" <> fullName user <> " <" <> mail user <> ">) [" <> T.intercalate ", " (concatMap getPermissions $ groups user) <> "]" <> " (not in allow list)"
  return $ decodeUtf8 $ B64.encode $ BL.toStrict $ samlToXML r
  where
    hxtText :: Text -> [NTree XNode]
    hxtText s = [NTree (XText $ txtToStr s) []]
    mkAttrib :: XString -> Text -> PossiblyEncrypted SAML.Attribute
    mkAttrib k val = NotEncrypted $ Attribute k (Identified AttributeNameFormatBasic) Nothing [] [hxtText val]
    mkGroupAttrib :: [Text] -> PossiblyEncrypted SAML.Attribute
    mkGroupAttrib val = let val' = concatMap getPermissions val
                        in  NotEncrypted $ Attribute "memberOf" (Identified AttributeNameFormatBasic) Nothing [] (map hxtText val')
    getPermissions g = case lookup g rm of
                         Nothing -> []
                         Just r  -> [r]

server :: (BlockCipher c, JassoSource s) => JassoState -> c -> s -> [(EntityID, ([(UnsignedShort, (Binding, AnyURI))], Maybe Text, Maybe Text, Maybe [Text], [(Text, Text)]))] -> RSA.KeyPair -> SignedExact Certificate -> PublicKeys -> Server SamlAPI
server state' symkey src speps privkey x509cert _ lang = let lang' = bestLang lang in getBasePage lang' :<|> getLoginPage lang' :<|> getLogin lang'
  where
    conf = jassoConfig state'
    v = verbosity conf
    assertSecs' = assertSecs conf
    sessSecs = sessionSecs conf
    eid = "https://" <> domain conf <> "/"

    maybeThrow401' = maybeThrow401 "SAML" v

    getBasePage lang' = return $ BasePage lang' $ siteName conf

    getLoginPage lang' rq' rs' _sa _sig cookies = do
      rq                 <- maybeThrow401' "no rq" rq'
      let rs = fromMaybe "" rs'
      rqs                <- liftIO (parseXML v readString $ BLU.toString $ decompress $ BL64.decodeLenient $ BL.fromStrict $ encodeUtf8 rq)
      (reqid, rq1)       <- maybeThrow401' "no reqid" $ listToMaybe $ map (strToTxt . protocolID . requestProtocol . authnRequest &&& id) $ rights rqs
      spid               <- maybeThrow401' "no spid" $ parseURI . baseID . nameBaseID . issuer =<< protocolIssuer (requestProtocol $ authnRequest rq1)
      (eps, _, _, a, rm) <- maybeThrow401' "unknown sp" $ lookup spid speps
      acsep              <- maybeThrow401' "bad acsep" $ case authnRequestAssertionConsumerServiceURL $ authnRequestAssertionConsumerService rq1 of
                                                           Just acsep' -> if acsep' `elem` [acsepuri | (_, (BindingHTTPPOST, acsepuri)) <- eps]
                                                                            then Just acsep'
                                                                            else Nothing
                                                           Nothing     -> case flip lookup eps =<< authnRequestAssertionConsumingServiceIndex rq1 of
                                                                            Just (BindingHTTPPOST, acsepuri) -> Just acsepuri
                                                                            _                                -> case take 1 [acsepuri | (_, (BindingHTTPPOST, acsepuri)) <- eps] of
                                                                                                                  [acsepuri] -> Just acsepuri
                                                                                                                  _          -> Nothing
      sess <- checkSession src state' symkey assertSecs' cookies
      case sess of
        Just (u, n, et, ss) -> do
          let allowed = allowAccess u a
          liftIO $ logItIO v Debug $ "SAML: Creating direct login with u " <> show u <> ", n " <> show n <> ", et " <> show et <> ", ss " <> show ss <> if allowed then "" else ", but not in access list"
          DirectLogin lang' (siteName conf) acsep rs <$> mkAssertion v eid privkey x509cert reqid spid acsep u n et ss allowed rm
        Nothing -> do
          liftIO $ logItIO v Debug $ "SAML: Creating login page with lang " <> lang' <> ", reqid " <> show reqid <> ", spid " <> show spid <> ", acsep " <> show acsep <> ", and rs " <> show rs
          return $ LoginPage lang' (siteName conf) reqid spid acsep rs

    jsonerror s = "{\"tag\": \"LoginBad\", \"contents\": \"" <> s <> "\"}"
    jsonerror' = maybeThrow401' . jsonerror

    getLogin _ (LoginRequest reqid' spid' acsep' uid' passwd') referer' = do
      reqid            <- jsonerror' "missing reqid" reqid'
      spid             <- jsonerror' "missing spid" spid'
      (_, _, _, a, rm) <- maybeThrow401' "unknown sp" $ lookup spid speps
      acsep            <- jsonerror' "missing acsep" acsep'
      uid''            <- jsonerror' "bad uid" uid'
      passwd           <- jsonerror' "bad uid" passwd' -- purposefully misleading error messages
      referer          <- jsonerror' "bad uid" referer'
      if eid `T.isPrefixOf` referer
        then return ()
        else throw401 "SAML" v "Bad referer" (jsonerror "bad uid")
      userM <- liftIO $ hoistJassoT state' $ verifyUser src uid'' passwd
      case userM of
        Just (user, token) -> mkSession symkey assertSecs' sessSecs token $ \n et -> do
          let allowed = allowAccess user a
          liftIO $ logItIO v Debug $ "SAML: Login good for " <> uid user <> if allowed then "" else ", but not in access list"
          LoginGood <$> mkAssertion v eid privkey x509cert reqid spid acsep user n et n allowed rm
        _         -> do
          delayms <- randomRIO (5000000, 9000000) -- TODO: record failed login attempts by username and IP address for rate-limiting purposes
          liftIO $ threadDelay delayms
          return $ noHeader LoginBad

data SPConf = SPConf { spEntityID :: Text, spACS :: Text, name :: Maybe Text, url :: Maybe Text, allow :: Maybe [Text], mapping :: Maybe [RoleMap] } deriving (Show, Eq, Generic)
instance FromJSON SPConf

parseURI' :: Text -> Maybe URI
parseURI' = parseURI . txtToStr

getSPEPs :: MonadIO m => LogLevel -> FilePath -> m [(EntityID, ([(UnsignedShort, (Binding, AnyURI))], Maybe Text, Maybe Text, Maybe [Text], [(Text, Text)]))]
getSPEPs v spDir = liftIO $ do
  catch (do
    spdirContents <- getDirectoryContents spDir
    let spxmls = filter (".xml" `isSuffixOf`) spdirContents
    spxml <- concat <$> mapM getEPxmls spxmls
    let spconfs = filter (".conf" `isSuffixOf`) spdirContents
    spconf <- mapM getEPconfs spconfs
    return $ concat spxml ++ concat spconf) ioCatcherD
    where
      getEPconfs filename = getEPconf =<< loadYamlSettings [spDir </> filename] [] useEnv
        where
          getEPconf (SPConf eid acsep n u a rm) = case (parseURI' eid, parseURI' acsep) of
            (Just eiduri, Just acsepuri) -> return [(eiduri, ([(0, (BindingHTTPPOST, acsepuri))], n, u, a, getRMs rm))]
            _ -> do
              logItIO v Error $ "SAML: Couldn't parse " <> spDir </> filename <> ", ignoring"
              return []
          getRMs Nothing  = []
          getRMs (Just rms) = map (\(RoleMap r p) -> (r, p)) rms

      getEPxmls filename = mapM getEP =<< parseXML v readDocument (spDir </> filename)
        where
          getEP (Left er) = do
            logItIO v Error $ "SAML: Couldn't parse " <> spDir </> filename <> ", ignoring"
            logItIO v Debug er
            return []
          getEP (Right ed) = return [(myEID, (myACSEPs, Nothing, Nothing, Nothing, []))]
            where
              myDescriptors = toList $ descriptors $ entityDescriptors ed
              myACSEPs = concatMap (mapMaybe translateEndpoint . toList . descriptorAssertionConsumerService) myDescriptors
              myEID = SAML.entityID ed

              translateEndpoint (IndexedEndpoint Endpoint{endpointBinding = Identified bt, endpointLocation = epl} i _) = Just (i, (bt, epl))
              translateEndpoint _ = Nothing
      ioCatcherD :: IOError -> IO [a]
      ioCatcherD e = do
        logItIO v Error $ "SAML: Couldn't read SP config directory" <> maybe "" (\s -> " \"" <> s <> "\"") (ioe_filename e) <> ": " <> show (ioe_type e) <> " (" <> ioe_description e <> "), ignoring"
        return []

samlSink :: JassoSource s => s -> JassoIO (Server SamlAPI)
samlSink jassoSource = do
  logIt Normal ("SAML: Starting" :: String)
  conf <- getConf
  (symkey, _) <- getSymKey
  (privkey, x509cert) <- getKeyAndCert
  state' <- get
  speps <- getSPEPs (verbosity conf) $ (varDir conf) </> "saml"
  case speps of
    [] -> logIt Warning ("SAML: No SPs configured" :: String)
    _  -> do
      logIt Info  $ "SAML: Configured SPs: " <> T.intercalate ", " (map desc speps)
      logIt Debug $ "SAML: Configured SPs: " <> T.intercalate ", " (map (pack . show) speps)
  return $ server state' symkey jassoSource speps privkey x509cert undefined
  where
    desc (e, (_, n, _, _, _)) = case n of
                                  Nothing -> pack $ show e
                                  Just n' -> n'
