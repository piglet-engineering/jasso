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

{-# LANGUAGE OverloadedStrings #-}

module Ldap(ldapSource) where

import Config
import UITypes
import JassoState
import Logger

import Control.Monad(foldM)
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Monad.Trans.State(get)
import Data.List(isSuffixOf, sort)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Maybe(listToMaybe, mapMaybe, catMaybes)
import qualified Data.Set as Set
import Data.Text(Text, unpack)
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8, encodeUtf8)
import Data.ByteString.UTF8(ByteString)
import Ldap.Client
import Ldap.Client.Bind
import Ldap.Client.Search

ldapSource :: JassoIO JassoState
ldapSource = do
  state <- get
  logIt Normal ("LDAP: Starting" :: String)
  let conf = ldap $ jassoConfig state
  logIt Info $ "LDAP: Configured: " <> host conf <> ":" <> show (port conf) <> ":" <> unpack (baseDn conf)
  return state

with'' :: MonadIO m => p -> (Ldap -> JassoT IO b) -> JassoT m b
with'' _ f = do
  state <- get
  let ldapconf = ldap $ jassoConfig state
  liftIO $ with' (Plain $ host ldapconf) (fromIntegral $ port ldapconf) $ hoistJassoT state . f

instance JassoSource JassoState where
  verifyUser    state u p = with'' state $ userBind u p
  verifyUser'   state t   = with'' state $ userBindT t
  getUser       state u   = with'' state $ getUser' u
  getUserByUUID state u   = with'' state $ getUserByUUID' u
  getUsers      state     = with'' state   getUsers'
  getGroup      state g   = with'' state $ getGroup' g
  getGroups     state     = with'' state   getGroups'

getUser' :: Text -> Ldap -> JassoIO (Maybe User)
getUser' u l = userBindM Nothing l (return ()) $ do
  conf <- getConf
  us <- liftIO $ search l (udn conf) mempty (And (Attr "ObjectClass" := "inetOrgPerson" :| [Attr "uid" := encodeUtf8 u])) [Attr "+", Attr "*"]
  attrs <- mapM (extractUserAttrs l) us
  return $ listToMaybe $ catMaybes attrs

getUserByUUID' :: Text -> Ldap -> JassoIO (Maybe User)
getUserByUUID' u l = do
  logIt Debug $ "LDAP: getUserByUUID: " <> u
  conf <- getConf
  us <- liftIO $ searchEither l (udn conf) mempty (And (Attr "ObjectClass" := "inetOrgPerson" :| [Attr "entryUUID" := encodeUtf8 u])) [Attr "+", Attr "*"]
  case us of
    Left (ResponseInvalid rq rs) -> do
      logIt Error $ "LDAP: rq: " <> show rq <> "\nrs: " <> show rs
      return Nothing
    Left (ResponseErrorCode rq c dn msg) -> do
      logIt Error $ "LDAP: rq: " <> show rq <> "\ncode: " <> show c <> " dn: " <> show dn <> ": " <> show msg
      return Nothing
    Right [] -> do
      logIt Error $ "LDAP: No results for UUID " <> u
      return Nothing
    Right results -> do
      logIt Debug $ "LDAP: Returning results for UUID " <> show u <> " " <> show results
      attrs <- mapM (extractUserAttrs l) results
      return $ listToMaybe $ catMaybes attrs

getUsers' :: Ldap -> JassoIO [User]
getUsers' l = userBindM [] l (return ()) $ do
  conf <- getConf
  us <- liftIO $ search l (udn conf) mempty (Attr "ObjectClass" := "inetOrgPerson") [Attr "+", Attr "*"]
  attrs <- mapM (extractUserAttrs l) us
  return $ catMaybes attrs

getGroup' :: Text -> Ldap -> JassoIO (Maybe Group)
getGroup' g l = userBindM Nothing l (return ()) $ do
  conf <- getConf
  gs <- liftIO $ groupSearch conf g l
  return $ listToMaybe $ mapMaybe extractGroupAttrs gs

gdn :: JassoConfig -> Dn
gdn conf = Dn (baseDn $ ldap conf) +: groupsDn (ldap conf)

getGroups' :: Ldap -> JassoIO [Group]
getGroups' l = userBindM [] l (return ()) $ do
  conf <- getConf
  gs <- liftIO $ search l (gdn conf) mempty (Attr "ObjectClass" := "groupOfNames") [Attr "+", Attr "*"]
  return $ mapMaybe extractGroupAttrs gs

lookupAttr :: Text -> [(Attr, [ByteString])] -> Maybe Text
lookupAttr s xs = case lookup (Attr s) xs of
                     Just (x:_) -> Just $ decodeUtf8 x
                     _          -> Nothing

extractUserAttrs :: Ldap -> SearchEntry -> JassoIO (Maybe User)
extractUserAttrs l (SearchEntry (Dn userDn) as) = do
    (Dn udn') <- udn <$> getConf
    case extractUserAttrs' udn' of
      Just ([rdn'], u) | "ou=" `T.isPrefixOf` rdn' -> do
        (SearchEntry _ r:_) <- liftIO $ search l (Dn udn') mempty (And (Attr "ObjectClass" := "OrganizationalUnit" :| [Attr "ou" := encodeUtf8 (T.drop 3 rdn')])) [Attr "description"]
        let u' = u $ case lookupAttr "description" r of
                       Just desc' -> desc'
                       _          -> ""
        Just . u' . sort . fst <$> getRoles ([], Set.empty) as
      Just (_, u)                 -> return $ Just $ u "" []
      _                           -> return Nothing
  where
    extractUserAttrs' udnbase = do
      uid'       <- lookupAttr "uid" as
      uuid'      <- lookupAttr "entryUUID" as
      fullName'  <- lookupAttr "cn" as
      mail'      <- lookupAttr "mail" as
      givenName' <- lookupAttr "givenName" as
      sn         <- lookupAttr "sn" as

      let rdn = drop 1 $ T.split (==',') userDn
      let udnbase' = T.split (==',') udnbase
      let ou' = T.intercalate "/" $ reverse $ mapMaybe (getOU . T.split (=='=')) $ if udnbase' `isSuffixOf` rdn then take (length rdn - length udnbase') rdn else []
      return (take 1 rdn, User uuid' uid' fullName' givenName' sn mail' ou')
    getRoles :: ([Role], Set.Set Text) -> AttrList [] -> JassoIO ([Role], Set.Set Text)
    getRoles (rs, seen) as' = do
      let groups' = filter (`Set.notMember` seen) $ concatMap (mapMaybe (getTL . decodeUtf8) . snd) $ filter ((== Attr "memberOf") . fst) as'
      let seen' = seen `Set.union` Set.fromList groups'
      (rs', seen'') <- foldM getSubRoles (rs, seen') groups'
      return (rs', seen'')

    getSubRoles :: ([Role], Set.Set Text) -> Text -> JassoIO ([Role], Set.Set Text)
    getSubRoles (rs, seen) gr = do
      gdn' <- gdn <$> getConf
      gs <- liftIO $ search l gdn' mempty (And (Attr "ObjectClass" := "groupOfNames" :| [Attr "cn" := encodeUtf8 gr])) [Attr "memberOf"]
      let as' = concatMap (\(SearchEntry _ r) -> r) gs
      getRoles (Role gr (mapMaybe (getTL . decodeUtf8) $ concatMap snd $ filter ((== Attr "memberOf") . fst) as'):rs, seen) as'
    getOU ["ou", value] = Just value
    getOU _             = Nothing

extractGroupAttrs :: SearchEntry -> Maybe Group
extractGroupAttrs (SearchEntry _ as) = do
  cn'          <- lookupAttr "cn" as
  description' <- lookupAttr "description" as
  let subs = concatMap (map decodeUtf8 . snd) $ filter ((== Attr "member") . fst) as
  let users' = mapMaybe getTL $ filter ("uid=" `T.isPrefixOf`) subs
  let groups' = mapMaybe getTL $ filter ("cn=" `T.isPrefixOf`) subs
  return $ Group cn' description' users' groups'

userSearch :: JassoConfig -> Text -> Ldap -> IO [SearchEntry]
userSearch conf uid' ldap' = let udn' = udn conf
                             in  (++) <$> search ldap' udn' mempty (And (Attr "ObjectClass" := "inetOrgPerson" :| [Attr "uid"  := encodeUtf8 uid'])) [Attr "+", Attr "*"]
                                      <*> search ldap' udn' mempty (And (Attr "ObjectClass" := "inetOrgPerson" :| [Attr "mail" := encodeUtf8 uid'])) [Attr "+", Attr "*"]
                                      -- TODO: use searchEither throughout, especially as some errors can be misleading

groupSearch :: JassoConfig -> Text -> Ldap -> IO [SearchEntry]
groupSearch conf cn ldap' = search ldap' (gdn conf) mempty (And (Attr "ObjectClass" := "groupOfNames" :| [Attr "cn" := encodeUtf8 cn])) [Attr "+", Attr "*"]

(+:) :: Dn -> Text -> Dn
(Dn x) +: s = Dn (s <> "," <> x)

udn :: JassoConfig -> Dn
udn conf = let lc = ldap conf in Dn (baseDn lc) +: usersDn lc

getTL :: Text -> Maybe Text
getTL s = listToMaybe (T.splitOn "," s) >>= (listToMaybe . drop 1 . T.splitOn "=")

userBindM :: a -> Ldap -> JassoIO () -> JassoIO a -> JassoIO a
userBindM d ldap' e f = do
  sess <- session <$> get
  case stoken <$> sess of
    Just (LDAPToken dn ps) -> userBind' d (Dn dn) ps ldap' e f
    _ -> return d

userBind' :: a -> Dn -> Text -> Ldap -> JassoIO () -> JassoIO a -> JassoIO a
userBind' d userDn passwd ldap' e f = do
  bindResult <- liftIO $ bindEither ldap' userDn (Password $ encodeUtf8 passwd)
  case bindResult of
    Right () -> do
      logIt Debug $ "LDAP: Bind successful: " ++ show userDn
      f
    Left rerr -> do
      logIt Debug $ "LDAP: Bind failed: " ++ show userDn ++ ", " ++ show rerr
      _ <- e
      return d

userBindT :: Token -> Ldap -> JassoIO (Maybe User)
userBindT NoToken           _     = return Nothing
userBindT (LDAPToken dn ps) ldap' = do
  conf <- getConf
  let v = verbosity conf
  se <- liftIO $ search ldap' (Dn dn) (scope BaseObject) (Present $ Attr "uid") [Attr "+", Attr "*"]
  fmap fst <$> bindFromSE ldap' v se ps dn

userBind :: Text -> Text -> Ldap -> JassoIO (Maybe (User, Token))
userBind uid' passwd ldap' = do
  conf <- getConf
  let v = verbosity conf
  se <- liftIO $ userSearch conf uid' ldap'
  bindFromSE ldap' v se passwd uid'

bindFromSE :: Ldap -> LogLevel -> [SearchEntry] -> Text -> Text -> JassoIO (Maybe (User, Token))
bindFromSE ldap' v se passwd usertext = do
  case se of
    (SearchEntry userDn as:_) -> do
      listToMaybe <$> userBind' [] userDn passwd ldap' (logItIO v Detail $ "LDAP: Authentication failed: " <> usertext) (do
          r <- extractUserAttrs ldap' $ SearchEntry userDn as
          let Dn dn = userDn
          case r of
            Nothing -> do
              logIt Detail $ "LDAP: Authentication error: " <> show userDn
              logIt Debug  $ "LDAP: Couldn't extract attributes: " <> show userDn
              return []
            Just r' -> do
              logIt Info  $ "LDAP: Authentication successful: " <> uid r'
              logIt Debug $ show as
              return [(r', LDAPToken dn passwd)])
    _ -> do
      logItIO v Detail $ "LDAP: Unknown user: " <> usertext
      return Nothing
