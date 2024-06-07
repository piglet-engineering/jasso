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

{-# LANGUAGE TemplateHaskell, DeriveGeneric, DataKinds, TypeOperators, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module UITypes(eitherToMaybe, Role(..), User(..), groups, Group(..), Status(..), MFAStatus(..), Token(..), UISPA(..), UIAPI'', UIAPI''', HTML) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Elm.Derive (defaultOptions, deriveBoth)
import GHC.Generics (Generic)
import Lucid hiding (svg_)
import Network.HTTP.Media ((//), (/:))
import Servant

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

data Role = Role { rolename :: Text, subroles :: [Text] } deriving (Show, Read, Eq, Generic, Ord)
deriveBoth defaultOptions ''Role

data User = User { uuid :: Text, uid :: Text, fullName :: Text, givenName :: Text, familyName :: Text, mail :: Text, ouId :: Text, ouName :: Text, roles :: [Role] } deriving (Show, Read, Eq, Generic)
deriveBoth defaultOptions ''User

groups :: User -> [Text]
groups = map rolename . roles

data Group = Group { gid :: Text, description :: Text, users :: [Text], subgroups :: [Text] } deriving (Show, Eq, Generic)
deriveBoth defaultOptions ''Group

data UISPA = NotAuthenticated String Text | UISPA String Text
deriveBoth defaultOptions ''UISPA

data Token = NoToken | LDAPToken Text Text deriving (Show, Eq, Read, Generic)
instance FromJSON Token where
  parseJSON _ = return NoToken
instance ToJSON Token where
  toJSON NoToken = Null
  toJSON (LDAPToken dn _) = String dn

data Status = SOK | SErr Text deriving (Show, Eq, Generic)
deriveBoth defaultOptions ''Status

data MFAStatus = MFAStatus { mfaEmailEnabled :: Bool, mfaTotpEnabled :: Bool, mfaTotpImg :: String } deriving (Show, Eq, Generic)
deriveBoth defaultOptions ''MFAStatus

data HTML
instance Accept HTML                   where contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML BL.ByteString where mimeRender _ = id
instance MimeRender HTML (Html a)      where mimeRender _ = renderBS
instance {-# OVERLAPPABLE #-} ToHtml a => MimeRender HTML a where mimeRender _ = renderBS . toHtml

type UIAPI'' =
       "user"                         :> Get '[JSON] [User]
  :<|> "user"  :> "me"                :> Get '[JSON] User
  :<|> "user"  :> "mfa"               :> Get '[JSON] MFAStatus
  :<|> "user"  :> CaptureAll "u" Text :> Get '[JSON] User
  :<|> "group"                        :> Get '[JSON] [Group]
  :<|> "group" :> CaptureAll "g" Text :> Get '[JSON] Group
  :<|> "logout"                       :> Get '[JSON] ()

type UIAPI''' =
       "user"                         :> Get '[JSON] [User]
  :<|> "user"  :> "me"                :> Get '[JSON] User
  :<|> "user"  :> "mfa"               :> Get '[JSON] MFAStatus
  :<|> "user"  :> Capture "u" Text    :> Get '[JSON] User
  :<|> "group"                        :> Get '[JSON] [Group]
  :<|> "group" :> Capture "g" Text    :> Get '[JSON] Group
  :<|> "logout"                       :> Get '[JSON] ()
