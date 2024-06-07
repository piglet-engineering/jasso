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

{-# LANGUAGE DeriveGeneric #-}

module ConfigTypes(JassoConfig(..), SamlConfig(..), OidcConfig(..), LdapConfig(..), RoleMap(..), RoleMapConfig(..)) where

import LogTypes

import GHC.Generics(Generic)
import Data.Aeson(FromJSON(..), ToJSON(..))
import Data.Time.Clock(NominalDiffTime)
import Data.Text(Text)

data RoleMap = RoleMap { role :: Text, permission :: Text } deriving (Show, Eq, Generic)
instance FromJSON RoleMap
instance ToJSON RoleMap

newtype RoleMapConfig = RoleMapConfig [RoleMap] deriving (Show, Eq, Generic)
instance FromJSON RoleMapConfig
instance ToJSON RoleMapConfig

data SamlConfig = SamlConfig { } deriving (Show, Eq, Generic)
instance FromJSON SamlConfig
instance ToJSON SamlConfig

data OidcConfig = OidcConfig { } deriving (Show, Eq, Generic)
instance FromJSON OidcConfig
instance ToJSON OidcConfig

data LdapConfig = LdapConfig { host :: String, port :: Int, baseDn :: Text, usersDn :: Text, groupsDn :: Text } deriving (Show, Eq, Generic)
instance FromJSON LdapConfig
instance ToJSON LdapConfig

data JassoConfig = JassoConfig { config :: String, verbosity :: LogLevel, siteName :: Text, domain :: Text, listen :: Int, varDir :: String, sessionSecs :: NominalDiffTime
                               , assertSecs :: NominalDiffTime, privKey :: String, pubCert :: String , ldap :: LdapConfig } deriving (Show, Eq, Generic)
instance FromJSON JassoConfig
instance ToJSON JassoConfig
