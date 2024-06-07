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

module OidcTypes (UserInfo(..)) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data UserInfo = UserInfo { sub :: Text, preferred_username :: Text, name :: Text, given_name :: Text, family_name :: Text, email :: Text, email_verified :: Text, groups :: [Text], roles :: [Text]
                         , permissions :: [Text], organization_id :: Text, organization_name :: Text } deriving (Eq, Show, Generic)
instance ToJSON UserInfo
