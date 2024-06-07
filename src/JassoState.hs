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

module JassoState(JassoSource(..), Session(..), JassoState(..), JassoT, JassoIO, hoistJassoT) where

import ConfigTypes
import LogTypes
import UITypes

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State (StateT, evalStateT)
import Crypto.Cipher.Blowfish (Blowfish448)
import Crypto.PubKey.RSA.Types
import qualified Data.ByteString.Char8 as BSC (ByteString)
import Data.Default (Default(..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.X509 hiding (Extensions)

data Session = Session { suser :: User, stoken :: Token, slogintime :: UTCTime, sexpiry :: UTCTime } deriving (Eq, Show, Read)

instance Show Blowfish448 where
  show _ = "symkey"

data JassoState = JassoState { jassoConfig :: JassoConfig, keyAndCert :: Maybe (KeyPair, SignedExact Certificate), symKey :: Maybe (Blowfish448, BSC.ByteString), session :: Maybe Session } deriving Show
type JassoT m   = StateT JassoState m
type JassoIO    = JassoT IO

hoistJassoT :: Monad m => JassoState -> JassoT m a -> m a
hoistJassoT = flip evalStateT

class JassoSource src where
  verifyUser    :: MonadIO m => src -> Text  -> Text -> JassoT m (Maybe (User, Token))
  verifyUser'   :: MonadIO m => src -> Token -> JassoT m (Maybe User)
  getUser       :: MonadIO m => src -> Text  -> JassoT m (Maybe User)
  getUserByUUID :: MonadIO m => src -> Text  -> JassoT m (Maybe User)
  getUsers      :: MonadIO m => src -> JassoT m [User]
  getGroup      :: MonadIO m => src -> Text  -> JassoT m (Maybe Group)
  getGroups     :: MonadIO m => src -> JassoT m [Group]

instance Default JassoState where
  def = JassoState (JassoConfig "/etc/jasso.conf" Normal "" "" 8181 "/var/lib/jasso" 2419200 3600 "server.key" "server.crt" (LdapConfig "localhost" 389 "" "ou=users" "ou=groups")) Nothing Nothing Nothing
