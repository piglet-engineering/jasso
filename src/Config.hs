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

{-# LANGUAGE CPP #-}

module Config(JassoConfig(..), SamlConfig(..), OidcConfig(..), LdapConfig(..), RoleMap(..), RoleMapConfig(..), loadConf, getConf, getFullPath) where

import ConfigTypes
import JassoState
import Logger

import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.State(get, put)
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap(unionWith)
#else
import Data.HashMap.Strict(unionWith)
#endif
import Data.Yaml
import Data.Yaml.Config(applyEnvValue, getCurrentEnv)
import System.Environment
import System.FilePath((</>))

mergeValues :: Value -> Value -> Value
mergeValues (Object x) (Object y) = Object $ unionWith mergeValues x y
mergeValues _ x = x

getConf :: Monad m => JassoT m JassoConfig
getConf = jassoConfig <$> get

loadConf :: Maybe String -> JassoIO JassoConfig
loadConf conffile = do
  JassoState defaultConfig keycert symkey sess <- get
  conf <- liftIO $ lookupEnv "JASSOCONF"
  let myconf = case conffile of
                 Just cf -> cf
                 _       -> case conf of
                              Just conf' -> conf'
                              _          -> config defaultConfig
  c0 <- liftIO $ decodeFileEither myconf
  let c1 = either (Left . prettyPrintParseException) Right $ mergeValues (toJSON defaultConfig) <$> c0
  env <- liftIO getCurrentEnv
  let c2 = applyEnvValue False env <$> c1

  case parseEither parseJSON =<< c2 of
    Left e  -> do
      logIt Error $ "Config: Continuing with default config after parsing of configuration failed:\n" ++ e
      return defaultConfig
    Right c3 -> do
      put $ JassoState c3 keycert symkey sess
      logIt Debug $ "Config: " <> show c3
      return c3

getFullPath :: FilePath -> String -> String
getFullPath parent dir = case dir of
  dir'@('/':_) -> dir'      -- if an abosulte path is given, honour it
  dir' -> parent </> dir'   -- a relative path is within the given parent dir
