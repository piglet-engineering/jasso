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

module Certs(getKeyAndCert) where

import JassoState
import Config
import Logger

import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (get, put)
import Crypto.PubKey.RSA.Types
import qualified Data.ByteString.Lazy as BSL
import Data.PEM
import Data.X509 hiding (Extensions)
import Data.X509.File
import GHC.IO.Exception (IOException(..))
import System.Exit (ExitCode(..), exitWith)

readCert :: String -> IO (Either String SignedCertificate)
readCert fn = do
  certfilebytes <- BSL.readFile fn
  return $ decodeSignedCertificate . pemContent =<< listToEither "No PEM data found" =<< pemParseLBS certfilebytes
    where
      listToEither s v = case v of
                           []    -> Left s
                           (x:_) -> Right x

getKeyAndCert :: JassoIO (KeyPair, SignedCertificate)
getKeyAndCert = do
  JassoState conf keycert symkey sess <- get
  case keycert of
    Just kc -> return kc
    Nothing -> do
      let varDir' = varDir conf
      let v = verbosity conf
      let privkeyfile = getFullPath varDir' $ privKey conf
      privkeys <- liftIO $ catch (readKeyFile privkeyfile) $ ioCatcher v privkeyfile
      case privkeys of
        [PrivKeyRSA pk] -> do
          let privkey = KeyPair pk
          logIt Info $ "Certs: Loaded private key from " <> privkeyfile
          let certfile = getFullPath varDir' $ pubCert conf
          cert' <- liftIO $ readCert certfile
          case cert' of
            Right x509cert -> do
              logIt Info $ "Certs: Loaded server certificate from " <> certfile
              put $ JassoState conf (Just (privkey, x509cert)) symkey sess
              return (privkey, x509cert)
            Left e -> do
              logIt Critical $ "Certs: Couldn't read certificate file " <> certfile <> "  " <> show e
              logIt Critical ("Certs: Unfortunately, I'm not clever enough to create a new one for you safely, so this is the end of the line for us I'm afraid" :: String)
              logIt Critical ("Certs: You should be able to create one yourself with openssl" :: String)
              liftIO $ exitWith $ ExitFailure 1
        _ -> do
          logIt Critical $ "Certs: Couldn't read private key file " <> privkeyfile
          logIt Critical ("Certs: Unfortunately, I'm not clever enough to create a new one for you safely, so this is the end of the line for us I'm afraid" :: String)
          logIt Critical $ "Certs: You should be able to create one yourself with the following command: openssl genrsa -out " <> privkeyfile <> " 2048"
          liftIO $ exitWith $ ExitFailure 1
  where
    ioCatcher :: LogLevel -> String -> IOError -> IO [a]
    ioCatcher v fn e = do
      logItIO v Critical $ "Certs: Couldn't read from private key file" <> maybe "" (\s -> " \"" <> s <> "\"") (ioe_filename e) <> ": " <> show (ioe_type e) <> " (" <> ioe_description e <> ")"
      logItIO v Critical ("Certs: Unfortunately, I'm not clever enough to create a new one for you safely, so this is the end of the line for us I'm afraid" :: String)
      logItIO v Critical $ "Certs: You should be able to create one yourself with the following command: openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:4096 -out " <> fn
      exitWith $ ExitFailure 1
