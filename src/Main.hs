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

{-# LANGUAGE TypeOperators, OverloadedStrings, TemplateHaskell #-}

import Paths_jasso(version)

import Certs
import Config
import JassoState
import UI
import Saml
import Oidc
import Ldap
import Session
import Logger

import qualified Data.ByteString.UTF8 as BSU (fromString)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (evalStateT)
import Data.Default (Default(..))
import Data.Proxy ()
import Data.Time (getCurrentTime)
import Data.Version (showVersion)
import Language.Haskell.TH (runIO, stringE)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setServerName, setPort)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, simpleCorsResourcePolicy)
import Servant
import System.Directory
import System.Environment
import System.Exit

type API = UIAPI :<|> SamlAPI :<|> OidcAPI

buildTime :: String
buildTime = $(stringE =<< runIO (show `fmap` getCurrentTime))

fullVersion :: String
fullVersion = showVersion version ++ ", built " ++ buildTime -- FIXME: Use commit ID or last modified date instead of build time (#reproduciblebuilds)

usage :: Int -> JassoIO ()
usage exitcode = do
  pn <- liftIO getProgName
  logIt Info $ "Jasso Single Sign-On from Piglet Engineering, version " ++ fullVersion
  logIt Info $ "Usage: " ++ pn ++ " conffile"
  liftIO $ exitWith $ ExitFailure exitcode

showversion :: JassoIO ()
showversion = do
  logIt Info $ "Jasso " ++ fullVersion
  liftIO $ exitWith $ ExitFailure 0

main :: IO ()
main = flip evalStateT def $ do
  loggerInit
  pwd <- liftIO getCurrentDirectory
  logIt Normal $ "Jasso Single Sign-On from Piglet Engineering: starting\nVersion " <> fullVersion <> "\nRunning from " <> pwd
  args <- liftIO getArgs
  conffile <- case args of
    []             -> return Nothing
    ["-h"]         -> void' $ usage 0
    ["--help"]     -> void' $ usage 0
    ["-v"]         -> void' showversion
    ["--version"]  -> void' showversion
    [cf]           -> return $ Just cf
    _              -> do
      logIt Critical ("Unknown option" :: String)
      void' $ usage 1
  conf <- loadConf conffile
  _ <- getSymKey
  _ <- getKeyAndCert
  src <- ldapSource
  ui <- uiServer src
  samlsink <- samlSink src
  oidcsink <- oidcSink src
  let cors' = cors (const $ Just $ simpleCorsResourcePolicy { corsRequestHeaders = ["content-type", "authorization", "x-okta-user-agent-extended"]}) -- TODO: should be moved to the modules
  liftIO $ runSettings (setPort (listen conf) $ setServerName ("jasso/" <> BSU.fromString fullVersion) defaultSettings) $ cors' $ serve (Proxy :: Proxy API) $ ui :<|> samlsink :<|> oidcsink
  where
    void' = (const Nothing <$>)
