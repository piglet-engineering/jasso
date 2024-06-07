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

module Logger(LogLevel(..), logIt, logIt', logItIO, loggerInit) where

import JassoState(JassoT, JassoState(jassoConfig))
import ConfigTypes(JassoConfig(verbosity))
import LogTypes
import StdoutLogger

import Control.Monad(when)
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Monad.Trans.State(get)

loggerInit :: MonadIO m => m ()
loggerInit = stdoutLoggerInit

logItIO :: (MonadIO m, JassoLogger a, StdoutLogger a) => LogLevel -> LogLevel -> a -> m ()
logItIO i j x = when (i <= j) (logItRaw stdoutLogger j x)

logIt :: (MonadIO m, JassoLogger a, StdoutLogger a) => LogLevel -> a -> JassoT m ()
logIt j x = do
  conf <- jassoConfig <$> get
  when (verbosity conf <= j) (liftIO $ logItRaw stdoutLogger j x)

logIt' :: (Show s, MonadIO m) => LogLevel -> s -> JassoT m ()
logIt' j = logIt j . show
