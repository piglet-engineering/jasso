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

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module StdoutLogger(StdoutLogger(..), stdoutLoggerInit) where

import LogTypes (LogLevel)

import System.IO (hSetBuffering, stdout, BufferMode(..))
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)

stdoutLoggerInit :: MonadIO m => m ()
stdoutLoggerInit = liftIO $ hSetBuffering stdout LineBuffering

class StdoutLogger a where
  stdoutLogger :: a -> a -> LogLevel -> a -> IO ()

instance StdoutLogger String where
  stdoutLogger dt pn i y = putStrLn (pn ++ ": " ++ dt ++ " " ++ show i ++ ": " ++ y)

instance StdoutLogger T.Text where
  stdoutLogger dt pn i y = T.putStrLn (pn <> ": " <> dt <> " " <> T.pack (show i) <> ": " <> y)
