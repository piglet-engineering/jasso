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

{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}

module LogTypes(LogLevel(..), JassoLogger(..)) where

import GHC.Generics(Generic)
import Control.Monad.IO.Class(liftIO, MonadIO)
import Data.Aeson(FromJSON(..), ToJSON(..))
import qualified Data.Text as T
import Data.Time.Clock(getCurrentTime)
import Data.Time.Format(formatTime, defaultTimeLocale)
import System.Environment(getProgName)

data LogLevel = Debug | Detail | Info | Normal | Warning | Error | Alert | Critical | Emergency deriving (Show, Eq, Read, Ord, Bounded, Enum, Generic)
instance FromJSON LogLevel
instance ToJSON LogLevel

class JassoLogger a where
  logItRaw :: MonadIO m => (a -> a -> LogLevel -> a -> IO ()) -> LogLevel -> a -> m ()

instance JassoLogger String where
  logItRaw logger i x = do
    dt <- formatTime defaultTimeLocale "%F %T" <$> liftIO getCurrentTime
    pn <- liftIO getProgName
    mapM_ (liftIO . logger dt pn i) $ dropWhile (=="") $ lines x

instance JassoLogger T.Text where
  logItRaw logger i x = do
    dt <- formatTime defaultTimeLocale "%F %T" <$> liftIO getCurrentTime
    pn <- liftIO getProgName
    mapM_ (liftIO . logger (T.pack dt) (T.pack pn) i) $ dropWhile T.null $ T.lines x
