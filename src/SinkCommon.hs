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

module SinkCommon(allowAccess, eitherToMaybe, txtToStr, strToTxt) where

import UITypes

import qualified Data.ByteString.Char8 as C8
import Data.List (intersect)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

txtToStr :: Text -> String
txtToStr = C8.unpack . encodeUtf8

strToTxt :: String -> Text
strToTxt = decodeUtf8 . C8.pack

allowAccess :: User -> Maybe [Text] -> Bool
allowAccess user access = case null . (`intersect` groups user) <$> access of
                            Just True -> False
                            _         -> True
