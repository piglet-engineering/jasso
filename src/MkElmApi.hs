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

module Main where

import UITypes
import Servant.Elm

main :: IO()
main = generateElmModuleWith defElmOptions ["Api"] defElmImports "frontend"
  [ DefineElm (Proxy :: Proxy Role)
  , DefineElm (Proxy :: Proxy User)
  , DefineElm (Proxy :: Proxy Group)
  , DefineElm (Proxy :: Proxy Status)
  , DefineElm (Proxy :: Proxy MFAStatus)
  ] (Proxy :: Proxy UIAPI''')
