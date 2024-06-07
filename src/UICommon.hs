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

module UICommon(doctypehtml, head__, main__, logo, uijs, favico, robots, throw400, maybeThrow400, throw401, maybeThrow401, redirect, knownlangs, tr, bestLang) where

import UIIncludes
import Logger

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import qualified Data.ByteString.Lazy as BLU
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.Text as Text (pack)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Lucid (HtmlT, a_, content_, doctype_, header_, href_, html_, lang_, main_, meta_, name_, style_, title_, head_, body_)
import Lucid.Svg (class_, d_, defs_, line_, path_, svg_, viewBox_, x1_, x2_, y1_, y2_)
import qualified Lucid.Svg as Svg (style_)
import Network.HTTP.Types (renderQuery)
import Network.HTTP.Types.Header (hLocation)
import Servant (Handler, ServerError(..), URI(..), err302, err400, err401, throwError)

doctypehtml :: Applicative m => String -> HtmlT m b -> HtmlT m b
doctypehtml lang m = doctype_ *> html_ [lang_ $ Text.pack lang] m

head__ :: Monad m => Bool -> HtmlT m b -> HtmlT m b
head__ isLogin m = head_ $ do
  meta_ [name_ "viewport", content_ "width=device-width"]
  style isLogin
  m

main__ :: Monad m => HtmlT m b -> HtmlT m b -> HtmlT m b
main__ m s = body_ (header_ mempty >> main_ m >> s)

style :: Monad m => Bool -> HtmlT m ()
style isLogin = style_ $ "\
\:root { accent-color: #104080; " <> (if isLogin then "" else "--mdc-theme-primary: #206080; --mdc-theme-secondary: #104080; ") <> "}\n\
\  html { height: 100dvh; }\n\
\  body { background: " <> (if isLogin then "#104080; background: linear-gradient(200deg, #083060 0%, #104080 50%, #104080 70%, #205880"
                                       else "#d0e0ff; background: linear-gradient(200deg, #f0f8f8 0%, #f0f8ff 75%, #e8f0ff") <> " 100%);\
        \ padding: 0; margin: 0; font-family: sans-serif; font-size: large; height: 100dvh; width: 100dvw; position: absolute; }\n\
\  header { padding: 10dvh 0; }\n\
\  #msgbox { align-items: center; justify-content: center; background: #c0c0c0; border: 1px solid #40c0ff; max-width: fit-content; margin: calc(20dvh + 1em) auto; }\n\
\  #msg { margin: 0.2em; padding: 0.5em; }\n\
\  form { display: flex; flex-direction: column; align-items: center; justify-content: center; max-width: fit-content; margin: auto; padding: 1.5em 2em; border: #888888 solid 1px; border-radius: 0.8em; background: #f8fcff; }\n\
\  form > * { width: 16em; margin: 0.5em 0; font-size: medium; box-sizing: border-box; }\n\
\  form > div { display: flex; justify-content: space-between; align-items: baseline; }\n\
\  form > div > input[type=\"submit\"] { width: auto; font-size: medium; cursor: pointer; }\n\
\  form > div > a { text-decoration: none; color: #888888; font-size: small; }\n\
\  form > div > a:hover { text-decoration: dotted underline; }\n"

logo :: Monad m => Bool -> HtmlT m ()
logo isLogin = a_ [title_ "Jasso Single Sign-On from Piglet Engineering", href_ "https://piglet.engineering/jasso-single-sign-on/"] $
  svg_ [viewBox_ "0 0 1000 750", Svg.style_ "width: 4em; position: fixed; bottom: 0; right: 0; margin: 0.5em;"] $ do
    defs_ $ Svg.style_ (".cls-1{fill:#" <> (if isLogin then "ffffff" else "000000") <> ";}.cls-2{fill:#60a0ff;}")
    path_ [class_ "cls-1", d_ "M497 479c-22 28-47 35-73 49-7 4 56-16 70-27C513 487 508 465 497 479Z"]
    path_ [class_ "cls-1", d_ "M438 453c-4 21-16 31-25 45-3 4 24-23 29-33C449 453 440 443 438 453Z"]
    path_ [class_ "cls-1", d_ "M725 180c26 4 48 17 66 37 23 25 36 55 44 87 3 11 6 21 15 28 8 6 18 7 28 8-20 8-41 0-50-20-10-24-17-49-31-71-13-20-28-46-66-59-45-13-66-3-72-3C661 188 686 175 725 180Z"]
    path_ [class_ "cls-1", d_ "M646 173c1-5 2-9 4-14 3-7 3-7 11-3 11 5 24 14 28 19 1 1 1 2 0 3-1 1-3-1-3-1-4-3-14-10-26-14-3-1-4-1-5 1-2 3-5 6-7 9C647 174 646 174 646 173Z"]
    path_ [class_ "cls-1", d_ "M726 272c-2 0-1-1-1-2 2-5 6-9 12-10 6-1 10 2 13 6 7 9 8 28-1 37-7 6-15 6-20-1a18 19 0 0 1-3-5c2 2 3 4 5 5 5 4 10 4 15-0 6-7 8-15 6-24a26 26 0 0 0-5-11C736 254 727 271 726 272Z"]
    path_ [class_ "cls-1", d_ "M727 274c0-2 10-15 19 2a20 20 0 0 1 0 16c-2 4-7 7-11 7-4-1-7-4-8-10 2 2 3 3 4 4 3 3 6 2 8-1 3-5 3-13 0-18-2-3-6-3-9 0-1 1-2 2-3 4C727 276 727 275 727 274Z"]
    path_ [class_ "cls-1", d_ "M224 266c-24 21-35 48-31 84 3 32 17 46 37 63 35 29 43 25 75 47 24 16 25 44 28 53-8-23-15-37-70-61a134 134 0 0 1-15-7C163 396 146 309 214 256c33-26 72-26 72-26s-24 9-35 16C238 255 231 260 224 266Z"]
    path_ [class_ "cls-1", d_ "M698 400c-1-1 10-15 26-16 13-1 24 8 45 5 10-1 14-3 20-10 9-9 12-6 14-5 5 3-1 11-7 14-18 9-25 9-31 9-11 0-30-5-43-7C710 388 698 400 698 400Z"]
    path_ [class_ "cls-1", d_ "M858 375a3 3 0 0 1-3-2c-1-3-1-5 0-7 3-5 15-6 15-6s-11 1-13 7c-1 2-1 2 0 5 1 1 2 1 2 1 6-3 9-10 9-9S865 376 858 375Z"]
    path_ [class_ "cls-2", d_ "M509 471c-21-17-29-27-67-26-16 0-5-36-29-68-14-19-33-23-47-23-45 0-65-63-72-120-1-12 5-20 17-24 63-19 107-26 154-19 36 5 57 16 68 23 18 10 33 31 32 31S538 212 494 203c-35-7-81-13-170 14-8 2-20 7-19 20 7 52 22 105 63 106C399 344 419 367 424 374c22 30 7 60 28 61 34 2 43 13 68 31 9 6 24 4 35 0 10-4 22-15 25-16 8-3 11 6 4 11-2 1-11 4-21 9C542 482 520 479 509 471Z"]
    path_ [class_ "cls-1", d_ "M647 229c2 1 3 3 5 5 0-2 0-3 0-6l7 5c1-8-3-13-11-14-5-1-11-1-16-1 1 15 6 24 16 24-1-4-2-8-3-12Z"]
    path_ [class_ "cls-1", d_ "M600 177c0-1 22-9 45 4 26 15 22 45 22 44-2-10-11-47-55-40 3 74 48 71 48 72S600 260 600 177Z"]
    line_ [class_ "cls-1", x1_ "594", y1_ "237", x2_ "602", y2_ "238"] $ return ()
    path_ [class_ "cls-2", d_ "M539 289c0-5 6-4 6-4 0 0 65-2 65 2 0 3-65 7-65 7S540 295 539 289Z"]
    path_ [class_ "cls-2", d_ "M546 328c-1-5 5-5 5-5 0 0 64-13 65-9 1 3-63 18-63 18S547 333 546 328Z"]
    path_ [class_ "cls-2", d_ "M556 363c-2-5 4-6 5-6 0 0 63-18 64-15 1 3-61 24-61 24S557 368 556 363Z"]
    path_ [class_ "cls-2", d_ "M569 395c-2-5 4-6 4-6 0 0 60-26 61-24 1 2-57 32-57 32S571 400 569 395Z"]
    path_ [class_ "cls-2", d_ "M585 426c-3-4 3-7 3-7 0 0 55-34 57-32 2 2-52 39-52 39S588 430 585 426Z"]
    path_ [class_ "cls-2", d_ "M455 419c2-28-9-58-25-83C456 358 464 390 455 419Z"]
    path_ [class_ "cls-2", d_ "M473 387c3-30-2-63-12-92C477 323 481 357 473 387Z"]
    path_ [class_ "cls-2", d_ "M555 426c-18-14-34-43-33-76C530 378 538 404 555 426Z"]
    path_ [class_ "cls-1", d_ "M110 386c7 7 14 8 23 5 9-3 17-10 19-19 3-14-3-26-15-31-7-3-15-2-18 3-5 7-5 15 1 22a33 33 0 0 0 13 9c3 1 5 0 5-3 0-5 0-9-4-12-2-1-4-2-4-6a1 1 0 0 1 1-2c3 0 5 1 6 3 5 5 6 14 2 21s-14 11-22 10C114 387 112 386 110 386Z"]
    path_ [class_ "cls-1", d_ "M169 358c0-2-1-4-2-14 0-2-10 5-13 14 0 1 0 10 2 11 1 1 3-2 6-5C165 362 167 361 169 358Z"]
    path_ [class_ "cls-1", d_ "M510 600a45 45 0 0 1-15-14c-6-9-18-13-26-18-11-9-31-27-47-33-21-9-22-19-14-42 9-26 15-46 13-71-2-36-21-61-64-60 25 8 48 17 50 45 3 35-7 67-23 98-11 20-5 28 14 37 22 11 44 22 62 39 1 1 2 11-7 9 0 0-32-10-56-21-24-11-20-8-38-19-11-7-11-8-20-25 3 18 8 28 15 33 38 26 55 23 91 46 0 0 10 6 15 9a23 23 0 0 0 6 3c2 1 4 2 6 0 2-2 0-4-2-5-2-2-4-4-5-8 14 13 28 13 44 6C514 606 515 603 510 600Z"]
    path_ [class_ "cls-1", d_ "M319 582c0-5 20-12 25-22-13 6-44 11-48 13a19 19 0 0 0-4 3c-1 1-3 2-2 4 1 2 3 1 4 1 2 0 4-1 8 1-15 4-22 14-24 27-1 4 1 6 5 4a36 36 0 0 1 16-4c7 0 35 5 68-33C329 599 322 594 319 582Z"]
    path_ [class_ "cls-1", d_ "M889 346a4 4 0 0 0-1-2 3 3 0 0 0-2-1a34 34 0 0 0-11 3 97 97 0 0 0-12 5c-1 0-3 2-4 2-5 2-9 6-10 12-2 7 2 15 3 15 1 0 0-7 1-14 1-5 5-7 9-9 1 0 2-1 3-1 7-4 19-8 19-8s1 0 1 0c-1 2-3 5-6 9-2 2-3 4-5 7l-1 1a89 89 0 0 0-6 10c-1 1-2 3-3 5-3 5-6 7-9 8-20 6-32-4-32-11 0-10 0-17-16-20-1 1 10 3 10 16-1 15 9 22 22 23 10 1 19-4 24-7 2-2 4-4 6-8 1-2 2-4 3-5a83 83 0 0 1 6-10l1-1c2-2 3-4 5-7C887 355 889 353 889 346Z"]
    path_ [class_ "cls-2", d_ "M605 444c19 0 68-25 71-36 5-21-19-35-30-56-13-25-14-50-19-71-3-14-15-30-30-34-7-2-15 0-26 8-2 1-12 9-12 9-1-1 6-8 7-9 0 0 8-9 13-13s12-7 19-7c16 1 28 17 31 22 5 8 12 25 13 34 7 51 13 45 24 62 12 17 19 27 22 38 0 0 3 8 2 15-1 4-2 9-7 14-13 12-50 34-77 32-13-1-25-16-23-16C586 437 596 444 605 444Z"]

throw400 :: BLU.ByteString -> Handler a
throw400 msg = throwError err400 { errBody = msg }

maybeThrow400 :: Text -> LogLevel -> BLU.ByteString -> Maybe a -> Handler a
maybeThrow400 mymodule v msg = maybe throwit return
  where
    throwit = do
      liftIO $ logItIO v Debug $ mymodule <> ": Error " <> decodeUtf8 (BLU.toStrict msg)
      throw400 msg

throw401 :: Text -> LogLevel -> Text -> BLU.ByteString -> Handler a
throw401 mymodule v logmsg msg = do
  liftIO $ logItIO v Debug $ mymodule <> ": Error " <> logmsg
  throwError err401 { errBody = msg }

maybeThrow401 :: Text -> LogLevel -> Text -> Maybe a -> Handler a
maybeThrow401 mymodule v msg = maybe (throw401 mymodule v msg $ BLU.fromStrict $ encodeUtf8 msg) return

redirect :: URI -> [(Text, Maybe Text)] -> Handler a
redirect uri params = throwError err302 { errHeaders = [(hLocation, BS.pack location)]}
  where
    location = show uri {uriQuery = BS.unpack $ renderQuery True $ mapMaybe encodePair params}
    encodePair (k, Just v)  = Just (encodeUtf8 k, Just $ encodeUtf8 v)
    encodePair (_, Nothing) = Nothing

-- TODO: use a better system for translating messages

knownlangs :: [String]
knownlangs = ["en", "de", "de-CH", "fr"]

tr :: String -> Text -> Text
tr "de-CH" "Username or Email Address" = "Username or Email Address"
tr "de" "Username or Email Address" = "Username or Email Address"
tr "fr" "Username or Email Address" = "Username or Email Address"
tr "de" "Password" = "Password"
tr "fr" "Password" = "Password"
tr "de" "Please confirm by reloading the page." = "Please confirm by reloading the page."
tr "fr" "Please confirm by reloading the page." = "Please confirm by reloading the page."

tr "de-CH" phrase = tr "de" phrase
tr _ phrase = phrase

bestLang :: Maybe String -> String
bestLang Nothing  = "en"
bestLang (Just l) = fromMaybe "en" $ find (`elem` knownlangs) $ map (takeWhile (`elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-" :: String))) $ splitOn "," l
