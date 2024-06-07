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

{-# LANGUAGE TypeApplications, OverloadedStrings, FlexibleContexts #-}

module Otp (hotp, hotpCheck, totp, totpCheck, getTotpKey) where

import Codec.QRCode (ErrorLevel(..), QRCodeOptions(..), QRImage(..), TextEncoding(..), encodeText, toMatrix)
import Control.Arrow ((***))
import Crypto.Hash.Algorithms (SHA1, SHA512)
import Crypto.MAC.HMAC (hmac)
import qualified Data.Binary as Bin (decode, encode)
import Data.Bits ((.&.))
import Data.Bool (bool)
import qualified Data.ByteArray as BA (convert)
import qualified Data.ByteString as BS (ByteString, drop, last, take)
import Data.ByteString.Base32 (encodeBase32')
import qualified Data.ByteString.Char8 as BSC (ByteString)
import qualified Data.ByteString.Lazy as BSL (ByteString, fromStrict, toStrict, empty)
import qualified Data.ByteString.UTF8 as BSU (fromString)
import Data.Colour.Names (black, white)
import Data.Monoid (Any)
import Data.Text (Text)
import qualified Data.Text.Encoding as TSE
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Vector.Unboxed as V
import Data.Word
import Diagrams.Backend.SVG (Options(..), SVG(..))
import Diagrams.Prelude (Backend, Path(..), QDiagram, Renderable, Trail, V2, (===), (|||), at, bg, fc, lw, mkWidth, none, p2, renderDia, scale, square, strutX, strutY, stroke)
import Graphics.Svg.Core (renderBS)
import Network.HTTP.Types (urlEncode)

-- allowed +/- skew offset before rejecting
off :: Word64
off = 3

qrOptions :: QRCodeOptions
qrOptions = QRCodeOptions 1 40 L False Nothing

getTotpKey :: BSC.ByteString -> Maybe Text -> Text -> (BS.ByteString, Word32, BSL.ByteString)
getTotpKey symkey siteName username = (r, Bin.decode $ BSL.fromStrict r, r')
  where
    b = BA.convert $ hmac @_ @_ @SHA512 symkey $ TSE.encodeUtf8 username <> "#piglet-jasso#" <> BSU.fromString (show @Int 1)
    r = BS.take 32 $ BS.drop (fromIntegral $ BS.last b .&. 0x1f) b
    r' = maybe BSL.empty qrToSvg $ encodeText qrOptions Iso8859_1 $ TSE.decodeUtf8 $ "otpauth://totp/" <> issuer <> ":" <> TSE.encodeUtf8 username <> "?secret=" <> encodeBase32' r <> "&issuer=" <> issuer
    issuer = (case siteName of
                Nothing -> ""
                Just s  -> urlEncode False (TSE.encodeUtf8 s) <> "%20") <> "SSO"

hotp :: BS.ByteString -> Word64 -> Word32
hotp key cnt = (Bin.decode @Word32 rb .&. 0x7fffffff) `rem` 1000000
  where
    b = BA.convert $ hmac @_ @_ @SHA1 key $ BSL.toStrict $ Bin.encode cnt
    rb = BSL.fromStrict $ BS.take 4 $ BS.drop (fromIntegral $ BS.last b .&. 0x0f) b

hotpCheck :: BS.ByteString -> Word64 -> Word32 -> Bool
hotpCheck key cnt pwd = pwd `elem` [hotp key c | c <- [cnt-off..cnt+off]]

totp :: BS.ByteString -> UTCTime ->  Word32
totp key time = hotp key $ floor (utcTimeToPOSIXSeconds time) `div` 30

totpCheck :: BS.ByteString -> UTCTime -> Word32 -> Bool
totpCheck key time pwd = pwd `elem` [hotp key t | t <- [t0-off..t0+off]]
  where
    t0 = floor (utcTimeToPOSIXSeconds time) `div` 30

instance Show QRImage where
  show (QRImage _v _e s d) = '\n' : replicate (s + 4) '█' ++ '\n' : replicate (s + 2) '█' ++ show' d ++ '█' : '█' : '\n' : replicate (s + 4) '█' ++ '\n' : replicate (s + 4) '█'
    where
      show' xs = if V.null xs then "" else let (v1, v2) = V.splitAt s xs in show'' v1 ++ show' v2
      show'' v1 = '█' : '█' : '\n' : '█' : '█' : V.foldl' (\b x -> b ++ [bool '█' ' ' x]) "" v1

qrToSvg :: QRImage -> BSL.ByteString
qrToSvg q = renderBS $ renderDia SVG (SVGOptions (mkWidth 320) Nothing "" [] True) dia
  where
    dia = scale 6 $ stroke' $ pathMatrix $ toMatrix True False q

    stroke' :: (Backend b V2 Double, Renderable (Path V2 Double) b) => Path V2 Double -> QDiagram b V2 Double Any
    stroke' = bg white . quiet . fc black . lw none . stroke
      where
        zoneX = strutX 4
        zoneY = strutY 4
        quiet d = zoneY === (zoneX ||| d ||| zoneX) === zoneY

    toTrail :: (Bounded a, Eq a) => a -> Trail V2 Double
    toTrail x = if x == minBound then mempty else square 1

    pathList :: (Bounded a, Eq a, Integral ix) => [((ix, ix), a)] -> Path V2 Double
    pathList = Path . fmap (uncurry (flip at) . (p2int *** toTrail))
      where p2int = p2 . (fromIntegral *** fromIntegral)

    pathMatrix :: (Bounded a, Eq a) => [[a]] -> Path V2 Double
    pathMatrix matrix =
      pathList $ do
        (c, col) <- count $ reverse matrix
        (r, val) <- count col
        return ((r,c), val)
      where count = zip [(0::Int)..]
