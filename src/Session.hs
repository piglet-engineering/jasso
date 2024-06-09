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

{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Session(mkSession, checkSession, checkSessionM, endSession, getSymKey, encrypt, decrypt, replace) where

import Config
import UITypes
import JassoState
import Logger
import SinkCommon

import Control.Exception(catch)
import GHC.IO.Exception(IOException(..))
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Monad.Trans.State(get, put)
import Crypto.Cipher.Blowfish(Blowfish448)
import Crypto.Cipher.Types(BlockCipher, cipherInit, makeIV, blockSize, ctrCombine)
import Crypto.Data.Padding(pad, Format(..))
import Crypto.Error(CryptoFailable(..))
import Data.Maybe(fromJust)
import Data.Text(pack, Text)
import qualified Data.Text as T(map)
import Data.Text.Encoding(encodeUtf8, decodeUtf8, decodeUtf8')
import Data.Time.Clock(NominalDiffTime, UTCTime, getCurrentTime, addUTCTime)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC
import Servant(AddHeader, addHeader, Handler)
import System.Random.Stateful(uniformByteStringM, globalStdGen)
import Text.Read(readMaybe)
import Web.Cookie(parseCookies)
import System.Directory(doesFileExist)
import System.FilePath((</>))
import System.Posix.Files(setFileCreationMask)
import System.Exit(exitWith, ExitCode(..))

replace :: Eq a => a -> a -> a -> a
replace t r x = if x == t then r else x

encrypt :: BlockCipher c => c -> Text -> IO Text
encrypt k s = do
  nonce <- uniformByteStringM (blockSize k) globalStdGen
  return $ T.map (replace ' ' ',' . replace '+' ',' . replace '/' ':' . replace '=' '_') $ decodeUtf8 $ B64.encode $ nonce <> ctrCombine k (fromJust $ makeIV nonce) (pad (ZERO $ blockSize k) $ encodeUtf8 $ "jasso" <> s)

decrypt :: BlockCipher c => c -> Text -> Maybe Text
decrypt k s = do
  let s' = B64.decodeLenient $ encodeUtf8 $ T.map (replace ',' '+' . replace ':' '/' . replace '_' '=') s
  let (nonce, msg) = splitAt (blockSize k) $ BSC.unpack s'
  n <- makeIV $ BSC.pack nonce
  let s''' = BS.takeWhile (/= 0) $ ctrCombine k n (BSC.pack msg)
  if BS.length s' `rem` blockSize k == 0 && BS.take 5 s''' == "jasso"
    then eitherToMaybe $ decodeUtf8' $ BS.drop 5 s'''
    else Nothing

checkSession :: (MonadIO m, BlockCipher c, JassoSource s) => s -> JassoState -> c -> NominalDiffTime -> Maybe Text -> m (Maybe (User, UTCTime, UTCTime, UTCTime))
checkSession src state symkey assertSecs' cookies' = do
  now <- liftIO getCurrentTime
  case sess' symkey of
    Just (sToken, sStart, sEnd) -> if sStart >= now || now >= sEnd
        then return Nothing
        else do
          let expiryTime = addUTCTime assertSecs' now
          if expiryTime > sEnd -- While you're here, we may as well reissue a new idp session with an updated expiry time
            then return Nothing
            else do
              user <- liftIO $ hoistJassoT state $ verifyUser' src sToken
              case user of
                Just user' -> return $ Just (user', addUTCTime (-2) now, expiryTime, sStart)
                Nothing    -> return Nothing
    Nothing -> return Nothing
  where
    sess' symkey' = do
      cookies <- cookies'
      sessioncookie <- lookup "piglet-jasso-session" $ parseCookies $ encodeUtf8 cookies
      sessioncookie' <- eitherToMaybe $ decodeUtf8' sessioncookie
      sessiontext <- decrypt symkey' sessioncookie'
      readMaybe $ txtToStr sessiontext

checkSessionM :: (MonadIO m, JassoSource s) => s -> Maybe Text -> JassoT m (Maybe Session)
checkSessionM src cookies = do
  conf <- getConf
  (symkey, _) <- getSymKey
  now <- liftIO getCurrentTime
  let aSecs = assertSecs conf
  let sess = sess' symkey
  case sess of
    Just (sToken, sStart, sEnd) -> if sStart >= now || now >= sEnd
        then return Nothing
        else do
          let expiryTime = addUTCTime aSecs now
          if expiryTime > sEnd
            then return Nothing -- While you're here, we may as well reissue a new idp session with an updated expiry time
            else do
              user <- verifyUser' src sToken
              case user of
                Nothing    -> return Nothing
                Just user' -> return $ Just $ Session user' sToken sStart sEnd
    _ -> return Nothing
  where
    sess' symkey = readMaybe . txtToStr =<< (decrypt symkey =<< eitherToMaybe . decodeUtf8' =<< lookup "piglet-jasso-session" . parseCookies . encodeUtf8 =<< cookies)

mkSession :: (BlockCipher c, AddHeader h Text a b) => c -> NominalDiffTime -> NominalDiffTime -> Token -> (UTCTime -> UTCTime -> Handler a) -> Handler b
mkSession symkey assertSecs' sessSecs token f = do
  now <- liftIO getCurrentTime
  let expiryTime = addUTCTime assertSecs' now
  let sEnd = addUTCTime sessSecs now
  sess <- liftIO $ encrypt symkey $ strToTxt $ show (token, now, sEnd)
  addHeader ("piglet-jasso-session=" <> sess <> "; Path=/; Max-Age=" <> pack (show assertSecs') <> "; SameSite=None; Secure; HttpOnly") <$> f now expiryTime

endSession :: (AddHeader h Text a b) => Handler a -> Handler b
endSession f = addHeader "piglet-jasso-session=; Path=/; Max-Age=-1; Secure; HttpOnly" <$> f

loadKey :: LogLevel -> FilePath -> IO BSC.ByteString
loadKey v symkeyfile = do
  logItIO v Normal ("Session: Loading existing symmetric key" :: String)
  catch (BS.readFile symkeyfile) ioCatcher
  where
    ioCatcher :: IOError -> IO BSC.ByteString
    ioCatcher e = do
      logItIO v Error $ "Session: Couldn't read from symmetric key file" <> maybe "" (\s -> " \"" <> s <> "\"") (ioe_filename e) <> ": " <> show (ioe_type e) <> " (" <> ioe_description e <> "), ignoring"
      logItIO v Error ("Session: This means that your sessions will not persist when this server is restarted" :: String)
      createSymKey v symkeyfile

createSymKey :: LogLevel -> FilePath -> IO BSC.ByteString
createSymKey v symkeyfile = do
  logItIO v Normal ("Session: Creating new symmetric key" :: String)
  r <- uniformByteStringM 32 globalStdGen
  prevmask <- setFileCreationMask 3839
  catch (BS.writeFile symkeyfile r) ioCatcher
  _ <- setFileCreationMask prevmask
  return r
  where
    ioCatcher :: IOError -> IO ()
    ioCatcher e = do
      logItIO v Error $ "Session: Couldn't write to symmetric key file" ++ maybe "" (\s -> " \"" ++ s ++ "\"") (ioe_filename e) ++ ": " ++ show (ioe_type e) ++ " (" ++ ioe_description e ++ "), ignoring"
      logItIO v Error ("Session: This means that your sessions will not persist when this server is restarted" :: String)

getSymKey :: MonadIO m => JassoT m (Blowfish448, BSC.ByteString)
getSymKey = do
  JassoState conf keycert symkey sess <- get
  case symkey of
    Just symkey' -> return symkey'
    Nothing -> do
      let v = verbosity conf
      let symkeyfile = varDir conf </> "sym.key"
      b <- liftIO $ doesFileExist symkeyfile
      seed <- if b
        then do
          liftIO $ loadKey v symkeyfile
        else do
          liftIO $ createSymKey v symkeyfile
      let symkey'' = cipherInit seed
      case symkey'' of
        CryptoPassed symkey''' -> do
          put $ JassoState conf keycert (Just (symkey''', seed)) sess
          return (symkey''', seed)
        CryptoFailed err -> do
          logIt Critical $ "Session: Couldn't create symmetric key: " ++ show err
          liftIO $ exitWith $ ExitFailure 2
