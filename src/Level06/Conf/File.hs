{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as LBS

import           Data.Text                  (Text, pack)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (try)

import qualified Data.Attoparsec.ByteString as AB

import           Waargonaut                 (Json)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))

import           Level06.AppM               (AppM (..), liftEither)
import           Level06.Types              (ConfigError (..),
                                             PartialConf (PartialConf),
                                             partialConfDecoder)

import           System.IO                  (withFile, IOMode (..), Handle)

import           Data.Functor               ((<&>))

-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> readConfFile "badFileName.no"
-- Left (undefined "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile :: FilePath -> AppM ConfigError ByteString
readConfFile fp = AppM $ try (LBS.readFile fp) <&> first ReadFileError

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile :: FilePath -> AppM ConfigError PartialConf
parseJSONConfigFile fp = readConfFile fp >>= decodeWithFailureHandling
  where
    decodeWithFailureHandling :: ByteString -> AppM ConfigError PartialConf
    decodeWithFailureHandling bs = first mkConfigError (decode bs)

    decode :: ByteString -> AppM (DecodeError, D.CursorHistory) PartialConf
    decode = liftEither . D.pureDecodeFromByteString AB.parseOnly partialConfDecoder

    mkConfigError :: (DecodeError, D.CursorHistory) -> ConfigError
    mkConfigError = BadConfFile . fst

-- Go to 'src/Level06/Conf.hs' next.
