{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Level06.Conf
    ( parseOptions
    ) where

import           GHC.Word                 (Word16)

import           Data.Bifunctor           (first)
import           Data.Monoid              (Last (Last), (<>))

import           Level06.AppM             (AppM (..), liftEither)
import           Level06.Types            (Conf (..), ConfigError (..),
                                           DBFilePath, mkDBFilePath, PartialConf (..),
                                           Port (Port))

import           Level06.Conf.CommandLine (commandLineParser)
import           Level06.Conf.File        (parseJSONConfigFile)

import           Control.Exception        (try)
import           Data.Functor             ((<&>))

-- | For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf :: PartialConf
defaultConf = PartialConf (Last $ Just $ Port 8080) (Last $ mkDBFilePath "defaultlevel06.db")

-- | We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig :: PartialConf -> Either ConfigError Conf
makeConfig (PartialConf (Last Nothing)  _               ) = Left PortMissingError
makeConfig (PartialConf _               (Last Nothing)  ) = Left DBFilePathMissingError
makeConfig (PartialConf (Last (Just p)) (Last (Just fp))) = Right $ Conf p fp

-- | This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
--
-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions :: FilePath -> AppM ConfigError Conf
parseOptions fp = do
  file        <- parseJSONConfigFile fp
  commandLine <- AppM $ try commandLineParser <&> first CommandLineError
  liftEither $ makeConfig (defaultConf <> file <> commandLine)
  -- Parse the options from the config file: "files/appconfig.json"
  -- Parse the options from the commandline using 'commandLineParser'
  -- Combine these with the default configuration 'defaultConf'
  -- Return the final configuration value