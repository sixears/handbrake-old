{-# LANGUAGE OverloadedStrings #-}

module Video.HandBrake.Subtitle
  ( Subtitle( Subtitle ) )
where

-- aeson -------------------------------

import Data.Aeson        ( FromJSON( parseJSON ), ToJSON  ( toJSON ) )
import Data.Aeson.Types  ( Value( String ) )

-- formatting --------------------------

import Formatting  ( (%), sformat, int, string )

-- regex -------------------------------

import qualified Text.Regex.Applicative as RE
import Text.Regex.Applicative         ( anySym, many )
import Text.Regex.Applicative.Common  ( decimal )

-- local imports ---------------------------------------------------------------

-- handbrake ---------------------------

import Video.HandBrake.REMatch  ( REMatch(..), parseJSONString )

-- Subtitle --------------------------------------------------------------------

data Subtitle = Subtitle { subtid :: Int
                         , str    :: String }
  deriving (Eq, Show)

instance REMatch Subtitle where
  re    = Subtitle <$> (decimal <* RE.string ", ") <*> many anySym
  parse = parseREMatch "subtitle"

instance FromJSON Subtitle where
  parseJSON = parseJSONString

instance ToJSON Subtitle where
  toJSON s = String $ sformat ("Subtitle " % int % " # " % string) (subtid s)
                                                                   (str s)

