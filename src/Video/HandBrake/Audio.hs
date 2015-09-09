{-# LANGUAGE TemplateHaskell   #-}

module Video.HandBrake.Audio
  ( Audio( Audio ) )
where

-- aeson -------------------------------

import Data.Aeson     ( FromJSON( parseJSON ), ToJSON  ( toJSON ) )
import Data.Aeson.TH  ( deriveJSON, defaultOptions )


-- base --------------------------------

import Text.Printf  ( printf )

-- regex -------------------------------

import Text.Regex.Applicative         ( many, psym, string )
import Text.Regex.Applicative.Common  ( decimal )

-- local imports -------------------------------------------

-- handbrake ---------------------------

import Video.HandBrake.REMatch  ( REMatch(..), parseJSONString, toJSONString )

-- Audio -----------------------------------------------------------------------

data Audio = Audio { audioid   :: !Int
                   , misc      :: !String
                   , frequency :: !Int
                   , bandwidth :: !Int
                   }
  deriving Eq

instance REMatch Audio where
  re    = Audio <$> trackid <*> stuff <*> freq <*> bw
          where trackid = decimal <* string ", "
                stuff   = many (psym (/= ',')) <* string ", "
                freq    = decimal <* string "Hz, "
                bw      = decimal <* string "bps"
  parse = parseREMatch "audio"

instance Show Audio where
  show (Audio aid stuff freq bw) = printf "%d: %dHz %dbps # %s" aid freq bw stuff

instance FromJSON Audio where
  parseJSON = parseJSONString

instance ToJSON Audio where
  toJSON = toJSONString
