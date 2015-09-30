module Video.HandBrake.FrameSize
  ( FrameSize( FrameSize ) )
where

-- aeson -------------------------------

import Data.Aeson  ( FromJSON( parseJSON ), ToJSON  ( toJSON ) )

-- base --------------------------------

import Data.Word    ( Word16 )
import Text.Printf  ( printf )

-- regex -------------------------------

import Text.Regex.Applicative         ( string )
import Text.Regex.Applicative.Common  ( decimal )

-- local imports ---------------------------------------------------------------

-- fluffy ------------------------------

import Fluffy.Text.Regex  ( REMatch(..), toJSONString, parseJSONString )

-- FrameSize -------------------------------------------------------------------

data FrameSize = FrameSize Word16 Word16
  deriving Eq

instance Show FrameSize where
  show (FrameSize w h) = printf "%dx%d" w h

instance REMatch FrameSize where
  re    = FrameSize <$> decimal <*> (string "x" *> decimal)
  parse = parseREMatch "framesize"

instance FromJSON FrameSize where
  parseJSON = parseJSONString

instance ToJSON FrameSize where
  toJSON = toJSONString

