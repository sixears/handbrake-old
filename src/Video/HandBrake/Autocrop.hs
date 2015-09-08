module Video.HandBrake.Autocrop
  ( Autocrop( Autocrop ) )
where

-- base --------------------------------

import Data.Word            ( Word16 )
import Text.Printf          ( printf )

-- regex -------------------------------

import Text.Regex.Applicative         ( string )
import Text.Regex.Applicative.Common  ( decimal )

-- handbrake ---------------------------

import Data.Aeson        ( FromJSON( parseJSON )
                         , ToJSON  ( toJSON )
                         )

import Video.HandBrake.REMatch  ( REMatch(..), toJSONString, parseJSONString )

-- Autocrop --------------------------------------------------------------------

data Autocrop = Autocrop Word16 Word16 Word16 Word16
  deriving Eq

instance REMatch Autocrop where
  re = let slash      = string "/"
        in Autocrop <$> (decimal <* slash)
                                 <*> (decimal <* slash)
                                 <*> (decimal <* slash)
                                 <*> decimal
  parse = parseREMatch "autocrop"

instance Show Autocrop where
  show (Autocrop t b l r) = printf "%d/%d/%d/%d" t b l r

instance FromJSON Autocrop where
  parseJSON = parseJSONString

instance ToJSON Autocrop where
  toJSON = toJSONString
