module Video.HandBrake.PixelAspect
  ( PixelAspect( PixelAspect ) )
where

-- base --------------------------------

import Data.Ratio           ( Ratio, (%), numerator, denominator )
import Data.Word            ( Word8 )
import Text.Printf          ( printf )

-- regex -------------------------------

import Text.Regex.Applicative         ( string )
import Text.Regex.Applicative.Common  ( decimal )

-- handbrake ---------------------------

import Data.Aeson        ( FromJSON( parseJSON )
                         , ToJSON  ( toJSON )
                         )

import Video.HandBrake.REMatch  ( REMatch(..), toJSONString, parseJSONString )

-- PixelAspect -----------------------------------------------------------------

newtype PixelAspect = PixelAspect (Ratio Word8)
  deriving Eq

instance REMatch PixelAspect where
  re    = PixelAspect <$> ((%) <$> decimal <*> (string "/" *> decimal))
  parse = parseREMatch "pixelaspect"

instance ToJSON PixelAspect where
  toJSON = toJSONString

instance FromJSON PixelAspect where
  parseJSON = parseJSONString

instance Show PixelAspect where
  show (PixelAspect p) = printf "%d/%d" (numerator p) (denominator p)

