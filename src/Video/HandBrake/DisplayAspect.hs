module Video.HandBrake.DisplayAspect
  ( DisplayAspect( DisplayAspect ) )
where

-- base --------------------------------

import Text.Printf          ( printf )

-- local imports ---------------------------------------------------------------

-- fluffy ------------------------------

import Fluffy.Text.Regex  ( frac )

-- handbrake ---------------------------

import Data.Aeson        ( FromJSON( parseJSON )
                         , ToJSON  ( toJSON )
                         )

import Video.HandBrake.REMatch  ( REMatch(..), toJSONString, parseJSONString )

-- DisplayAspect ---------------------------------------------------------------

newtype DisplayAspect = DisplayAspect Float
  deriving Eq

instance Show DisplayAspect where
  show (DisplayAspect p) = printf "%3.2f" p

instance REMatch DisplayAspect where
  re    = DisplayAspect <$> frac
  parse = parseREMatch "displayaspect"

instance FromJSON DisplayAspect where
  parseJSON = parseJSONString

instance ToJSON DisplayAspect where
  toJSON = toJSONString

