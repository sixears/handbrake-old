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
  show (DisplayAspect p) = dtrim $ printf "%3f" p

instance REMatch DisplayAspect where
  re    = DisplayAspect <$> frac
  parse = parseREMatch "displayaspect"

instance FromJSON DisplayAspect where
  parseJSON = parseJSONString

instance ToJSON DisplayAspect where
  toJSON = toJSONString


--------------------------------------------------------------------------------

-- | trim the non-essential tail off a decimal (trailing 0s after a ., and maybe
--   the . itself
dtrim :: String -> String
dtrim s | '.' `elem` s = reverse $ dropWhile (== '.') $ dropWhile (== '0') $
                                   reverse s
        | otherwise    = s