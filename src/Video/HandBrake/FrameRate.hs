module Video.HandBrake.FrameRate
  ( FrameRate( FrameRate ) )
where

-- aeson -------------------------------

import Data.Aeson  ( FromJSON( parseJSON ), ToJSON  ( toJSON ) )

-- base --------------------------------

import Text.Printf  ( printf )

-- regex -------------------------------

import Text.Regex.Applicative  ( many, string, sym )

-- local imports ---------------------------------------------------------------

-- fluffy ------------------------------

import Fluffy.Text.Regex  ( REMatch(..), frac, toJSONString, parseJSONString )

-- FrameRate -------------------------------------------------------------------

newtype FrameRate = FrameRate Float -- in fps
  deriving Eq

instance Show FrameRate where
  show (FrameRate f) = dtrim $ printf "%3.2ffps" f

instance REMatch FrameRate where
  re    = FrameRate <$> frac <* many (sym ' ') <* string "fps"
  parse = parseREMatch "framerate"

instance FromJSON FrameRate where
  parseJSON = parseJSONString

instance ToJSON FrameRate where
  toJSON = toJSONString

--------------------------------------------------------------------------------

-- | trim the non-essential tail off a decimal (trailing 0s after a ., and maybe
--   the . itself
dtrim :: String -> String
dtrim s | '.' `elem` s = reverse $ dropWhile (== '.') $ dropWhile (== '0') $
                                   reverse s
        | otherwise    = s

