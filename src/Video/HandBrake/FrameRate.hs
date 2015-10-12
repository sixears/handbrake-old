module Video.HandBrake.FrameRate
  ( FrameRate( FrameRate ) )
where

-- aeson -------------------------------

import Data.Aeson  ( FromJSON( parseJSON ), ToJSON  ( toJSON ) )

-- base --------------------------------

import Text.Printf  ( printf )

-- QuickCheck --------------------------

import Test.QuickCheck  ( Arbitrary(..), Gen, choose )

-- regex -------------------------------

import Text.Regex.Applicative  ( many, string, sym )

-- local imports ---------------------------------------------------------------

-- fluffy ------------------------------

import Fluffy.Text.Regex  ( REMatch(..), frac, toJSONString, parseJSONString )

-- FrameRate -------------------------------------------------------------------

newtype FrameRate = FrameRate Float -- in fps
  deriving Eq

instance Show FrameRate where
  show (FrameRate f) = dtrim (printf "%3.2f" f) ++ "fps"

instance REMatch FrameRate where
  re    = FrameRate <$> frac <* many (sym ' ') <* string "fps"
  parse = parseREMatch "framerate"

instance FromJSON FrameRate where
  parseJSON = parseJSONString

instance ToJSON FrameRate where
  toJSON = toJSONString

instance Arbitrary FrameRate where
  -- generate an arbitrary value between 0 & 100, with up to two decimal places
  arbitrary = do a <- choose (0,100) :: Gen Int
                 b <- choose (0,100) :: Gen Int
                 return $ FrameRate (fromIntegral a + fromIntegral b / 100.0)

--------------------------------------------------------------------------------

-- | trim the non-essential tail off a decimal (trailing 0s after a ., and maybe
--   the . itself
dtrim :: String -> String
dtrim s | '.' `elem` s = reverse . dropWhile (== '.') . dropWhile (== '0') $
                                   reverse s
        | otherwise    = s

