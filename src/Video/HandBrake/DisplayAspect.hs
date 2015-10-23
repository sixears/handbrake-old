module Video.HandBrake.DisplayAspect
  ( DisplayAspect( DisplayAspect ) )
where

-- aeson -------------------------------

import Data.Aeson  ( FromJSON( parseJSON ), ToJSON  ( toJSON ) )

-- base --------------------------------

import Data.Word    ( Word16 )
import Text.Printf  ( printf )

-- QuickCheck --------------------------

import Test.QuickCheck  ( Arbitrary(..), Gen, choose )

-- local imports ---------------------------------------------------------------

-- fluffy ------------------------------

import Fluffy.Text.Regex  ( REMatch(..), frac, parseJSONString, toJSONString )

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

instance Arbitrary DisplayAspect where
  arbitrary = do a <- choose (0,999) :: Gen Word16
                 b <- choose (0,999) :: Gen Word16
                 let s = printf "%d.%03d" a b
                 return $ DisplayAspect . read $ s

--------------------------------------------------------------------------------

-- | trim the non-essential tail off a decimal (trailing 0s after a ., and maybe
--   the . itself
dtrim :: String -> String
dtrim s | '.' `elem` s = reverse . dropWhile (== '.') . dropWhile (== '0') $
                                   reverse s
        | otherwise    = s
