module Video.HandBrake.Autocrop
  ( Autocrop( Autocrop ) )
where

-- aeson -------------------------------

import Data.Aeson        ( FromJSON( parseJSON ), ToJSON  ( toJSON ) )

-- base --------------------------------

import Data.Word            ( Word16 )
import Text.Printf          ( printf )

-- QuickCheck --------------------------

import Test.QuickCheck  ( Arbitrary( arbitrary ) )

-- regex -------------------------------

import Text.Regex.Applicative         ( string )
import Text.Regex.Applicative.Common  ( decimal )

-- local packages ------------------------------------------

-- fluffy ------------------------------

import Fluffy.Text.Regex  ( REMatch(..), parseJSONString, toJSONString )

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

instance Arbitrary Autocrop where
  arbitrary = do t <- arbitrary
                 b <- arbitrary
                 l <- arbitrary
                 r <- arbitrary
                 return $ Autocrop t b l r
