{-# LANGUAGE OverloadedStrings #-}

module Video.HandBrake.Details
  ( Details( Details ) )
where

-- aeson -------------------------------

import Data.Aeson        ( FromJSON( parseJSON ), ToJSON ( toJSON ) )
import Data.Aeson.Types  ( Value( String ) )

-- base --------------------------------

import Control.Applicative  ( (<|>) )

-- formatting --------------------------

import Formatting  ( (%), sformat, shown )

-- QuickCheck --------------------------

import Test.QuickCheck  ( Arbitrary(..) )

-- text --------------------------------

import Data.Text  ( Text, unpack )

-- local imports ---------------------------------------------------------------

-- fluffy ------------------------------

import Fluffy.Text.Regex  ( REMatch(..), parseJSONString )

-- handbrake ---------------------------

import Video.HandBrake.DisplayAspect  ( DisplayAspect )
import Video.HandBrake.FrameRate      ( FrameRate )
import Video.HandBrake.FrameSize      ( FrameSize )
import Video.HandBrake.PixelAspect    ( PixelAspect )

------------------------------------------------------------

-- Details ---------------------------------------------------------------------

data Details = Details FrameSize PixelAspect DisplayAspect FrameRate
  deriving Eq

instance REMatch Details where
  re =     Details <$> re
                   <*> (", pixel aspect: "   *> re)
                   <*> (", display aspect: " *> re)
                   <*> (", " *> re)
       <|> Details <$> re <*> (", " *> re) <*> (", " *> re) <*> (", " *> re)
  parse = parseREMatch "details"

showt :: Details -> Text
showt (Details fs pa da fr) =
  sformat (shown % ", " % shown % ", " % shown % ", " % shown) fs pa da fr

instance Show Details where
  show = unpack . showt

instance ToJSON Details where
  toJSON = String . showt

instance FromJSON Details where
  parseJSON = parseJSONString

instance Arbitrary Details where
  arbitrary = do fs <- arbitrary
                 pa <- arbitrary
                 da <- arbitrary
                 fr <- arbitrary
                 return $ Details fs pa da fr
