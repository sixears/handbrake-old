{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Video.HandBrake.Audio
  ( Audio( Audio ) )
where

-- aeson -------------------------------

import Data.Aeson        ( FromJSON( parseJSON ), ToJSON  ( toJSON ) )
import Data.Aeson.Types  ( Value( String ) )

-- base --------------------------------

import Control.Applicative  ( (<|>) )

-- formatting --------------------------

import Formatting  ( (%), sformat, int, string )

-- regex -------------------------------

import qualified Text.Regex.Applicative as RE
import Text.Regex.Applicative         ( anySym, many, psym )
import Text.Regex.Applicative.Common  ( decimal )

-- text --------------------------------

import Data.Text  ( Text, unpack )

-- local imports -------------------------------------------

-- fluffy ------------------------------

import Fluffy.Text.Regex  ( REMatch(..), parseJSONString )

-- Audio -----------------------------------------------------------------------

data Audio = Audio { audioid   :: !Int
                   , misc      :: !String
                   , frequency :: !Int
                   , bandwidth :: !Int
                   }
  deriving Eq


instance REMatch Audio where
  re    =     Audio <$> (trackid <* RE.string ", ")
                    <*> stuff
                    <*> (freq <* RE.string ", ")
                    <*> bw
          <|>
              audio <$> (RE.string "Audio " *> trackid)
                    <*> (RE.string ": " *> freq <* space)
                    <*> (bw <* RE.string " # ")
                    <*> anyStr
          where audio a f b m = Audio a m f b
                space   = RE.string " "
                anyStr  = many anySym
                trackid = decimal
                stuff   = many (psym (/= ',')) <* RE.string ", "
                freq    = decimal <* RE.string "Hz"
                bw      = decimal <* RE.string "bps"

  parse = parseREMatch "audio"

showt :: Audio -> Text
showt a = sformat ("Audio " % int % ": " % int % "Hz " % int %  "bps # " % string)
                  (audioid a) (frequency a) (bandwidth a) (misc a)


instance Show Audio where
  show = unpack . showt

instance FromJSON Audio where
  parseJSON = parseJSONString

instance ToJSON Audio where
  toJSON = String . showt
