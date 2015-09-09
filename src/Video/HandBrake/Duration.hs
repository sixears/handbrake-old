module Video.HandBrake.Duration
  ( Duration( Duration ) )
where

-- aeson -------------------------------

import Data.Aeson        ( FromJSON( parseJSON )
                         , ToJSON  ( toJSON )
                         )
import Data.Aeson.Types  ( Value( String ) )

-- base --------------------------------

import Control.Monad  ( mzero )
import Data.Word      ( Word8, Word32 )

-- regex -------------------------------

import Text.Regex.Applicative         ( string )
import Text.Regex.Applicative.Common  ( decimal )

-- text --------------------------------

import Data.Text  ( pack, unpack )

-- local imports -------------------------------------------

-- fluffy ------------------------------

import Fluffy.Data.Time   ( timeFormatDuration, timeScanDuration )

-- handbrake ---------------------------

import Video.HandBrake.REMatch  ( REMatch(..) )

-- Duration --------------------------------------------------------------------

newtype Duration = Duration Word32 -- in seconds
  deriving Eq

instance ToJSON Duration where
  toJSON (Duration d) = String (pack $ timeFormatDuration d)

instance FromJSON Duration where
  parseJSON (String t) = maybe mzero (return . Duration) $ timeScanDuration (unpack t)
  parseJSON _          = mzero

durFromHHMMSS :: (Word8, Word8, Word8) -> Duration
durFromHHMMSS (hh,mm,ss) = Duration $   fromIntegral hh*60*60
                                      + fromIntegral mm * 60
                                      + fromIntegral ss

instance Show Duration where
  show (Duration d) = timeFormatDuration d

instance REMatch Duration where
  re = let colon      = string ":"
        in durFromHHMMSS <$> ((,,) <$> (decimal <* colon)
                                   <*> (decimal <* colon)
                                   <*> decimal)
  parse = parseREMatch "duration"

