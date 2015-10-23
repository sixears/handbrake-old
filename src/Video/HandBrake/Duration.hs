module Video.HandBrake.Duration
  ( Duration( Duration ), formatDuration )
where

-- aeson -------------------------------

import Data.Aeson        ( FromJSON( parseJSON )
                         , ToJSON  ( toJSON )
                         )
import Data.Aeson.Types  ( Value( String ) )

-- base --------------------------------

import Control.Applicative  ( (<|>), optional )
import Control.Exception    ( Exception )
import Control.Monad        ( mzero )
import Data.List            ( find )
import Data.Maybe           ( catMaybes, fromMaybe )
import Data.Word            ( Word8, Word32 )

-- lens --------------------------------

import Control.Lens  ( _1, _2, view )

-- QuickCheck --------------------------

import Test.QuickCheck  ( Arbitrary( arbitrary ) )

-- regex -------------------------------

import Text.Regex.Applicative         ( RE, sym, string )
import Text.Regex.Applicative.Common  ( decimal )

-- text --------------------------------

import Data.Text               ( unpack )

-- local imports -------------------------------------------

-- fluffy ------------------------------

import Fluffy.Data.Time   ( formatDuration, scanDuration )
import Fluffy.Text.Regex  ( REMatch(..) )

-- Duration --------------------------------------------------------------------

newtype Duration = Duration Word32 -- in seconds
  deriving Eq

instance ToJSON Duration where
  toJSON (Duration d) = String (formatDuration d)

instance FromJSON Duration where
  parseJSON (String t) = maybe mzero (return . Duration) $ scanDuration (unpack t)
  parseJSON _          = mzero

durFromHHMMSS :: (Word8, Word8, Word8) -> Duration
durFromHHMMSS (hh,mm,ss) = Duration $   fromIntegral hh*60*60
                                      + fromIntegral mm * 60
                                      + fromIntegral ss

durationSuffices :: [(Char, Word32)]
durationSuffices = [ ('d', 86400), ('h', 3600), ('m', 60), ('s', 1) ]

data WrongSuffixException = WrongSuffixException String
  deriving Show
instance Exception WrongSuffixException

wrongSuffixError :: Char -> String
wrongSuffixError c = "no such duration suffix '" ++ [c] ++ "'"

durationMultiplier :: Char -> Maybe Word32
durationMultiplier c = fmap (view _2) $ find ((== c) . view _1) durationSuffices

durationMultiplier' :: Integral i => Char -> i
durationMultiplier' c =
  fromIntegral . fromMaybe (error $ wrongSuffixError c) $ durationMultiplier c

parseDur :: [(Int,Char)] -> Duration
parseDur =
  Duration . fromIntegral . sum . fmap (\(i,c) -> i * durationMultiplier' c)

instance Show Duration where
  show (Duration d) = formatDuration d

instance REMatch Duration where
  re = let colon      = string ":"
           cat4Maybes a b c d = catMaybes [a,b,c,d]
           -- construct a regex for part of a duration
           durPart :: Num a => Char -> RE Char (Maybe (a, Char))
           durPart c = optional ((,) <$> decimal <*> sym c)
        in     durFromHHMMSS <$> ((,,) <$> (decimal <* colon)
                                       <*> (decimal <* colon)
                                       <*> decimal)
           <|>
               parseDur <$> (cat4Maybes <$> durPart 'd'
                                        <*> durPart 'h'
                                        <*> durPart 'm'
                                        <*> durPart 's'
                            )

  parse = parseREMatch "duration"

instance Arbitrary Duration where
  arbitrary = fmap Duration arbitrary
