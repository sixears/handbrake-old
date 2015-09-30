{-# LANGUAGE OverloadedStrings #-}

module Video.HandBrake.Chapter
  ( Chapter( Chapter ) )
where

-- aeson -------------------------------

import Data.Aeson        ( FromJSON( parseJSON ), ToJSON ( toJSON ) )
import Data.Aeson.Types  ( Value( String ) )

-- base --------------------------------

import Control.Applicative  ( (<|>) )
import Data.Char            ( isSpace )
import Data.Word            ( Word8, Word16 )

-- formatting --------------------------

import Formatting             ( (%), (%.), sformat, int, shown )
import Formatting.Formatters  ( left )

-- regex -------------------------------

import Text.Regex.Applicative         ( psym, some )
import Text.Regex.Applicative.Common  ( decimal )

-- text --------------------------------

import Data.Text  ( Text, unpack )

-- local imports -------------------------------------------

-- fluffy ------------------------------

import Fluffy.Text.Regex  ( REMatch(..), parseJSONString )

-- handbrake ---------------------------

import Video.HandBrake.Cells       ( Cells )
import Video.HandBrake.Duration    ( Duration )

-- Chapter ---------------------------------------------------------------------

type Blocks = Word16

data Chapter = Chapter { chpid    :: Word8
                       , cells    :: Cells
                       , blocks   :: Blocks
                       , duration :: Duration
                       }
  deriving Eq

showt :: Chapter -> Text
showt c =
  sformat ("Chapter " % (pad 2 %. int) % ": " % (pad 9 %. shown)
                        % " " % (pad 7 %. int) % " blocks cells " % shown)
                        (chpid c)               (duration c)
                                (blocks c)                          (cells c)
  where pad n = left n ' '

instance Show Chapter where
  show = unpack . showt

-- XXX split out Cells.hs
-- XXX clean up, hlint

instance REMatch Chapter where
  re    = let whitespace = some (psym isSpace)
              reNative = Chapter <$> (decimal <* ": ") 
                                 <*> ("cells " *> re <* ", ") 
                                 <*> (decimal <* " blocks, ") 
                                 <*> ("duration " *> re)
              chapter i d b c = Chapter i c b d
              reLocal  = chapter <$> ("Chapter" *> whitespace *> decimal)
                                 <*> (":" *> whitespace *> re)
                                 <*> (whitespace *> decimal <* " blocks ")
                                 <*> ("cells " *> re)
           in reLocal <|> reNative
  parse = parseREMatch "chapter"

instance ToJSON Chapter where
  toJSON = String . showt

instance FromJSON Chapter where
  parseJSON = parseJSONString
