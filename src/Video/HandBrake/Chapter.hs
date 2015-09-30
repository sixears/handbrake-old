{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Video.HandBrake.Chapter
  ( Cells( Cells ), Chapter( Chapter ) )
where

-- aeson -------------------------------

import Data.Aeson        ( FromJSON( parseJSON ), ToJSON  ( toJSON ) )
import Data.Aeson.Types  ( Value( String ) )

-- base --------------------------------

import Control.Applicative  ( (<|>) )
import Data.Char            ( isSpace )
import Data.Word            ( Word8, Word16 )

-- formatting --------------------------

import Formatting             ( (%), (%.), sformat, int, shown )
import Formatting.Formatters  ( left )

-- regex -------------------------------

import Text.Regex.Applicative         ( psym, some, string )
import Text.Regex.Applicative.Common  ( decimal )

-- text --------------------------------

import Data.Text  ( Text )

-- local imports -------------------------------------------

-- fluffy ------------------------------

import Fluffy.Text.Regex  ( REMatch(..), parseJSONString )

-- handbrake ---------------------------

import Video.HandBrake.Duration    ( Duration )

-- Cells -----------------------------------------------------------------------


data Cells = Cells Word8 Word8
  deriving Eq


instance Show Cells where
  show (Cells begin end) = show begin ++ "->" ++ show end

instance REMatch Cells where
  re    = Cells <$> decimal <*> (string "->" *> decimal)
  parse = parseREMatch "cells"

instance FromJSON Cells where
  parseJSON = parseJSONString

-- Chapter ---------------------------------------------------------------------

type Blocks = Word16

data Chapter = Chapter { chpid    :: Word8
                       , cells    :: Cells
                       , blocks   :: Blocks
                       , duration :: Duration
                       }
  deriving Eq

instance Show Chapter where
  show (Chapter i c b d) = show i ++ " - cells: " ++ show c ++ ", " ++ show b ++ " blocks " ++ show d

-- XXX use IsString (Overloaded Strings) to auto-apply string in regexen ?
-- XXX split out Cells.hs
-- XXX clean up, hlint

instance REMatch Chapter where
  re    = let whitespace = some (psym isSpace)
              reNative = Chapter <$> (decimal <* string ": ") <*>
                                     (string "cells " *> re <* string ", ") <*>
                                     (decimal <* string " blocks, ") <*>
                                     (string "duration " *> re)
              chapter i d b c = Chapter i c b d
              reLocal  = chapter <$> (string "Chapter" *> whitespace *> decimal)
                                 <*> (string ":" *> whitespace *> re)
                                 <*> (whitespace *> decimal <* string " blocks ")
                                 <*> (string "cells " *> re)
           in reLocal <|> reNative
  parse = parseREMatch "chapter"

showt :: Chapter -> Text
showt c =
  sformat ("Chapter " % (pad 2 %. int) % ": " % (pad 9 %. shown)
                        % " " % (pad 7 %. int) % " blocks cells " % shown)
                        (chpid c)               (duration c)
                                (blocks c)                          (cells c)
  where pad n = left n ' '

instance ToJSON Chapter where
  toJSON = String . showt

instance FromJSON Chapter where
  parseJSON = parseJSONString
