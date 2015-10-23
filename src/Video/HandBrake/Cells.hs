{-# LANGUAGE OverloadedStrings #-}

module Video.HandBrake.Cells
  ( Cells( Cells ) )
where

-- aeson -------------------------------

import Data.Aeson        ( FromJSON( parseJSON ), ToJSON  ( toJSON ) )
import Data.Aeson.Types  ( Value( String ) )

-- base --------------------------------

import Control.Applicative  ( optional )
import Data.Word            ( Word8 )

-- formatting --------------------------

import Formatting  ( (%), sformat, int )

-- QuickCheck --------------------------

import Test.QuickCheck  ( Arbitrary( arbitrary ) )

-- regex -------------------------------

import Text.Regex.Applicative.Common  ( decimal )

-- text --------------------------------

import Data.Text  ( Text, unpack )

-- local imports -------------------------------------------

-- fluffy ------------------------------

import Fluffy.Text.Regex  ( REMatch(..), parseJSONString )

-- Cells -----------------------------------------------------------------------


data Cells = Cells Word8 Word8
  deriving Eq

showt :: Cells -> Text
showt (Cells begin end) = sformat (int % "-" % int) begin end

instance Show Cells where
  show = unpack . showt

instance REMatch Cells where
  re    = Cells <$> decimal <*> ("-" *> optional ">" *> decimal)
  parse = parseREMatch "cells"

instance FromJSON Cells where
  parseJSON = parseJSONString

instance ToJSON Cells where
  toJSON = String . showt

instance Arbitrary Cells where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return $ Cells a b
