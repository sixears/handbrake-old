module Video.HandBrake.REMatch
  ( REMatch( .. ), parseJSONString, toJSONString )
where

-- aeson -------------------------------

import Data.Aeson        ( Value   ( String ) )
import Data.Aeson.Types  ( Parser )

-- base --------------------------------

import Control.Monad        ( mzero )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadThrow )

-- regex -------------------------------

import Text.Regex.Applicative         ( RE, (=~) )

-- text --------------------------------

import Data.Text  ( pack, unpack )

-- local imports ---------------------------------------------------------------

-- fluffy ------------------------------

import Fluffy.Sys.Exit    ( dieParse )

--------------------------------------------------------------------------------

class REMatch r where
  re :: RE Char r
  parse :: MonadThrow m => String -> m r
  -- parse = parseREMatch "thing"
  parseREMatch :: MonadThrow m => String -> String -> m r
  parseREMatch name s =
    case s =~ re of
      Just x  -> return x
      Nothing -> dieParse $ "failed to parse " ++ name ++ " '" ++ s ++ "'"

parseJSONString :: (REMatch r) => Value -> Parser r
parseJSONString (String s) = maybe mzero return ((parse . unpack) s)
parseJSONString _          = mzero

toJSONString :: Show t => t -> Value
toJSONString = String . pack . show
