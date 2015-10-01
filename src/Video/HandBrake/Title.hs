{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UndecidableInstances #-}

module Video.HandBrake.Title
  ( Title, mkTitle )
where

-- aeson -------------------------------

import Data.Aeson        ( ToJSON  ( toJSON ) )
import Data.Aeson.TH     ( deriveJSON, defaultOptions )

-- base --------------------------------

import Control.Monad        ( foldM, unless )
import Data.Word            ( Word8 )
import Text.Printf          ( printf )

-- bytestring --------------------------

import qualified Data.ByteString.Char8 as BS

-- containers --------------------------

import Data.Tree ( Tree, drawTree )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadThrow )

-- lens --------------------------------

import Control.Lens    ( Lens'
                       , (&), (%~), (^.), (?~)
                       , makeLenses, set, view
                       )
import Data.Tree.Lens  ( branches, root )

-- regex -------------------------------

import Text.Regex.Applicative         ( string )
import Text.Regex.Applicative.Common  ( decimal )

-- yaml imports ------------------------

import Data.Yaml.Aeson  ( encode )

-- local imports ---------------------------------------------------------------

-- fluffy ------------------------------

import Fluffy.Data.List   ( splitBy2 )
import Fluffy.Sys.Exit    ( dieParse )
import Fluffy.Text.Regex  ( REMatch(..) )

-- handbrake ---------------------------

import Video.HandBrake.Audio          ( Audio )
import Video.HandBrake.Autocrop       ( Autocrop )
import Video.HandBrake.Chapter        ( Chapter )
import Video.HandBrake.Details        ( Details )
import Video.HandBrake.Duration       ( Duration )
import Video.HandBrake.Subtitle       ( Subtitle )

--------------------------------------------------------------------------------

-- Title -----------------------------------------------------------------------

data Title = Title { _titleid   :: !Word8
                   , _duration  :: !(Maybe Duration)
                   , _chapters  :: ![Chapter]
                   , _subtitles :: ![Subtitle]
                   , _audios    :: ![Audio]
                   , _autocrop  :: !(Maybe Autocrop)
                   , _details   :: !(Maybe Details)
                   , _unparsed  :: [Tree String]
                   }

$( makeLenses ''Title )
$( deriveJSON defaultOptions ''Title )

class Showable a where
  showit :: a -> String

instance Showable String where
  showit = id

instance Showable [(Int, String)] where
  showit as = unlines $ fmap (uncurry (printf " - %02d - %8s")) as

instance REMatch Title where
  re    = newTitle <$> (string "title " *> decimal) <* string ":"
  parse = parseREMatch "title"

instance Show Title where
  show = BS.unpack . encode . toJSON

newTitle :: Word8 -> Title
newTitle ti = Title ti Nothing [] [] [] Nothing Nothing []

appUnp :: Tree String -> Title -> Title
appUnp tree title = title & unparsed %~ (tree:)

treeDepth :: Tree x -> Int
treeDepth t = case t ^. branches of
                [] -> 1
                bs -> 1 + maximum (fmap treeDepth bs)

-- | check the depth of a tree is as expected
checkDepths :: MonadThrow m => Int -> Int -> Tree String -> m ()
checkDepths low high t =
  let d = treeDepth t
   in unless (d >= low && d <= high) . dieParse $
        printf "failed to parse tree; got depth %d, expected %d-%d\n%s"
               d low high (drawTree t)

checkDepth :: MonadThrow m => Int -> Tree String -> m ()
checkDepth n = checkDepths n n

----------

parseMany :: (MonadThrow m, REMatch x) =>
             Int -> Int -> Tree String -> m [x]
parseMany low high t = do
  checkDepths low high t
  mapM (parse . view root) (t ^. branches)

parseChapters :: MonadThrow m => Tree String -> m [Chapter]
-- we require some chapters, so there has to be a depth of two
parseChapters = parseMany 2 2

parseSubtitles :: MonadThrow m => Tree String -> m [Subtitle]
-- may (depth 2) or may not (depth 1) be some subtitles
parseSubtitles = parseMany 1 2

--------------------------------------------------------------------------------

parseAudios :: MonadThrow m => Tree String -> m [Audio]
-- may (depth 2) or may not (depth 1) be some audios (yes!  some tracks have no
-- audio, e.g., credits or copyright notices)
parseAudios = parseMany 1 2

------------------------------------------------------------

addDetail :: MonadThrow m => Title -> Tree String -> m Title
addDetail t branch =
  let trim :: String -> String
      trim = dropWhile (== ' ')
      parse_tree parser lens setter =
        parser branch >>= \x -> return $ t & lens `setter` x
      parse_root :: (MonadThrow m, REMatch r) =>
                    String -> Lens' Title (Maybe r) -> m Title
      parse_root x lens =
        checkDepth 1 branch >> parse (trim x) >>= \ x' -> return $ t & lens ?~ x'
   in case splitBy2 (`elem` ":,") (branch ^. root) of
        ("duration", x)         -> parse_root x duration
        ("chapters", "")        -> parse_tree parseChapters  chapters  set
        ("subtitle tracks", "") -> parse_tree parseSubtitles subtitles set
        ("audio tracks", "")    -> parse_tree parseAudios    audios    set
        ("autocrop", x)         -> parse_root x autocrop
        ("size", x)             -> parse_root x details
        _                       -> return $ appUnp branch t

mkTitle :: MonadThrow m => Tree String -> m Title
mkTitle t = do
  ti <- parse $ t ^. root
  foldM addDetail ti (t ^. branches)
