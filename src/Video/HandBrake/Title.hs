{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UndecidableInstances #-}

module Video.HandBrake.Title
  ( Title, mkTitle )
where

-- use yaml

-- aeson -------------------------------

import Data.Aeson        ( ToJSON  ( toJSON ) )
import Data.Aeson.TH     ( deriveJSON, defaultOptions )

-- base --------------------------------

import Control.Monad        ( foldM )
import Data.Word            ( Word8, Word16 )
import Text.Printf          ( printf )

-- bytestring --------------------------

import qualified Data.ByteString.Char8 as BS

-- containers --------------------------

import Data.Tree ( Tree, drawTree )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadThrow )

-- lens --------------------------------

import Control.Lens    ( Lens'
                       , (&), (%~), (^.), (?~), (.~)
                       , makeLenses, set, view
                       )
import Data.Tree.Lens  ( branches, root )

-- regex -------------------------------

import Text.Regex.Applicative         ( anySym, many, string )
import Text.Regex.Applicative.Common  ( decimal )

-- yaml imports ------------------------

import Data.Yaml.Aeson  ( encode )

-- local imports ---------------------------------------------------------------

-- fluffy ------------------------------

import Fluffy.Data.List   ( splitBy2 )
import Fluffy.Sys.Exit    ( dieParse )

-- handbrake ---------------------------

import Video.HandBrake.Audio          ( Audio )
import Video.HandBrake.Autocrop       ( Autocrop )
import Video.HandBrake.DisplayAspect  ( DisplayAspect )
import Video.HandBrake.Duration       ( Duration )
import Video.HandBrake.FrameRate      ( FrameRate )
import Video.HandBrake.FrameSize      ( FrameSize )
import Video.HandBrake.PixelAspect    ( PixelAspect )
import Video.HandBrake.REMatch        ( REMatch(..) )

--------------------------------------------------------------------------------


-- Subtitle --------------------------------------------------------------------

data Subtitle = Subtitle { subtid :: Int
                         , str    :: String }
  deriving Show

$( deriveJSON defaultOptions ''Subtitle )

instance REMatch Subtitle where
  re    = Subtitle <$> (decimal <* string ", ") <*> many anySym
  parse = parseREMatch "subtitle"

-- Chapter ---------------------------------------------------------------------

data Cells = Cells Word8 Word8
$( deriveJSON defaultOptions ''Cells )

instance Show Cells where
  show (Cells begin end) = show begin ++ "->" ++ show end

instance REMatch Cells where
  re    = Cells <$> decimal <*> (string "->" *> decimal)
  parse = parseREMatch "cells"

type Blocks = Word16

data Chapter = Chapter Word8 Cells Blocks Duration

$( deriveJSON defaultOptions ''Chapter )

instance Show Chapter where
  show (Chapter i c b d) = show i ++ " - cells: " ++ show c ++ ", " ++ show b ++ " blocks " ++ show d

instance REMatch Chapter where
  re    = Chapter <$> (decimal <* string ": ") <*>
                      (string "cells " *> re <* string ", ") <*>
                      (decimal <* string " blocks, ") <*>
                      (string "duration " *> re)
  parse = parseREMatch "chapter"

-- Title -----------------------------------------------------------------------

data Title = Title { _titleid   :: !Word8
                   , _duration  :: !(Maybe Duration)
                   , _chapters  :: ![Chapter]
                   , _subtitles :: ![Subtitle]
                   , _audios    :: ![Audio]
                   , _autocrop  :: !(Maybe Autocrop)
                   , _pixasp    :: !(Maybe PixelAspect)
                   , _framesize :: !(Maybe FrameSize)
                   , _framerate :: !(Maybe FrameRate)
                   , _dispasp   :: !(Maybe DisplayAspect)
                   , _unparsed  :: [Tree String]
                   }

$( makeLenses ''Title )
$( deriveJSON defaultOptions ''Title )

class Showable a where
  showit :: a -> String

instance Showable String where
  showit = id

instance Showable [(Int, String)] where
  showit as = unlines $ fmap (\ (k, v) -> printf " - %02d - %8s" k v) as

instance REMatch Title where
  re    = newTitle <$> (string "title " *> decimal) <* string ":"
  parse = parseREMatch "title"

instance Show Title where
  show = BS.unpack . encode . toJSON

newTitle :: Word8 -> Title
newTitle ti = Title ti Nothing [] [] [] Nothing Nothing Nothing Nothing Nothing []

appUnp :: Tree String -> Title -> Title
appUnp tree title = title & unparsed %~ (tree:)

treeDepth :: Tree x -> Int
treeDepth t = case t ^. branches of
                [] -> 1
                bs -> 1 + (maximum $ fmap treeDepth bs)

-- | check the depth of a tree is as expected
checkDepths :: MonadThrow m => Int -> Int -> Tree String -> m ()
checkDepths low high t =
  let d = treeDepth t
   in if d >= low && d <= high
      then return ()
      else dieParse $
             printf "failed to parse tree; got depth %d, expected %d-%d\n%s"
                    d low high (drawTree t)

checkDepth :: MonadThrow m => Int -> Tree String -> m ()
checkDepth n = checkDepths n n

----------

parseMany :: (MonadThrow m, REMatch x) =>
             Int -> Int -> Tree String -> m [x]
parseMany low high t = do
  checkDepths low high t
  mapM parse (map (view root) $ t ^. branches)

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

type Details = (FrameSize, PixelAspect, DisplayAspect, FrameRate)

instance REMatch Details where
  re = (,,,) <$> re
             <*> (string ", pixel aspect: "   *> re)
             <*> (string ", display aspect: " *> re)
             <*> (string ", " *> re)
  parse = parseREMatch "details"

getDetails :: Title -> Maybe Details
getDetails title = do
  pa <- title ^. pixasp
  fr <- title ^. framerate
  fs <- title ^. framesize
  da <- title ^. dispasp
  return (fs, pa, da, fr)

setDetails :: Title -> (FrameSize, PixelAspect, DisplayAspect, FrameRate)
           -> Title
setDetails t (size, pixel_aspect, display_aspect, rate) =
  t & pixasp    ?~ pixel_aspect
    & framerate ?~ rate
    & framesize ?~ size
    & dispasp   ?~ display_aspect

unsetDetails :: Title -> Title
unsetDetails t =
  t & pixasp    .~ Nothing
    & framerate .~ Nothing
    & framesize .~ Nothing
    & dispasp   .~ Nothing

-- | set details if Just x; unset details if Nothing
maybeSetDetails :: Title -> Maybe Details -> Title
maybeSetDetails t m_det = maybe (unsetDetails t) (setDetails t) m_det

-- | lens onto details of title as a single thing; details will appear to be Nothing
--   if any member is Nothing
details :: Functor f => (Maybe Details -> f (Maybe Details)) -> Title -> f Title
details f t =
  let f_det = f (getDetails t)
      ff :: Maybe Details -> Title
      ff = maybeSetDetails t
   in ff <$> f_det


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
