{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UndecidableInstances #-}

module Video.HandBrake.Title
  ( Title, mkTitle )
where

-- use yaml

-- aeson -------------------------------

import Data.Aeson        ( FromJSON( parseJSON )
                         , Result  ( Error, Success )
                         , ToJSON  ( toJSON )
                         , Value   ( Number, String )
                         )
import Data.Aeson.TH     ( deriveJSON, defaultOptions )
import Data.Aeson.Types  ( Parser )

-- base --------------------------------

import Control.Monad        ( foldM, mzero )
import Data.Char            ( isDigit, isSpace )
import Data.List            ( intercalate )
import Data.Maybe           ( fromMaybe, isJust )
import Data.Ratio           ( Ratio, (%), numerator, denominator )
import Data.Word            ( Word8, Word16, Word32 )
import Debug.Trace          ( traceShow )
import Text.Printf          ( printf )
import Text.Read            ( readMaybe )

-- bytestring --------------------------

import qualified Data.ByteString.Char8 as BS

-- containers --------------------------

import Data.Tree ( Tree, drawTree )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadThrow )

-- lens --------------------------------

import Control.Lens    ( Getter, ASetter, Lens'
                       , (&), (%~), (^.), (?~), (.~)
                       , _1, makeLenses, set, to, view
                       )
import Data.Tree.Lens  ( branches, root )

-- regex -------------------------------

import Text.Regex.Applicative         ( RE, (=~)
                                      , anySym, many, psym, string, sym )
import Text.Regex.Applicative.Common  ( decimal, digit )

-- safe --------------------------------

import Safe  ( lastDef )

-- scientific --------------------------

import Data.Scientific  ( Scientific, fromFloatDigits )

-- text --------------------------------

import Data.Text  ( pack, unpack )

-- time --------------------------------

import Data.Time.Clock  ( DiffTime, secondsToDiffTime )

-- unordered-containers ----------------

import Data.HashMap.Strict  ( singleton )

-- yaml imports ------------------------

import Data.Yaml.Aeson  ( encode )

-- fluffy ------------------------------

import Fluffy.Data.List   ( splitBy2, splitOn, splitOn2 )
import Fluffy.Data.Time   ( timeFormatDuration, timeScanDuration )
import Fluffy.Sys.Exit    ( dieParse )
import Fluffy.Text.Regex  ( frac )

--------------------------------------------------------------------------------

-- | trim the non-essential tail off a decimal (trailing 0s after a ., and maybe
--   the . itself
dtrim :: String -> String
dtrim s | '.' `elem` s = reverse $ dropWhile (== '.') $ dropWhile (== '0') $
                                   reverse s
        | otherwise    = s

-- REMatch ---------------------------------------------------------------------

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

-- PixelAspect -----------------------------------------------------------------

newtype PixelAspect = PixelAspect (Ratio Word8)

instance REMatch PixelAspect where
  re    = PixelAspect <$> ((%) <$> decimal <*> (string "/" *> decimal))
  parse = parseREMatch "pixelaspect"

instance ToJSON PixelAspect where
  toJSON = toJSONString

instance FromJSON PixelAspect where
  parseJSON = parseJSONString

instance Show PixelAspect where
  show (PixelAspect p) = printf "%d/%d" (numerator p) (denominator p)

-- Autocrop --------------------------------------------------------------------

data Autocrop = Autocrop Word16 Word16 Word16 Word16

instance REMatch Autocrop where
  re = let slash      = string "/"
           whitespace = many (psym isSpace)
        in Autocrop <$> (decimal <* slash)
                                 <*> (decimal <* slash)
                                 <*> (decimal <* slash)
                                 <*> decimal
  parse = parseREMatch "autocrop"

instance Show Autocrop where
  show (Autocrop t b l r) = printf "%d/%d/%d/%d" t b l r

instance FromJSON Autocrop where
  parseJSON = parseJSONString

instance ToJSON Autocrop where
  toJSON = toJSONString

-- DisplayAspect ---------------------------------------------------------------

newtype DisplayAspect = DisplayAspect Float

instance Show DisplayAspect where
  show (DisplayAspect p) = printf "%3.2f" p

instance REMatch DisplayAspect where
  re    = DisplayAspect <$> frac
  parse = parseREMatch "displayaspect"

instance FromJSON DisplayAspect where
  parseJSON = parseJSONString

instance ToJSON DisplayAspect where
  toJSON = toJSONString

-- FrameRate -------------------------------------------------------------------

newtype FrameRate = FrameRate Float -- in fps

instance Show FrameRate where
  show (FrameRate f) = dtrim $ printf "%3.2ffps" f

instance REMatch FrameRate where
  re    = FrameRate <$> frac <* many (sym ' ') <* string "fps"
  parse = parseREMatch "framerate"

instance FromJSON FrameRate where
  parseJSON = parseJSONString

instance ToJSON FrameRate where
  toJSON = toJSONString

-- FrameSize -------------------------------------------------------------------

data FrameSize = FrameSize Word16 Word16

instance Show FrameSize where
  show (FrameSize w h) = printf "%dx%d" w h

instance REMatch FrameSize where
  re    = FrameSize <$> decimal <*> (string "x" *> decimal)
  parse = parseREMatch "framesize"

instance FromJSON FrameSize where
  parseJSON = parseJSONString

instance ToJSON FrameSize where
  toJSON = toJSONString

-- Duration --------------------------------------------------------------------

newtype Duration = Duration Word32 -- in seconds

instance ToJSON Duration where
  toJSON (Duration d) = String (pack $ timeFormatDuration d)
  
instance FromJSON Duration where
  parseJSON (String t) = maybe mzero (return . Duration) $ timeScanDuration (unpack t)
  parseJSON x          = mzero

durFromHHMMSS :: (Word8, Word8, Word8) -> Duration
durFromHHMMSS (hh,mm,ss) = Duration $   fromIntegral hh*60*60
                                      + fromIntegral mm * 60
                                      + fromIntegral ss

instance Show Duration where
  show (Duration d) = timeFormatDuration d

instance REMatch Duration where
  re = let colon      = string ":"
           whitespace = many (psym isSpace)
        in durFromHHMMSS <$> ((,,) <$> (decimal <* colon)
                                   <*> (decimal <* colon)
                                   <*> decimal)
  parse = parseREMatch "duration"

-- Audio -----------------------------------------------------------------------

data Audio = Audio { trackid :: !Int
                   , stuff   :: !String
                   , freq    :: !Int
                   , bw      :: !Int
                   }
  deriving Show

$( deriveJSON defaultOptions ''Audio )

instance REMatch Audio where
  re    = Audio <$> trackid <*> stuff <*> freq <*> bw
          where trackid = decimal <* string ", "
                stuff   = many (psym (/= ',')) <* string ", "
                freq    = decimal <* string "Hz, "
                bw      = decimal <* string "bps"
  parse = parseREMatch "audio"

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

mshow :: Show a => Maybe a -> String
mshow = maybe "" show

duration_mshow :: Getter Title String
duration_mshow = duration . to mshow

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

show' t@(Title i d cs sts as ac pa size rate da up) =
    let kvs = [ ( "title"       , show i)
              , ( "duration"    , t ^. duration_mshow) -- maybe "" show d)
              , ( "chapters"    , (show $ length cs))
              , ( "chapters"    , showit $ (zip [1..] (fmap show cs) :: [(Int, String)]))
              , ( "autocrop"    , maybe "" show ac)
              , ( "pixel aspect", maybe "" show pa)
              , ( "subtitles"   , showit $ (zip [1..] (fmap show sts) :: [(Int, String)]))
              ]
        keylen = maximum $ fmap (length . view _1) kvs
        keyf k = k ++ replicate (keylen - length k) ' '
        chaps = zip [1..] cs :: [(Int, Chapter)]
     in concat ( fmap (\ (k, v) -> concat [ keyf k
                                          , ":"
                                          , if '\n' `elem` v then "\n" else " "
                                          , v
                                          , if '\n' == lastDef ' ' v then "" else "\n"]
               ) kvs
                )
--     in printf "Title %d %d@%s [%s]%s {%s%s%s}%s:\n%s%s%s"
--               i (length cs) (maybe "" formatDuration d)
--               (intercalate "," $ fmap formatDuration cs)
--               (case ac of
--                 Just (t,b,l,r) -> " " ++ intercalate "/" (map show [t,b,l,r])
--                 Nothing        -> "")
--               (show pa)
--               (maybe "" ((' ' :) . show) size)
--               (maybe "" ((' ' :) . show) da)
--               (maybe "" show rate)
--               (unlines $ fmap show as)
--               (unlines $ fmap show sts)
--               (unlines (fmap (("t: " ++) . drawTree) up))

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
checkDepths min max t =
  let d = treeDepth t
   in if d >= min && d <= max
      then return ()
      else dieParse $
             printf "failed to parse tree; got depth %d, expected %d-%d\n%s"
                    d min max (drawTree t)

checkDepth n = checkDepths n n

----------

parseOne :: MonadThrow m => (String -> m x) -> Tree String -> m x
parseOne parse t = do
  checkDepth 1 t
  parse (t ^. root)

parseMany :: (MonadThrow m, REMatch x) =>
             Int -> Int -> Tree String -> m [x]
parseMany min max t = do
  checkDepths min max t
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

parseDetails :: MonadThrow m => String -> m Details
parseDetails = parse

------------------------------------------------------------

type Details = (FrameSize, PixelAspect, DisplayAspect, FrameRate)

instance REMatch Details where
  re = (,,,) <$> re
             <*> (string ", pixel aspect: " *> re)
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
      parse_tree parser lens set =
        parser branch >>= \x -> return $ t & lens `set` x
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


-- I have a Lens' a b == (a -> f a) -> b -> f b
-- I want a Lens' a String == (a -> f a) -> String -> f String
-- lensLens :: Lens' Temp a -> (a -> b) -> (b -> a) -> Lens' Temp b
lensLens :: (a -> b) -> (b -> a) -> Lens' x a -> Lens' x b
lensLens aToB bToA l functor p =
  ((p &) . set l . bToA) <$> functor (aToB (p ^. l))
