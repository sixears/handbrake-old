{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Video.HandBrake.Title
  ( Title, mkTitle, newTitle
    -- just for testing
  , ID( ID )
  )
where

-- aeson -------------------------------

import Data.Aeson  ( FromJSON( parseJSON ), ToJSON( toJSON )
                   , Value( Array, Number, Object, String )
                   , (.:), (.:?), (.!=)
                   , eitherDecode
                   )
import Data.Aeson.Parser  ( value )
import Data.Aeson.Types  ( Parser )

-- base --------------------------------

import Control.Monad  ( foldM, mapM, unless )
import Data.Word      ( Word8 )
import Debug.Trace
import Text.Printf    ( printf )

-- bytestring --------------------------

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

-- containers --------------------------

import Data.Tree ( Tree( Node ), drawTree )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadThrow )

-- lens --------------------------------

import Control.Lens    ( Lens'
                       , (&), (%~), (^.), (?~)
                       , makeLenses, set, view
                       )
import Data.Tree.Lens  ( branches, root )

-- QuickCheck --------------------------

import Test.QuickCheck  ( Arbitrary( arbitrary ), sublistOf )

-- regex -------------------------------

import Text.Regex.Applicative         ( string )
import Text.Regex.Applicative.Common  ( decimal )

-- scientific --------------------------

import Data.Scientific  ( toBoundedInteger, scientific )

import qualified  Data.Text  as  T
import Data.Text  ( Text )

-- unordered-containers ----------------

import Data.HashMap.Strict  ( fromList, lookup )

-- vector ------------------------------

import qualified  Data.Vector  as  V


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

-- ID --------------------------------------------------------------------------

newtype ID = ID Word8
  deriving (Eq, Show)

instance Arbitrary ID where
  arbitrary = fmap ID arbitrary

instance ToJSON ID where
  toJSON (ID i) = (Number . (\n -> scientific n 0) . toInteger) i

instance FromJSON ID where
  parseJSON (Number n) = maybe (fail "bad ID") (return . ID) $ toBoundedInteger n
  parseJSON _          = fail "ID not a Number"

instance REMatch ID where
  re    = ID <$> decimal
  parse = parseREMatch "id"


-- AesonTree -------------------------------------------------------------------

-- | a Tree that we can write to JSON/YAML

newtype AesonTree a = AesonTree (Tree a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (AesonTree a) where
  arbitrary = do
    let unAETree :: AesonTree a -> Tree a
        unAETree (AesonTree a) = a
    rootval  <- arbitrary
    a <- arbitrary
    b <- arbitrary
    children <- sublistOf (fmap unAETree [a,b]) -- (fmap . fmap) unAETree arbitrary
    return $ AesonTree (Node rootval children)

instance ToJSON a => ToJSON (AesonTree a) where
  toJSON (AesonTree (Node r cs)) =
    let cjs = Array $ V.fromList $ fmap (toJSON . AesonTree) cs
        -- cj is ( "children", ... ); but nothing if there are no children, just
        -- to avoid needless noise
        cj = if null cs
             then []
             else [("children", cjs )]
     in (Object . fromList) ([ ( "label", toJSON r ) ] ++ cj)

instance FromJSON a => FromJSON (AesonTree a) where
  -- fromJSON :: Value -> Parser (AesonTree a)
  parseJSON (Object t) = do l  <- t .: "label"
                            cs <- t .:? "children" .!= []
                            return . AesonTree $ Node l cs
  parseJSON _          = fail "bad tree"

-- Title -----------------------------------------------------------------------

data Title = Title { _titleid   :: !ID
                   , _duration  :: !(Maybe Duration)
                   , _chapters  :: ![Chapter]
                   , _subtitles :: ![Subtitle]
                   , _audios    :: ![Audio]
                   , _autocrop  :: !(Maybe Autocrop)
                   , _details   :: !(Maybe Details)
                   , _unparsed  :: ![AesonTree String]
                   }
  deriving Eq

$( makeLenses ''Title )

instance ToJSON Title where
  toJSON t = let toJ lens = toJSON (view lens t)
              in (Object . fromList)
                   [ ( "id"        , toJ titleid )
                   , ( "duration"  , toJ duration)
                   , ( "chapters"  , toJ chapters)
                   , ( "subtitles" , toJ subtitles)
                   , ( "audios"    , toJ audios)
                   , ( "autocrop"  , toJ autocrop)
                   , ( "details"   , toJ details)
                   , ( "unparsed"  , toJ unparsed)
                   ]

newtype SubtitleList = SubtitleList [Subtitle]

instance FromJSON SubtitleList where
  parseJSON (Array subs) = SubtitleList <$> mapM parseJSON (V.toList subs)

instance FromJSON Title where
  -- fromJSON :: Value -> Parser Title
  parseJSON (Object t) = do -- subs <- parseJSON =<< (t .: "subtitles")
                            subs2 <- t .: "subtitles"
--                            let subx = case subs2 of
--                                         Array subsv -> Just $ V.head subsv
--                                         _           -> Nothing
                            -- chpx <- fmap value subx
                            let atol (Array v) = V.toList v
                                subs2x = atol subs2
--                            (subs2xx) <- sequence (fmap parseJSON subs2x)
                            let eD (String s) = eitherDecode (LBS.pack (T.unpack s))
                                subs2xx = fmap eD subs2x

                            let parseJS :: (REMatch r) => Value -> Parser r
                                parseJS (String s) = maybe (fail $ "failed to parse '" ++ T.unpack s ++ "'") return ((parse . T.unpack) s)
                                parseJS _          = fail "not a string"

                            let parseArray :: Value -> Parser [Subtitle]
                                parseArray (Array vs) = mapM parseJS (V.toList vs)
                            
                            pa <- parseArray subs2

                            trace "subs (2)" $
                              traceShow (subs2 :: Value) $
                              return ()
                            trace "subs2x" $
                              traceShow (subs2x :: [Value]) $
                              return ()
                            trace "subs2xx" $
                              traceShow (subs2xx :: [Either String Subtitle]) $
                              return ()
                            trace "parseArray" $
                              traceShow pa $
                              return ()
--X                            tid <- t .: "id"
--X                            dur <- t .: "duration"
--X                            chps <-  t .: "chapters"
--X                            subsr <- t .: "subtitles"
--                                    <*> return subs -- t .: "subtitles"
--X                            auds <- t .: "audios"
--X                            crop <- t .: "autocrop"
--X                            dets <- t .: "details"
--X                            unp <- t .: "unparsed"
                            trace "subs (1)" $
--                              traceShow (subs :: [Subtitle]) $
--                              trace "subx" $
--                              traceShow (subx :: Maybe Value) $
                              -- trace "chpx" $
                              -- traceShow (chpx :: Maybe Subtitle) $

                               Title <$> t .: "id"
                                     <*> t .: "duration"
                                     <*> t .: "chapters"
                                     <*> t .: "subtitles"
                                     <*> t .: "audios"
                                     <*> t .: "autocrop"
                                     <*> t .: "details"
                                     <*> t .: "unparsed"
--A                              return $ Title tid dur chps pa auds crop dets unp
--C                              return $ Title tid dur chps subsr auds crop dets unp

  parseJSON _ = fail "title is not a JSON object"

instance Arbitrary Title where
  arbitrary = do tid   <- arbitrary
                 dur   <- arbitrary
                 chaps <- arbitrary
                 subs  <- arbitrary
                 auds  <- arbitrary
                 crops <- arbitrary
                 dets  <- arbitrary
                 unp   <- arbitrary
                 return $ Title tid dur chaps subs auds crops dets unp


instance REMatch Title where
  re    = new <$> (string "title " *> decimal) <* string ":"
          where new :: Word8 -> Title
                new ti = Title (ID ti) Nothing [] [] [] Nothing Nothing []

  parse = parseREMatch "title"

instance Show Title where
  show = BS.unpack . encode . toJSON

newTitle :: Word8 -> Duration -> [Chapter] -> [Audio] -> [Subtitle]
         -> Autocrop -> Details -> [Tree String]
         -> Title
newTitle tid dur chps auds subs crop dets unp = Title { _titleid   = ID tid
                                                      , _duration  = Just dur
                                                      , _chapters  = chps
                                                      , _subtitles = subs
                                                      , _audios    = auds
                                                      , _autocrop  = Just crop
                                                      , _details   = Just dets
                                                      , _unparsed  =
                                                          fmap AesonTree unp
                                                      }

appUnp :: Tree String -> Title -> Title
appUnp tree title = title & unparsed %~ (AesonTree tree:)

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
   in case splitBy2 (`elem` [ ':', ',' ]) (branch ^. root) of
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
