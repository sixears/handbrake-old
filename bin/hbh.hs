#!/usr/local/ghc-7.10.1/bin/runhaskell -i/home/martyn/src/fluffy/src:/home/martyn/src/handbrake/src
-- #!/usr/local/ghc-7.10.1/bin/runhaskell -i/home/martyn/src/fluffy/src:/home/martyn/bin/hlib:/home/martyn/src/handbrake/src

{-# LANGUAGE TemplateHaskell #-}

-- base --------------------------------

import Control.Monad  ( forM_ )
import Data.Char      ( isDigit )
import Data.List      ( findIndices, isPrefixOf )
import Data.Word      ( Word8 )
import System.IO  ( FilePath, IOMode( ReadMode ), openFile )

-- containers --------------------------

import qualified Data.Tree as T

-- lens --------------------------------

import Control.Lens  ( (&), (^.), (?~), (<>~), makeLenses )

-- pipes imports -----------------------

import Pipes  ( Consumer, Pipe, Producer, (>->) )

import qualified Pipes.Prelude as P --  ( fold, filter, map )

-- transformers ------------------------

import Control.Monad.Trans.Class  ( lift )

-- fluffy ------------------------------

import Fluffy.Console.Getopt ( ArgArity(..), OptDesc(..)
                             , getOptions
                             )
import Fluffy.Data.List        ( splitOn, splitOn2, splitOnL, stripPre )
import Fluffy.Sys.Exit       ( handleDie, dieParse )
-- import Fluffy.Video.Handbrake.HBScan ( test7 )

-- handbrake ---------------------------

import Video.HandBrake.Title  ( Title, mkTitle )

--------------------------------------------------------------------------------

data Options = Options { } -- _raw :: Bool, _interval :: Maybe Int }

$( makeLenses ''Options )

options :: [OptDesc Options]
options = [ -- OptDesc [ "raw" ]  ""    "dump raw data"
            --                    (optBool raw)
          -- , OptDesc [ "n"   ]  "Int" "repeat every n seconds"
          --                      (optPosInt set_interval)
          ]

linesFromFile :: FilePath -> Producer String IO ()
linesFromFile fn = (lift $ openFile fn ReadMode) >>= P.fromHandle

markDepth :: Monad m => Pipe String (Int, String) m ()
markDepth = P.filter flt >-> P.map add_depth
  where flt x             = '+' == (head $ stripPre " " x)
        add_depth x       = let (a,b) = splitOn2 "+" x
                             in ((length a) `div` 2, tail b)

test7 :: FilePath -> IO [T.Tree String]
test7 fn = do
  x <- (P.toListM (linesFromFile fn >-> P.filter ((== '+') . head . dropWhile (== ' ')) >-> markDepth) :: IO [(Int,String)])
  let f as = let ((i',s') : as') = as
                 as'' = takeWhile ( \(x,_) -> x > i' ) as'
              in (s', map (flip drop as'') (findIndices (\ (i,_) -> i == i'+1) as''))
--  putStrLn $ T.drawTree $ T.unfoldTree f x
      titles :: [[(Int,String)]]
      titles = fmap (flip drop x) $ findIndices (\(n,_) -> n == 0) x
  return $ T.unfoldForest f titles

main :: IO()
main = handleDie $ do
  (opts, [arg]) <- getOptions ARGS_ONE ["<dvd>"] options (Options)
  putStrLn arg
  ts <- test7 arg
--  forM_ ts (putStrLn . T.drawTree)
  forM_ ts (\t -> case mkTitle t of
                    Left e -> print e
                    Right t' -> print t')






-- dump :: T.Tree String -> String
-- dump ts = let t = Title ts
--           in (T.rootLabel ts) ++ ": " ++ show t
