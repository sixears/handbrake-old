import Video.HandBrake.REMatch  ( REMatch(..), parseJSONString, toJSONString )

-- aeson -------------------------------

import qualified Data.Aeson.Types as DATypes

import Data.Aeson        ( encode, decode )
import Data.Aeson.Types  ( FromJSON( parseJSON ), Result( Success )
                         , ToJSON( toJSON ), Value( String ) )

-- base --------------------------------

-- import Control.Applicative  ( (<*), (<*>) )
-- import Data.Functor         ( (<$>) )

-- regex -------------------------------

import Text.Regex.Applicative         ( string )
import Text.Regex.Applicative.Common  ( decimal )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit ( (@?=), testCase )

-- text --------------------------------

import Data.Text  ( pack )

--------------------------------------------------------------------------------

data T = T Int Int
  deriving Eq

instance Show T where
  show (T x y) = show x ++ "/" ++ show y

instance REMatch T where
  re = T <$> decimal <*> (string "/" *> decimal)
  parse = parseREMatch "T"

instance FromJSON T where
  parseJSON = parseJSONString

instance ToJSON T where
    toJSON = toJSONString

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [rematch]

rematch :: TestTree
rematch =
  testGroup "REMatch hunit tests"
            [ testCase                                                 "parse" $
                parse "4/5" @?= Just (T 4 5)
            , testCase                                          "toJSONString" $
                toJSONString (T 7 9) @?= String (pack "7/9")
            , testCase                                       "parseJSONString" $
                DATypes.parse parseJSONString (String "2/7") @?= Success (T 2 7)
            , testCase                                                "encode" $
                encode (T 4 3) @?= "\"4/3\""
            , testCase                                                "decode" $
                decode "[ \"6/2\" ]" @?= Just [ T 6 2 ]
            ]
