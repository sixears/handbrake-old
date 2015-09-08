{-# LANGUAGE OverloadedStrings #-}

import Video.HandBrake.PixelAspect  ( PixelAspect( PixelAspect ) )
import Video.HandBrake.REMatch      ( parse )

-- aeson -------------------------------

import Data.Aeson        ( encode, decode )

-- base --------------------------------

import Data.Ratio  ( (%) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit ( (@?=), testCase )

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [pixel_aspect]

pixel_aspect :: TestTree
pixel_aspect =
  testGroup "PixelAspect hunit tests"
            [
              testCase                                                 "parse" $
                parse "8/9" @?= Just (PixelAspect (8%9))
            , testCase                                                "encode" $
                encode (PixelAspect (4%3)) @?= "\"4/3\""
            , testCase                                                "decode" $
                -- if Ratio reduces 6/2 to 3/1, it should do it on both sides...
                decode "[ \"6/2\" ]" @?= Just [ PixelAspect (6%2) ]
            ]
