{-# LANGUAGE OverloadedStrings #-}

import Video.HandBrake.Autocrop       ( Autocrop( Autocrop ) )
import Video.HandBrake.DisplayAspect  ( DisplayAspect( DisplayAspect ) )
import Video.HandBrake.Duration       ( Duration( Duration ) )
import Video.HandBrake.FrameRate      ( FrameRate( FrameRate ) )
import Video.HandBrake.FrameSize      ( FrameSize( FrameSize ) )
import Video.HandBrake.PixelAspect    ( PixelAspect( PixelAspect ) )
import Video.HandBrake.REMatch        ( parse )

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
tests = testGroup "Tests" [ pixel_aspect, autocrop, display_aspect
                          , framerate, framesize, duration ]

----------------------------------------

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

----------------------------------------

autocrop :: TestTree
autocrop =
  testGroup "Autocrop hunit tests"
            [
              testCase                                                 "parse" $
                parse "2/3/4/5" @?= Just (Autocrop 2 3 4 5)
            , testCase                                                "encode" $
                encode (Autocrop 9 8 7 6) @?= "\"9/8/7/6\""
            , testCase                                                "decode" $
                decode "[ \"1/3/5/7\" ]" @?= Just [ Autocrop 1 3 5 7 ]
            ]

----------------------------------------

display_aspect :: TestTree
display_aspect =
  testGroup "DisplayAspect hunit tests"
            [
              testCase                                                 "parse" $
                parse "2.3" @?= Just (DisplayAspect 2.3)
            , testCase                                                "encode" $
                -- display aspect trims needless .0
                encode (DisplayAspect 9.0) @?= "\"9\""
            , testCase                                                "decode" $
                decode "[ \"1.7\" ]" @?= Just [ DisplayAspect 1.7 ]
            ]

----------------------------------------

framerate :: TestTree
framerate =
  testGroup "FrameRate hunit tests"
            [
              testCase                                                 "parse" $
                parse "3.2fps" @?= Just (FrameRate 3.2)
            , testCase                                                "encode" $
                -- framerate shown to two decimal places
                encode (FrameRate 8.9) @?= "\"8.90fps\""
            , testCase                                                "decode" $
                decode "[ \"7.1fps\" ]" @?= Just [ FrameRate 7.1 ]
            ]

----------------------------------------

framesize :: TestTree
framesize =
  testGroup "FrameSize hunit tests"
            [
              testCase                                                 "parse" $
                parse "16x9" @?= Just (FrameSize 16 9)
            , testCase                                                "encode" $
                encode (FrameSize 4 3) @?= "\"4x3\""
            , testCase                                                "decode" $
                decode "[ \"9x16\" ]" @?= Just [ FrameSize 9 16 ]
            ]

----------------------------------------

duration :: TestTree
duration =
  testGroup "Duration hunit tests"
            [
              testCase                                                 "parse" $
                parse "24:11:1" @?= Just (Duration 87061)
            , testCase                                                "encode" $
                encode (Duration 11111) @?= "\"3h05m11s\""
            , testCase                                                "decode" $
                decode "[ \"3h3m\" ]" @?= Just [ Duration 10980 ]
            ]

----------------------------------------
