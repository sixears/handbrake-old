{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Video.HandBrake.Audio          ( Audio( Audio ) )
import Video.HandBrake.Autocrop       ( Autocrop( Autocrop ) )
import Video.HandBrake.Cells          ( Cells( Cells ) )
import Video.HandBrake.Chapter        ( Chapter( Chapter ) )
import Video.HandBrake.Details        ( Details( Details ) )
import Video.HandBrake.DisplayAspect  ( DisplayAspect( DisplayAspect ) )
import Video.HandBrake.Duration       ( Duration( Duration ) )
import Video.HandBrake.FrameRate      ( FrameRate( FrameRate ) )
import Video.HandBrake.FrameSize      ( FrameSize( FrameSize ) )
import Video.HandBrake.PixelAspect    ( PixelAspect( PixelAspect ) )
import Video.HandBrake.Subtitle       ( Subtitle( Subtitle ) )

-- aeson -------------------------------

import Data.Aeson  ( FromJSON, ToJSON, encode, decode )

-- base --------------------------------

import Data.Function  ( on )
import Data.Maybe     ( fromJust )
import Data.Ratio     ( (%) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit ( (@?=), testCase )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck as QC

-- local imports -------------------------------------------

-- fluffy ------------------------------

import qualified  Fluffy.Text.Regex  as  RE

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ unitTests, propTests ]

unitTests :: TestTree
unitTests = testGroup "unitTests"
                      [ pixel_aspect, autocrop, display_aspect, framerate
                      , framesize, duration, audio, subtitle, cells, chapter
                      , details
                      ]

propTests :: TestTree
propTests = testGroup "propTests"
                      [ pixelAspectProps, displayAspectProps, frameRateProps
                      , frameSizeProps, detailsProps ]

parse :: (RE.REMatch r) => String -> Maybe r
parse = RE.parse

parse_show' :: (RE.REMatch a, Show a) => (Maybe a -> Maybe a -> Bool) -> a -> Bool
parse_show' p a = p ((parse . show) a) (Just a)

parse_show :: (RE.REMatch a, Show a, Eq a) => a -> Bool
parse_show = parse_show' (==)

decode_encode :: (FromJSON a, ToJSON a, Show a, Eq a) => a -> Bool
decode_encode = decode_encode' (==)

decode_encode' :: (FromJSON a, ToJSON a)
               => (Maybe [a] -> Maybe [a] -> Bool) -> a -> Bool
decode_encode' p a = p ((decode . encode) [a]) (Just [a])

class Floaty a where
  extract :: a -> Float

instance Floaty DisplayAspect where
  extract (DisplayAspect fl) = fl

instance Floaty FrameRate where
  extract (FrameRate fl) = fl

instance Floaty a => Floaty (Maybe a) where
  extract = extract . fromJust

instance Floaty a => Floaty (Maybe [a]) where
  extract = extract . head . fromJust

-- test floats for "equality", dealing with floating-point imprecision by looking
-- for a minimal difference relative to the size of the original value
floatIsh :: Float -> Float -> Bool
floatIsh a b |  b == 0     = (abs a) < 1e-6
             |  otherwise  = abs ((a - b) / b) < 1e-6

floatyIsh :: Floaty f => f -> f -> Bool
floatyIsh = floatIsh `on` extract

-- cannot use the instance of Floaty Maybe [a] without hitting overlapping instances
floatyIshML :: Floaty f => Maybe [f] -> Maybe [f] -> Bool
floatyIshML = floatIsh `on` (extract . head . fromJust)

--------------------------------------------------------------------------------

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

pixelAspectProps :: TestTree
pixelAspectProps =
  testGroup "pixelAspectProps"
    [ QC.testProperty                                             "show-parse" $
        \(pa :: PixelAspect) -> parse_show pa
    , QC.testProperty                                          "encode-decode" $
        \(pa :: PixelAspect) -> decode_encode pa
    ]

----------------------------------------

autocrop :: TestTree
autocrop =
  testGroup "Autocrop hunit tests"
            [ testCase                                                 "parse" $
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
            [ testCase                                                 "parse" $
                parse "2.3" @?= Just (DisplayAspect 2.3)
            , testCase                                                "encode" $
                -- display aspect trims needless .0
                encode (DisplayAspect 9.0) @?= "\"9\""
            , testCase                                                "decode" $
                decode "[ \"1.7\" ]" @?= Just [ DisplayAspect 1.7 ]
            ]

----------------------------------------

displayAspectProps :: TestTree
displayAspectProps =
  testGroup "displayAspectProps"
    [ QC.testProperty                                           "parse . show" $
        \(da :: DisplayAspect) -> parse_show' floatyIsh da
    , QC.testProperty                                        "decode . encode" $
        \(da :: DisplayAspect) -> decode_encode' floatyIshML da
    ]

----------------------------------------

framerate :: TestTree
framerate =
  testGroup "FrameRate hunit tests"
            [ testCase                                                 "parse" $
                parse "3.2fps" @?= Just (FrameRate 3.2)
            , testCase                                                "encode" $
                -- framerate shown to two decimal places
                encode (FrameRate 8.9) @?= "\"8.9fps\""
            , testCase                                                "decode" $
                decode "[ \"7.1fps\" ]" @?= Just [ FrameRate 7.1 ]
            ]

----------------------------------------

frameRateProps :: TestTree
frameRateProps =
  testGroup "frameRateProps"
    [ QC.testProperty                                             "show-parse" $
        \(fr :: FrameRate) -> parse_show fr
    , QC.testProperty                                          "encode-decode" $
        \(fr :: FrameRate) -> decode_encode fr
    ]

----------------------------------------

framesize :: TestTree
framesize =
  testGroup "FrameSize hunit tests"
            [ testCase                                                 "parse" $
                parse "16x9" @?= Just (FrameSize 16 9)
            , testCase                                                "encode" $
                encode (FrameSize 4 3) @?= "\"4x3\""
            , testCase                                                "decode" $
                decode "[ \"9x16\" ]" @?= Just [ FrameSize 9 16 ]
            ]

----------------------------------------

frameSizeProps :: TestTree
frameSizeProps =
  testGroup "frameSizeProps"
    [ QC.testProperty                                             "show-parse" $
        \(fr :: FrameSize) -> parse_show fr
    , QC.testProperty                                          "encode-decode" $
        \(fr :: FrameSize) -> decode_encode fr
    ]

----------------------------------------

duration :: TestTree
duration =
  testGroup "Duration hunit tests"
            [ testCase                                                 "parse" $
                parse "24:11:1"   @?= Just (Duration 87061)
            , testCase                                                 "parse" $
                parse "24h16m11s" @?= Just (Duration 87371)
            , testCase                                                "encode" $
                encode (Duration 11111) @?= "\"3h05m11s\""
            , testCase                                                "decode" $
                decode "[ \"3h3m\" ]" @?= Just [ Duration 10980 ]
            ]

----------------------------------------

audio :: TestTree
audio =
  let au_str = "1, Unknown (AC3) (2.0 ch) (iso639-2: und), 48000Hz, 192000bps"
   in testGroup "Audio hunit tests"
                [ testCase                                             "parse" $
                    parse au_str
                      @?= Just (Audio 1 "Unknown (AC3) (2.0 ch) (iso639-2: und)"
                                      48000 192000)
                , testCase                                            "encode" $
                    encode (Audio 2 "blomquist" 23 77)
                      @?= "\"Audio 2: 23Hz 77bps # blomquist\""
                , testCase                                            "decode" $
                    decode "[ \"Audio 3: 45Hz 88bps # quistbop\" ]"
                      @?= Just [ Audio 3 "quistbop" 45 88 ]
                ]

----------------------------------------

subtitle :: TestTree
subtitle =
  testGroup "Subtitle hunit tests"
            [ testCase                                                 "parse" $
                parse "2, some stuff" @?= Just (Subtitle 2 "some stuff")
            , testCase                                                "encode" $
                encode (Subtitle 3 "more stuff") @?= "\"Subtitle 3 # more stuff\""
            , testCase                                                "decode" $
                decode "[ \"3, also nonsense\" ]"
                  @?= Just [ Subtitle 3 "also nonsense" ]
            ]

----------------------------------------

cells :: TestTree
cells =
  testGroup "Cells hunit tests"
            [ testCase                                                 "parse" $
                parse "0->1" @?= Just (Cells 0 1)
            , testCase                                                "encode" $
                encode (Cells 0 1) @?= "\"0->1\""
            , testCase                                                "decode" $
                decode "[ \"4->6\" ]" @?= Just [ Cells 4 6 ]
            , testCase                                                  "show" $
                show (Cells 0 2) @?= "0->2"
            ]

----------------------------------------

chapter :: TestTree
chapter =
  testGroup "Chapter hunit tests"
            [ testCase                                                 "parse" $
                parse "4: cells 0->1, 256 blocks, duration 103:48:57"
                  @?= Just (Chapter 4 (Cells 0 1) 256 (Duration 373737))
            , testCase                                                "encode" $
                encode (Chapter 4 (Cells 0 1) 256 (Duration 3737))
                  @?= "\"Chapter  4:  1h02m17s     256 blocks cells 0->1\""
            , testCase                                                "decode" $
                decode "[ \"Chapter 13: 1h23m45s 12345 blocks cells 4->6\" ]"
                  @?= Just [ Chapter 13 (Cells 4 6) 12345 (Duration 5025) ]
            , testCase                                                  "show" $
                show (Chapter 5 (Cells 0 2) 257 (Duration 3600))
                  @?= "Chapter  5:        1h     257 blocks cells 0->2"
            ]

----------------------------------------

details :: TestTree
details =
  testGroup "Details hunit tests"
            [ testCase                                                 "parse" $
                parse "16x9, pixel aspect: 8/9, display aspect: 2.3, 7.1fps"
                  @?= Just (Details (FrameSize 16 9)
                                    (PixelAspect (8%9))
                                    (DisplayAspect 2.3)
                                    (FrameRate 7.1)
                           )
            , testCase                                                "encode" $
                encode (Details (FrameSize 9 16)    (PixelAspect (9%8))
                                (DisplayAspect 3.2) (FrameRate 1.7))
                  @?= "\"9x16, 9/8, 3.2, 1.7fps\""
            , testCase                                                "decode" $
                decode "[ \"8x6, 8/6, 1.0, 06.60fps\" ]"
                  @?= Just [ Details (FrameSize 8 6)   (PixelAspect (4%3))
                                     (DisplayAspect 1) (FrameRate 6.6)     ]
            , testCase                                                  "show" $
                show (Details (FrameSize 8 6)   (PixelAspect (8%6))
                              (DisplayAspect 1.0) (FrameRate 06.60)     )
                  @?= "8x6, 4/3, 1, 6.6fps"
            ]

----------------------------------------

diffDetails :: Maybe Details -> Maybe Details -> Bool
diffDetails (Just (Details fs1 pa1 da1 fr1)) (Just (Details fs2 pa2 da2 fr2)) =
  and [ fs1 == fs2
      , pa1 == pa2
      , floatyIsh da1 da2
      , fr1 == fr2
      ]
diffDetails _ _ = False

detailsProps :: TestTree
detailsProps =
  testGroup "detailsProps"
    [ QC.testProperty                                             "show-parse" $
        \(dt :: Details) -> parse_show' diffDetails dt
    , QC.testProperty                                          "encode-decode" $
        \(dt :: Details) -> decode_encode' (diffDetails `on` (fmap head)) dt
    ]

