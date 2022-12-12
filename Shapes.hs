{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (dart)

data Penrose2 n = Penrose2
  { long1 :: Trail' Line V2 n,
    long2 :: Trail' Line V2 n,
    short1 :: Trail' Line V2 n,
    short2 :: Trail' Line V2 n
  }

penrose2 long1 long2 short1 short2 =
  let phi = (sqrt 5 + 1) / 2
   in Penrose2
        { long1 = scaleLine phi long1,
          long2 = scaleLine phi long2,
          short1 = scaleLine 1 short1,
          short2 = scaleLine 1 short2
        }
  where
    scaleLine offset line =
      let offsets = lineOffsets line
          scale = offset / norm (sumV offsets)
       in lineFromOffsets [scale *^ d | d <- offsets]

kite Penrose2 {long1, long2, short1, short2} =
  let line = short1 <> rotate (108 @@ deg) (long1 <> rotate (108 @@ deg) (long2 <> rotate (108 @@ deg) short2))
   in closeLine line

dart Penrose2 {long1, long2, short1, short2} =
  let line = long2 <> rotate (144 @@ deg) (short2 <> rotate (324 @@ deg) (short1 <> rotate (144 @@ deg) long1))
   in closeLine line

main =
  let p2 = let line = hrule 1 in penrose2 line line line line
   in mainWith (kite p2 # strokeLoop # showOrigin ||| dart p2 # strokeLoop # showOrigin :: Diagram B)
