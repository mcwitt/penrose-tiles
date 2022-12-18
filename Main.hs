module Main where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Path.Boolean qualified as B

u, v, w :: V2 Double
u = V2 1 0
v = u # rotate (108 @@ deg)
w = u # scale (2.0 * sinA (18 @@ deg)) # rotate (36 @@ deg)

l1, l2, l3 :: Trail V2 Double
l1 = fromOffsets [u]
l2 = fromOffsets [v]
l3 = fromOffsets [w, w # rotate (36 @@ deg)]

main =
  let nr = 4
      nc = 25

      cuts =
        [mconcat (replicate nc l1) `at` P (fromIntegral i * v) | i <- [0 .. nr]]
          <> [mconcat (replicate nr l2) `at` P (fromIntegral i * u) | i <- [0 .. nc]]
          <> [mconcat (replicate (min (nr - i) nc) l3) `at` P (fromIntegral i * v) | i <- [0 .. nr]]
          <> [mconcat (replicate (min (nc - i) nr) l3) `at` P (fromIntegral i * u) | i <- [1 .. nc]]

      rad1 = norm w
      rad2 = 1 - norm w
      rad3 = 1 / (2 - rad1 ** 2) - rad2
      circleWidth = (rad2 - rad3) / 2

      mkCirclesKite p =
        B.union
          Winding
          ( annularWedge
              (rad1 + circleWidth)
              (rad1 - circleWidth)
              (xDir # rotate (-72 @@ deg))
              (72 @@ deg)
              # translate (p + v)
              <> annularWedge
                (rad2 + circleWidth)
                (rad2 - circleWidth)
                (direction (w # rotate (36 @@ deg)))
                (144 @@ deg)
                # translate (p + w)
          )
          <> arc (direction (V2 1 0)) (-72 @@ deg) # scale rad1 # translate (p + v)

      mkCirclesDart p =
        B.union
          Winding
          ( annularWedge
              (rad2 + circleWidth)
              (rad2 - circleWidth)
              (xDir # rotate (108 @@ deg))
              (72 @@ deg)
              # translate (p + u)
              <> ( annularWedge
                     (rad3 + circleWidth)
                     (rad3 - circleWidth)
                     (direction (w # rotate (-180 @@ deg)))
                     (216 @@ deg)
                     # translate (p + w)
                 )
          )
          <> arc (direction (V2 (-1) 0)) (-72 @@ deg) # scale rad2 # translate (p + u)

      grid = [fromIntegral i * v + fromIntegral j * u | i <- [0 .. nr - 1], j <- [0 .. nc - 1]]
      circles = foldMap (<$> grid) [mkCirclesKite, mkCirclesDart]

      box =
        let width = fromIntegral nc + dx
            dx = fromIntegral nr * sinA (18 @@ deg)
            height = fromIntegral nr * cosA (18 @@ deg)
            margin = 0.25
         in rect (width + 2 * margin) (height + 2 * margin) # translate (V2 (width / 2 - dx) (height / 2))
   in mainWith
        ( mconcat
            [ stroke cuts,
              stroke circles # lc red,
              box
            ]
            # lwL 0.01 ::
            Diagram B
        )
