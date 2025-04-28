module Main where

import qualified Clay.Color as CC
import Clay.Declaration
import Clay.Geometry
import Clay.Layout
import Clay.Render
import Debug.Trace
import qualified Graphics.Gloss as G

main :: IO ()
main = do
  _ <- initialize (Size glossViewWidth glossViewHeight)
  (renderCommands, _) <- calculateLayout layout input
  print renderCommands
  let picture = renderPicture renderCommands
  G.display
    ( G.InWindow
        "Nice Window"
        (glossViewWidth, glossViewHeight)
        (0, 0)
    )
    G.white
    (glossTopLeftTranslate picture)

glossViewWidth :: Int
glossViewWidth = 500

glossViewHeight :: Int
glossViewHeight = 500

glossTopLeftTranslate :: G.Picture -> G.Picture
glossTopLeftTranslate =
  G.Translate
    (-1 * fromIntegral glossViewWidth / 2)
    (-1 * fromIntegral glossViewHeight / 2)

input :: InputState
input = InputState (0, 0) False (glossViewWidth, glossViewHeight)

type Event = ()

type Font = ()

type Custom = ()

type GlossElement = Element Event Font NoImage Custom

layout :: GlossElement
layout =
  root
    (backgroundColor CC.blue <> growY_ <> fixedX (viewWidth / 2))
    [ element_ (backgroundColor CC.red <> fixed 100) []
    ]

renderPicture :: [RenderCommand Font NoImage Custom] -> G.Picture
renderPicture = foldMap renderCommandToPicture

renderCommandToPicture :: RenderCommand Font NoImage Custom -> G.Picture
renderCommandToPicture cmd = case cmd of
  (RenderCommand boundingBox (RenderRect (RenderRectCommand color cornerRadius))) ->
    G.color (toGlossColor color) (toGlossRect boundingBox)
  _ -> undefined

toGlossRect :: Rect Float -> G.Picture
toGlossRect (Rect (Size w h) (Position x y)) =
  traceShowId $
    G.polygon
      [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

toGlossColor :: CC.Color -> G.Color
toGlossColor (CC.Color r g b a) = G.makeColor (toF r) (toF g) (toF b) (toF a)
  where
    toF x = fromIntegral x / 255
