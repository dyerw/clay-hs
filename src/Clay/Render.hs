{-# LANGUAGE StrictData #-}

module Clay.Render (
  RenderCommand (..),
  RenderData (..),
  calculateLayout,
  Rect (..),
  TextRenderData (..),
)
where

import Clay.Layout
import Clay.Raw
import Clay.Raw.Types
import Data.Text
import Foreign.C.Types

data RenderCommand font = RenderCommand
  { renderCommandBoundingBox :: Rect Float
  , renderCommandZIndex :: Int
  , renderCommandRenderData :: RenderData font
  }

newtype RenderData font
  = TextData (TextRenderData font)

calculateLayout :: Element f e i -> IO ([RenderCommand font], [e])
calculateLayout root = do
  clayBeginLayout
  declareElement root
  clayCommands <- clayEndLayout >>= arrayToList
  let commands = clayRenderCommandToRenderCommand <$> clayCommands
  pure (commands, [])

declareElement :: Element f e i -> IO ()
declareElement e = do
  clayOpenElement
  configureElement e
  clayCloseElement

configureElement :: Element f e i -> IO ()
configureElement e = do
  let clayDecl = toClayElementDeclaration e undefined
  clayConfigureOpenElement clayDecl
  pure ()

toClayElementDeclaration :: Element f e i -> ElementContext -> ClayElementDeclaration
toClayElementDeclaration = undefined

toClayLayoutConfig :: ElementStyle -> ElementContext -> ClayLayoutConfig
toClayLayoutConfig style ctx =
  ClayLayoutConfig
    { clayLayoutConfigSizing = toClaySizing style ctx
    , clayLayoutConfigChildAlignment = undefined
    , clayLayoutConfigChildGap = undefined
    , clayLayoutConfigPadding = undefined
    , clayLayoutConfigLayoutDirection = undefined
    }

toClaySizing :: ElementStyle -> ElementContext -> ClaySizing
toClaySizing style ctx =
  let xSizing = toMaybe $ getSizing style ctx XAxis
      ySizing = toMaybe $ getSizing style ctx YAxis
      toClayAxis a = case a of
        Fit -> ClaySizingAxis (Right 0) claySizingTypeFit
        Grow -> ClaySizingAxis (Right 0) claySizingTypeFit
        Percent f -> ClaySizingAxis (Right (CFloat f)) claySizingTypePercent
        Fixed i ->
          ClaySizingAxis
            (Left $ ClaySizingMinMax (fromIntegral i) (fromIntegral i))
            claySizingTypeFixed
   in ClaySizing
        { claySizingWidth = toClayAxis <$> xSizing
        , claySizingHeight = toClayAxis <$> ySizing
        }

clayRenderCommandToRenderCommand :: ClayRenderCommand -> RenderCommand font
clayRenderCommandToRenderCommand = undefined

data Rect a = Rect
  { rectX :: a
  , rectY :: a
  , rectWidth :: a
  , rectHeight :: a
  }

data TextRenderData font = TextRenderData
  { textRenderDataText :: Text
  , textRenderDataColor :: Color
  , textRenderDataFont :: font
  , textRenderDataFontSize :: Int
  , textRenderDataLetterSpacing :: Int
  , textRenderDataLineHeight :: Int
  }
