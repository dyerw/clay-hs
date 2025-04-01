{-# LANGUAGE StrictData #-}

module Clay.Render
  ( RenderCommand (..),
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
  { renderCommandBoundingBox :: Rect Float,
    renderCommandZIndex :: Int,
    renderCommandRenderData :: RenderData font
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
  let clayDecl = elementDeclarationToClayElementDeclaration e
  clayConfigureOpenElement clayDecl
  pure ()

elementDeclarationToClayElementDeclaration :: Element f e i -> ClayElementDeclaration
elementDeclarationToClayElementDeclaration = undefined

elementDeclarationToClayLayoutConfig :: Bool -> Style e -> ClayLayoutConfig
elementDeclarationToClayLayoutConfig isHovered style =
  ClayLayoutConfig
    { clayLayoutConfigSizing =
        ClaySizing
          { claySizingWidth = axisToClayAxis <$> xAxis,
            claySizingHeight = axisToClayAxis <$> yAxis
          },
      clayLayoutConfigChildAlignment = undefined,
      clayLayoutConfigChildGap = undefined,
      clayLayoutConfigPadding = undefined,
      clayLayoutConfigLayoutDirection = undefined
    }
  where
    sizingStyle = styleSizing style

    xAxis :: Maybe Sizing
    xAxis = resolveHoverableLast isHovered $ sizingStyleXAxis sizingStyle

    yAxis :: Maybe Sizing
    yAxis = resolveHoverableLast isHovered $ sizingStyleYAxis sizingStyle

    axisToClayAxis :: Sizing -> ClaySizingAxis
    axisToClayAxis a = case a of
      Fit -> ClaySizingAxis (Right 0) claySizingTypeFit
      Grow -> ClaySizingAxis (Right 0) claySizingTypeFit
      Percent f -> ClaySizingAxis (Right (CFloat f)) claySizingTypePercent
      Fixed i ->
        ClaySizingAxis
          (Left $ ClaySizingMinMax (fromIntegral i) (fromIntegral i))
          claySizingTypeFixed

clayRenderCommandToRenderCommand :: ClayRenderCommand -> RenderCommand font
clayRenderCommandToRenderCommand = undefined

data Rect a = Rect
  { rectX :: a,
    rectY :: a,
    rectWidth :: a,
    rectHeight :: a
  }

data TextRenderData font = TextRenderData
  { textRenderDataText :: Text,
    textRenderDataColor :: Color,
    textRenderDataFont :: font,
    textRenderDataFontSize :: Int,
    textRenderDataLetterSpacing :: Int,
    textRenderDataLineHeight :: Int
  }
