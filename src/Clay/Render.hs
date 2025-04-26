{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module Clay.Render where

import Clay.Color
import Clay.Declaration
import Clay.Layout
import Clay.Layout.Config (ConfigValue (..))
import Clay.Raw
import Clay.Raw.Types
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Text
import Foreign (Word16)
import Foreign.C.Types
import Foreign.Ptr (Ptr)

calculateLayout :: (HasSourceDimensions i) => Element e f i c -> InputState -> IO ([RenderCommand f], [e])
calculateLayout root input = do
  updateInput input

  clayBeginLayout
  events <- declareRoot root input

  clayCommands <- clayEndLayout >>= arrayToList

  let commands = clayRenderCommandToRenderCommand <$> clayCommands
  pure (commands, events)

declareRoot :: (HasSourceDimensions i) => Element e f i c -> InputState -> IO [e]
declareRoot root input = do
  ctx <- rootContext root
  (_, events) <- runDeclaration elementDeclaration ctx
  pure events
  where
    rootContext :: Element e f i c -> IO (ElementDeclarationContext e f i c)
    rootContext root' = do
      eid <- calculateClayElementId Nothing root'
      isHovered <- clayHovered
      pure $ ElementDeclarationContext root' (CommonDeclarationContextFields input isHovered Nothing eid)

elementDeclaration :: (HasSourceDimensions i) => ElementDeclaration e f i c ()
elementDeclaration = do
  liftIO clayOpenElement
  configureElement
  liftIO clayCloseElement

updateInput :: InputState -> IO ()
updateInput (InputState (pointerX, pointerY) pointerDown (layoutWidth, layoutHeight)) = do
  claySetPointerState
    (ClayVector2 (CFloat $ fromIntegral pointerX) (CFloat $ fromIntegral pointerY))
    (CBool $ if pointerDown then 1 else 0)

  claySetLayoutDimensions
    (ClayDimensions (CFloat layoutWidth) (CFloat layoutHeight))

data RenderCommand font = RenderCommand
  { renderCommandBoundingBox :: Rect Float,
    renderCommandZIndex :: Int,
    renderCommandRenderData :: RenderData font
  }

newtype RenderData font
  = TextData (TextRenderData font)

configureElement :: (HasSourceDimensions i) => ElementDeclaration e f i c ()
configureElement = do
  clayDecl <- getClayElementDeclaration
  liftIO $ clayConfigureOpenElement clayDecl

getClayElementDeclaration :: (HasSourceDimensions i) => ElementDeclaration e f i c ClayElementDeclaration
getClayElementDeclaration =
  ClayElementDeclaration
    <$> getContextElementId
    <*> getClayLayoutConfig
    <*> getClayBackgroundColor
    <*> getClayCornerRadius
    <*> getClayImageElementConfig
    <*> getClayFloatingElementConfig
    <*> getClayCustomElementConfig
    <*> getClayScrollElementConfig
    <*> getClayBorderElementConfig
    <*> getClayUserData

getClayLayoutConfig :: ElementDeclaration e f i c (Maybe ClayLayoutConfig)
getClayLayoutConfig =
  Just
    <$> ( ClayLayoutConfig
            <$> getClayLayoutConfigSizing
            <*> getClayLayoutConfigPadding
            <*> getClayLayoutConfigChildGap
            <*> getClayLayoutConfigChildAlignment
            <*> getClayLayoutConfigLayoutDirection
        )

getClayLayoutConfigSizing :: ElementDeclaration e f i c ClaySizing
getClayLayoutConfigSizing = ClaySizing <$> getClaySizingWidth <*> getClaySizingHeight

getClaySizingHeight :: ElementDeclaration e f i c (Maybe ClaySizingAxis)
getClaySizingHeight = getClaySizingAxis (sizingStyleYAxis . styleSizing)

getClaySizingWidth :: ElementDeclaration e f i c (Maybe ClaySizingAxis)
getClaySizingWidth = getClaySizingAxis (sizingStyleXAxis . styleSizing)

getClaySizingAxis :: (ElementStyleValues -> ConfigValue Sizing) -> ElementDeclaration e f i c (Maybe ClaySizingAxis)
getClaySizingAxis getSizing = do
  sizing <- getConfigValue getSizing
  axisSize <- traverse getClaySizingAxisSize sizing
  let axisType = toClaySizingAxisType <$> sizing
  pure $
    ClaySizingAxis
      <$> axisSize
      <*> axisType

getClaySizingAxisSize :: Sizing -> ElementDeclaration e f i c (Either ClaySizingMinMax CFloat)
getClaySizingAxisSize (Fit b) = Left <$> getClaySizingMinMax b
getClaySizingAxisSize (Grow b) = Left <$> getClaySizingMinMax b
getClaySizingAxisSize (Percent f) = pure $ Right $ CFloat f
getClaySizingAxisSize (Fixed c) = do
  i <- resolveLayoutCalculation c
  pure $ Right $ CFloat i

getClaySizingMinMax :: SizingBounds -> ElementDeclaration e f i c ClaySizingMinMax
getClaySizingMinMax (SizingBounds min' max') = do
  resolvedMin <- traverse resolveLayoutCalculation (toMaybe min')
  resolvedMax <- traverse resolveLayoutCalculation (toMaybe max')
  pure $
    ClaySizingMinMax
      (CFloat <$> resolvedMin)
      (CFloat <$> resolvedMax)

toClaySizingAxisType :: Sizing -> ClaySizingType
toClaySizingAxisType (Fit _) = claySizingTypeFit
toClaySizingAxisType (Grow _) = claySizingTypeGrow
toClaySizingAxisType (Percent _) = claySizingTypePercent
toClaySizingAxisType (Fixed _) = claySizingTypeFixed

getClayLayoutConfigPadding :: ElementDeclaration e f i c ClayPadding
getClayLayoutConfigPadding =
  ClayPadding
    <$> getWord16ValueZero (paddingStyleLeft . stylePadding)
    <*> getWord16ValueZero (paddingStyleRight . stylePadding)
    <*> getWord16ValueZero (paddingStyleTop . stylePadding)
    <*> getWord16ValueZero (paddingStyleBottom . stylePadding)

getClayLayoutConfigChildGap :: ElementDeclaration e f i c Word16
getClayLayoutConfigChildGap = getWord16ValueZero (childStyleChildGap . styleChild)

getClayLayoutConfigChildAlignment :: ElementDeclaration e f i c ClayChildAlignment
getClayLayoutConfigChildAlignment =
  ClayChildAlignment
    <$> getClayChildLayoutAlignmentX
    <*> getClayChildLayoutAlignmentY

getClayChildLayoutAlignmentX :: ElementDeclaration e f i c ClayLayoutAlignmentX
getClayChildLayoutAlignmentX =
  maybe
    clayAlignXLeft
    toClayLayoutAlignmentX
    <$> getConfigValue (childStyleChildAlignX . styleChild)

getClayChildLayoutAlignmentY :: ElementDeclaration e f i c ClayLayoutAlignmentY
getClayChildLayoutAlignmentY =
  maybe
    clayAlignYTop
    toClayLayoutAlignmentY
    <$> getConfigValue (childStyleChildAlignY . styleChild)

toClayLayoutAlignmentX :: XAlignment -> ClayLayoutAlignmentX
toClayLayoutAlignmentX AlignXCenter = clayAlignXCenter
toClayLayoutAlignmentX AlignXLeft = clayAlignXLeft
toClayLayoutAlignmentX AlignXRight = clayAlignXRight

toClayLayoutAlignmentY :: YAlignment -> ClayLayoutAlignmentY
toClayLayoutAlignmentY AlignYCenter = clayAlignYCenter
toClayLayoutAlignmentY AlignYTop = clayAlignYTop
toClayLayoutAlignmentY AlignYBottom = clayAlignYBottom

getClayLayoutConfigLayoutDirection :: ElementDeclaration e f i c ClayLayoutDirection
getClayLayoutConfigLayoutDirection = do
  direction <- getConfigValue (directionStyleDirection . styleDirection)
  pure $ case direction of
    Just TopToBottom -> clayTopToBottom
    _ -> clayLeftToRight

getClayBackgroundColor :: ElementDeclaration e f i c (Maybe ClayColor)
getClayBackgroundColor = fmap toClayColor <$> getConfigValue styleBackgroundColor

getClayCornerRadius :: ElementDeclaration e f i c (Maybe ClayCornerRadius)
getClayCornerRadius =
  Just
    <$> ( ClayCornerRadius
            <$> (fromMaybe 0 <$> getCFloatValue (cornerRadiusStyleTopLeft . styleCornerRadius))
            <*> (fromMaybe 0 <$> getCFloatValue (cornerRadiusStyleTopRight . styleCornerRadius))
            <*> (fromMaybe 0 <$> getCFloatValue (cornerRadiusStyleBottomLeft . styleCornerRadius))
            <*> (fromMaybe 0 <$> getCFloatValue (cornerRadiusStyleBottomRight . styleCornerRadius))
        )

class HasSourceDimensions i where
  sourceDimensions :: i -> (Float, Float)

getClayImageElementConfig :: (HasSourceDimensions i) => ElementDeclaration e f i c (Maybe ClayImageElementConfig)
getClayImageElementConfig = do
  ele <- getDeclarationElement
  case ele of
    ImageElement (ImageConfig {imageConfigImage}) -> do
      ptr <- registerImage imageConfigImage
      let (srcW, srcH) = sourceDimensions imageConfigImage
      pure $ Just $ ClayImageElementConfig ptr (ClayDimensions (CFloat srcW) (CFloat srcH))
    _ -> pure Nothing

getClayFloatingElementConfig :: ElementDeclaration e f i c (Maybe ClayFloatingElementConfig)
getClayFloatingElementConfig = undefined

getClayCustomElementConfig :: ElementDeclaration e f i c (Maybe ClayCustomElementConfig)
getClayCustomElementConfig = undefined

getClayScrollElementConfig :: ElementDeclaration e f i c (Maybe ClayScrollElementConfig)
getClayScrollElementConfig = undefined

getClayBorderElementConfig :: ElementDeclaration e f i c (Maybe ClayBorderElementConfig)
getClayBorderElementConfig = undefined

getClayUserData :: ElementDeclaration e f i c (Ptr ())
getClayUserData = undefined

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
