{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module Clay.Render where

import Clay.Color
import Clay.Declaration
import Clay.Geometry
import Clay.Layout
import Clay.Layout.Config (ConfigValue (..))
import Clay.Raw
import Clay.Raw.Types
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Control.Monad.RWS (local)
import Data.Maybe (fromMaybe)
import Data.Text
import Data.Void (Void, absurd)
import Foreign
import Foreign.C.Types

newtype RenderContext = RenderContext {renderContextClayContext :: Ptr ClayContext}

initialize :: Size Int -> IO RenderContext
initialize initialDimensions = do
  minMemorySize <- fromIntegral <$> clayMinMemorySize
  arenaMem <- mallocBytes minMemorySize
  arena <-
    clayCreateArenaWithCapacityAndMemory
      (fromIntegral minMemorySize)
      arenaMem
  let handleError = print
  ctxPtr <- clayInitialize arena (toClayDimensions initialDimensions) handleError
  pure (RenderContext ctxPtr)

calculateLayout :: (HasSourceDimensions i) => Element e f i c -> InputState -> IO ([RenderCommand f i c], [e])
calculateLayout root' input = do
  updateInput input

  clayBeginLayout
  events <- declareRoot root' input

  clayCommands <- clayEndLayout >>= arrayToList

  let commands = clayRenderCommandToRenderCommand <$> clayCommands
  pure (commands, events)

newtype NoImage = NoImage Void deriving (Eq, Show)

instance HasSourceDimensions NoImage where
  sourceDimensions (NoImage v) = absurd v

declareRoot :: (HasSourceDimensions i) => Element e f i c -> InputState -> IO [e]
declareRoot root' input = do
  clayOpenElement
  ctx <- rootContext root'
  (_, events) <- runDeclaration configure ctx
  clayCloseElement
  pure events
  where
    rootContext :: Element e f i c -> IO (ElementDeclarationContext e f i c)
    rootContext root'' = do
      eid <- calculateClayElementId Nothing root'
      isHovered <- clayHovered
      pure $ ElementDeclarationContext root'' (CommonDeclarationContextFields input isHovered Nothing eid)

declareChild :: (HasSourceDimensions i) => Clay e f i c -> ElementDeclaration e f i c ()
declareChild child = do
  case child of
    ClayElement childElement -> do
      liftIO clayOpenElement
      parentId <- getContextElementId
      childId <- calculateChildId childElement
      childHovered <- liftIO clayHovered
      local
        ( \(ElementDeclarationContext _ common) ->
            ElementDeclarationContext
              childElement
              common
                { declarationContextElementId = childId,
                  declarationContextParentId = parentId,
                  declarationContextIsHovered = childHovered
                }
        )
        configure
      liftIO clayCloseElement
    -- TODO: Text declarations
    ClayText childText -> undefined

configure :: (HasSourceDimensions i) => ElementDeclaration e f i c ()
configure = do
  configureElement
  children <- getContextChildren
  forM_ children declareChild

updateInput :: InputState -> IO ()
updateInput (InputState (pointerX, pointerY) pointerDown (layoutWidth, layoutHeight)) = do
  claySetPointerState
    (ClayVector2 (CFloat $ fromIntegral pointerX) (CFloat $ fromIntegral pointerY))
    (CBool $ if pointerDown then 1 else 0)

  claySetLayoutDimensions
    (ClayDimensions (CFloat $ fromIntegral layoutWidth) (CFloat $ fromIntegral layoutHeight))

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
    <*> pure nullPtr

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

-- TODO: Implement
getClayFloatingElementConfig :: ElementDeclaration e f i c (Maybe ClayFloatingElementConfig)
getClayFloatingElementConfig = pure Nothing

-- TODO: Implement
getClayCustomElementConfig :: ElementDeclaration e f i c (Maybe ClayCustomElementConfig)
getClayCustomElementConfig = pure Nothing

-- TODO: Implement
getClayScrollElementConfig :: ElementDeclaration e f i c (Maybe ClayScrollElementConfig)
getClayScrollElementConfig = pure Nothing

-- TODO: Implement
getClayBorderElementConfig :: ElementDeclaration e f i c (Maybe ClayBorderElementConfig)
getClayBorderElementConfig = pure Nothing

clayRenderCommandToRenderCommand :: ClayRenderCommand -> RenderCommand f i c
clayRenderCommandToRenderCommand (ClayRenderCommand boundingBox renderData _ zIndex _ _) =
  case renderData of
    ( ClayRenderDataRectangle
        (ClayRectangleRenderData color cornerRadius')
      ) ->
        RenderCommand
          (fromClayBoundingBox boundingBox)
          ( RenderRect $
              RenderRectCommand
                (fromClayColor color)
                (fromClayCornerRadius cornerRadius')
          )
    ( ClayRenderDataText
        ( ClayTextRenderData
            txt
            color
            font
            fontSize
            letterSpacing
            height
          )
      ) -> undefined
    ( ClayRenderDataImage
        ( ClayImageRenderData
            tint
            cornerRadius
            sourceDims
            img
          )
      ) -> undefined
    ( ClayRenderDataCustom
        ( ClayCustomRenderData
            color
            cornerRadius
            custom
          )
      ) -> undefined
    ( ClayRenderDataBorder
        ( ClayBorderRenderData
            color
            cornerRadius
            sideWidths
          )
      ) -> undefined
    (ClayRenderDataScroll (ClayScrollRenderData _ _)) -> undefined
    ClayRenderDataNone -> undefined

data RenderCommand f i c = RenderCommand
  { renderCommandBoundingBox :: Rect Float,
    renderCommandCommand :: Command f i c
  }
  deriving (Eq, Show)

data Command f i c
  = RenderRect RenderRectCommand
  | RenderBorder RenderBorderCommand
  | RenderText (RenderTextCommand f)
  | RenderImage (RenderImageCommand i)
  | RenderCustom (RenderCustomCommand c)
  deriving (Eq, Show)

data RenderRectCommand = RenderRectCommand
  { renderRectColor :: Color,
    renderRectCornerRadius :: Corners Float
  }
  deriving (Eq, Show)

data RenderBorderCommand = RenderBorderCommand
  { renderBorderCommandColor :: Color,
    renderBorderCommandCornerRadius :: Corners Float,
    renderBorderCommandWidth :: Sides Float
  }
  deriving (Eq, Show)

data RenderTextCommand f = TextRenderData
  { renderTextCommandText :: Text,
    renderTextCommandColor :: Color,
    renderTextCommandFont :: f,
    renderTextCommandFontSize :: Int,
    renderTextCommandLetterSpacing :: Int,
    renderTextCommandLineHeight :: Int
  }
  deriving (Eq, Show)

data RenderImageCommand i = RenderImageCommand
  { renderImageCommandTint :: Color,
    renderImageCommandCornerRadius :: Corners Float,
    renderImageCommandSourceDimensions :: Size Float,
    renderImageCommandImage :: i
  }
  deriving (Eq, Show)

data RenderCustomCommand c = RenderCustomCommand
  { renderCustomCommandColor :: Color,
    renderCustomCommandCornerRadius :: Corners Float,
    renderCustomCommandCustom :: c
  }
  deriving (Eq, Show)
