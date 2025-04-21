{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Clay.Render
  ( RenderCommand (..),
    InputState (..),
    RenderData (..),
    calculateLayout,
    Rect (..),
    TextRenderData (..),
  )
where

import Clay.Layout
import Clay.Raw
import Clay.Raw.Types
import Control.Monad.IO.Class
import Control.Monad.RWS.Strict (MonadReader (ask), RWST (runRWST), asks)
import Data.Text
import Foreign.C.Types
import Foreign.Ptr (Ptr)

newtype Declaration e f i a = Declaration
  { unDeclaration ::
      RWST
        (DeclarationContext e f i)
        [e]
        ()
        IO
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (DeclarationContext e f i)
    )

declareLayout :: Element e f i -> InputState -> IO [e]
declareLayout root input = do
  ctx <- rootContext root
  (_, _, events) <-
    runRWST
      (unDeclaration elementDeclaration)
      ctx
      ()
  pure events
  where
    rootContext :: Element e f i -> IO (DeclarationContext e f i)
    rootContext root' = do
      eid <- calculateClayElementId Nothing root'
      isHovered <- clayHovered
      pure $ DeclarationContext input root' isHovered Nothing eid

elementDeclaration :: Declaration e f i ()
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
    (ClayDimensions (CFloat $ fromIntegral layoutWidth) (CFloat $ fromIntegral layoutHeight))

calculateLayout :: Element e f i -> InputState -> IO ([RenderCommand f], [e])
calculateLayout root input = do
  updateInput input

  clayBeginLayout
  events <- declareLayout root input

  clayCommands <- clayEndLayout >>= arrayToList

  let commands = clayRenderCommandToRenderCommand <$> clayCommands
  pure (commands, events)

data RenderCommand font = RenderCommand
  { renderCommandBoundingBox :: Rect Float,
    renderCommandZIndex :: Int,
    renderCommandRenderData :: RenderData font
  }

newtype RenderData font
  = TextData (TextRenderData font)

data InputState = InputState
  { inputStatePointerLocation :: (Int, Int),
    inputStatePointerDown :: Bool,
    inputStateLayoutDimensions :: (Int, Int)
  }

data DeclarationContext e f i = DeclarationContext
  { declarationContextInput :: InputState,
    declarationContextElement :: Element e f i,
    declarationContextIsHovered :: Bool,
    declarationContextParentId :: Maybe ClayElementId,
    declarationContextElementId :: Maybe ClayElementId
  }

calculateClayElementId :: Maybe ClayElementId -> Element e f i -> IO (Maybe ClayElementId)
calculateClayElementId parentId ele = do
  clayStringId <- traverse toClayString (elementId ele)
  let parentIdHash = maybe 0 clayElementIdId parentId
  traverse (\csi -> clayHashString csi 0 parentIdHash) clayStringId

getStyleValue :: (ElementStyleValues -> StyleValue b) -> Declaration e f i (Maybe b)
getStyleValue f = do
  style <- asks $ elementStyle . declarationContextElement
  isHovered <- asks declarationContextIsHovered
  let baseValue = (f . styleBase) style
  let hoveredValue = (f . styleHovered) style
  pure $
    toMaybe $
      if isHovered
        then baseValue <> hoveredValue
        else baseValue

resolveLayoutSizeValue :: LayoutSizeValue -> Declaration e f i Float
resolveLayoutSizeValue v = do
  (viewWidth, viewHeight) <- asks (inputStateLayoutDimensions . declarationContextInput)
  pure $ case v of
    Pixels i -> fromIntegral i
    Var ViewHeight -> fromIntegral viewHeight
    AddVar ViewHeight i -> fromIntegral $ viewWidth + i
    SubtractVar ViewHeight i -> fromIntegral $ viewHeight - i
    MultiplyVar ViewHeight i -> fromIntegral $ viewHeight * i
    DivideVar ViewHeight i -> fromIntegral viewHeight / fromIntegral i
    Var ViewWidth -> fromIntegral viewWidth
    AddVar ViewWidth i -> fromIntegral $ viewWidth + i
    SubtractVar ViewWidth i -> fromIntegral $ viewWidth - i
    MultiplyVar ViewWidth i -> fromIntegral $ viewWidth * i
    DivideVar ViewWidth i -> fromIntegral viewWidth / fromIntegral i

configureElement :: Declaration e f i ()
configureElement = do
  clayDecl <- getClayElementDeclaration
  liftIO $ clayConfigureOpenElement clayDecl

getClayElementDeclaration :: Declaration e f i ClayElementDeclaration
getClayElementDeclaration =
  ClayElementDeclaration
    <$> getClayElementId
    <*> getClayLayoutConfig
    <*> getClayBackgroundColor
    <*> getClayCornerRadius
    <*> getClayImageElementConfig
    <*> getClayFloatingElementConfig
    <*> getClayCustomElementConfig
    <*> getClayScrollElementConfig
    <*> getClayBorderElementConfig
    <*> getClayUserData

getClayElementId :: Declaration e f i (Maybe ClayElementId)
getClayElementId = asks declarationContextElementId

getClayLayoutConfig :: Declaration e f i (Maybe ClayLayoutConfig)
getClayLayoutConfig =
  ClayLayoutConfig
    <$> getClayLayoutConfigSizing
    <*> getClayLayoutConfigPadding
    <*> getClayLayoutConfigChildGap
    <*> getClayLayoutConfigChildAlignment
    <*> getClayLayoutCOnfigLayoutDirection

getClayBackgroundColor :: Declaration e f i (Maybe ClayColor)
getClayBackgroundColor = getStyleValue styleBackgroundColor

getClayCornerRadius :: Declaration e f i (Maybe ClayCornerRadius)
getClayCornerRadius = undefined

getClayImageElementConfig :: Declaration e f i (Maybe ClayImageElementConfig)
getClayImageElementConfig = undefined

getClayFloatingElementConfig :: Declaration e f i (Maybe ClayFloatingElementConfig)
getClayFloatingElementConfig = undefined

getClayCustomElementConfig :: Declaration e f i (Maybe ClayCustomElementConfig)
getClayCustomElementConfig = undefined

getClayScrollElementConfig :: Declaration e f i (Maybe ClayScrollElementConfig)
getClayScrollElementConfig = undefined

getClayBorderElementConfig :: Declaration e f i (Maybe ClayBorderElementConfig)
getClayBorderElementConfig = undefined

getClayUserData :: Declaration e f i (Ptr ())
getClayUserData = undefined

-- getClayChildGap :: Declaration e f i (Maybe Word16)
-- getClayChildGap = do
--   pure undefined

-- getClayChildAlignment :: Declaration e f i ClayChildAlignment
-- getClayChildAlignment = ClayChildAlignment <$> getClayChildAlignmentX <*> getClayChildAlignmentY
-- getClayChildAlignmentX = do
--   childStyleAlignX <- getStyleValue (childStyleChildAlignX . styleChild)
--     StyleValue AlignLeft -> Just clayAlignXLeft
--     StyleValue AlignCenter -> Just clayAlignXCenter

-- getClayChildAlignmentY :: Declaration e f i ClayLayoutAlignmentY
-- getClayChildAlignmentY = do
--   childStyleAlignY <- getStyleValue (childStyleChildAlignY . styleChild)
--   pure $ case childStyleAlignY of
--     Default -> Nothing
--     StyleValue AlignTop -> Just clayAlignYTop
--     StyleValue AlignMiddle -> Just clayAlignYCenter

-- getClaySizing :: Declaration e f i ClaySizing
-- getClaySizing = do
--   let xSizing = toMaybe $ getSizing style ctx XAxis
--       ySizing = toMaybe $ getSizing style ctx YAxis
--       toClayAxis a = case a of
--         Fit b -> ClaySizingAxis (Left $ toClayMinMax ctx b) claySizingTypeFit
--         Percent f -> ClaySizingAxis (Right (CFloat f)) claySizingTypePercent
--         Fixed i ->
--           ClaySizingAxis
--             (Left $ ClaySizingMinMax (Just $ fromIntegral i) (Just $ fromIntegral i))
--             claySizingTypeFixed
--    in ClaySizing
--         { claySizingWidth = toClayAxis <$> xSizing,
--           claySizingHeight = toClayAxis <$> ySizing
--         }

-- toClayMinMax :: SizingBounds -> Declaration e f i ClaySizingMinMax
-- toClayMinMax ctx (SizingBounds bMin bMax) =
--   ClaySizingMinMax
--     { claySizingMinMaxMin = toMinMaxCFloat bMin,
--       claySizingMinMaxMax = toMinMaxCFloat bMax
--     }

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
