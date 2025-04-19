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
import Control.Monad.RWS.Strict (MonadReader (ask), RWST (runRWST))
import Control.Monad.State.Class
import Data.Text
import Foreign (Word16)
import Foreign.C.Types

calculateLayout :: Element f e i -> InputState -> IO ([RenderCommand f], [e])
calculateLayout root (InputState (pointerX, pointerY) pointerDown dims@(layoutWidth, layoutHeight)) = do
  claySetPointerState
    (ClayVector2 (CFloat $ fromIntegral pointerX) (CFloat $ fromIntegral pointerY))
    (CBool $ if pointerDown then 1 else 0)

  claySetLayoutDimensions
    (ClayDimensions (CFloat $ fromIntegral layoutWidth) (CFloat $ fromIntegral layoutHeight))

  clayBeginLayout
  (_, _, events) <-
    runRenderM
      (declareElement root)
      (RenderContext dims)
      (RenderState Nothing Nothing)
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

data RenderContext = RenderContext
  { renderContextIsHovered :: Bool,
    renderContextViewSize :: (Int, Int),
    renderContextStyle :: ElementStyle,
    renderContextParentId :: Maybe ClayElementId,
    renderContextElementId :: Maybe ClayElementId
  }

makeRenderContext :: Maybe ClayElementId -> Element f e i -> InputState -> IO RenderContext
makeRenderContext parentId ele input = do
  clayStringId <- liftIO $ traverse toClayString (elementId ele)
  let parentIdHash = maybe 0 clayElementIdId parentId
  eid <- liftIO $ traverse (\csi -> clayHashString csi 0 parentIdHash) clayStringId

  isHovered <- clayHovered

  pure $ RenderContext isHovered (inputStateLayoutDimensions input) (elementStyle ele) parentId eid

newtype RenderM e a = RenderM {unRenderM :: RWST RenderContext [e] () IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader RenderContext)

runRenderM :: RenderM e a -> RenderContext -> IO (a, [e])
runRenderM rm ctx = do
  (out, _, events) <- runRWST (unRenderM rm) ctx ()
  pure (out, events)

getStyleValue ::
  (Semigroup a) =>
  Style a ->
  (a -> StyleValue b) ->
  RenderM e (Maybe b)
getStyleValue (Style h b) f = do
  isHovered <- asks renderContextIsHovered
  let derivedStyle = if isHovered then b <> h else b
   in f derivedStyle

resolveLayoutSizeValue :: LayoutSizeValue -> RenderM e Float
resolveLayoutSizeValue v = do
  (viewWidth, viewHeight) <- asks renderContextViewSize
  case v of
    Pixels i -> fromIntegral i
    Var ViewHeight -> fromIntegral viewHeight
    Var ViewWidth -> fromIntegral viewWidth
    AddVar ViewHeight i -> fromIntegral $ viewHeight + i
    AddVar ViewWidth i -> fromIntegral $ viewWidth + i
    SubtractVar ViewHeight i -> fromIntegral $ viewHeight - i
    SubtractVar ViewWidth i -> fromIntegral $ viewWidth - i
    MultiplyVar ViewHeight i -> fromIntegral $ viewHeight * i
    MultiplyVar ViewWidth i -> fromIntegral $ viewWidth * i
    DivideVar ViewHeight i -> fromIntegral viewHeight / fromIntegral i
    DivideVar ViewWidth i -> fromIntegral viewWidth / fromIntegral i

declareElement :: Element f e i -> RenderM e ()
declareElement ele = do
  liftIO clayOpenElement

  isHovered <- liftIO clayHovered

  setCurrentElementId ele

  configureElement

  -- TODO: Declare children

  liftIO clayCloseElement

configureElement :: RenderM e ()
configureElement = do
  clayDecl <- getClayElementDeclaration e
  liftIO $ clayConfigureOpenElement clayDecl
  pure ()

getClayElementDeclaration :: RenderM e ClayElementDeclaration
getClayElementDeclaration =
  ClayElementDeclaration
    <$> gets renderStateCurrentElementId
    <*> getClayLayoutConfig

-- { clayElementDeclarationId = eid,
--   clayElementDeclarationLayout = toClayLayoutConfig (elementStyle e) ctx,
--   clayElementDeclarationBackgroundColor = undefined,
--   clayElementDeclarationCornerRadius = undefined,
--   clayElementDeclarationImage = undefined,
--   clayElementDeclarationScroll = undefined,
--   clayElementDeclarationFloating = undefined,
--   clayElementDeclarationBorder = undefined,
--   clayElementDeclarationCustom = undefined,
--   clayElementDeclarationUserData = undefined
-- }

getClayLayoutConfig :: RenderM e ClayLayoutConfig
getClayLayoutConfig =
  ClayLayoutConfig
    { clayLayoutConfigSizing = toClaySizing style ctx,
      clayLayoutConfigChildAlignment = toClayChildLayoutAlignment style ctx,
      clayLayoutConfigChildGap = undefined,
      clayLayoutConfigPadding = undefined,
      clayLayoutConfigLayoutDirection = undefined
    }

getClayChildGap :: RenderM e (Maybe Word16)
getClayChildGap = do
  pure ()

getClayChildAlignment :: RenderM e ClayChildAlignment
getClayChildAlignment = ClayChildAlignment <$> getClayChildAlignmentX <*> getClayChildAlignmentY

getClayChildAlignmentX :: RenderM e ClayLayoutAlignmentX
getClayChildAlignmentX = do
  childStyleAlignX <- getStyleValue (childStyleChildAlignX . styleChild)
  pure $ case childStyleAlignX of
    Default -> Nothing
    StyleValue AlignLeft -> Just clayAlignXLeft
    StyleValue AlignRight -> Just clayAlignXRight
    StyleValue AlignCenter -> Just clayAlignXCenter

getClayChildAlignmentY :: RenderM e ClayLayoutAlignmentY
getClayChildAlignmentY = do
  childStyleAlignY <- getStyleValue (childStyleChildAlignY . styleChild)
  pure $ case childStyleAlignY of
    Default -> Nothing
    StyleValue AlignTop -> Just clayAlignYTop
    StyleValue AlignBottom -> Just clayAlignYBottom
    StyleValue AlignMiddle -> Just clayAlignYCenter

getClaySizing :: RenderM e ClaySizing
getClaySizing = do
  let xSizing = toMaybe $ getSizing style ctx XAxis
      ySizing = toMaybe $ getSizing style ctx YAxis
      toClayAxis a = case a of
        Fit b -> ClaySizingAxis (Left $ toClayMinMax ctx b) claySizingTypeFit
        Grow b -> ClaySizingAxis (Left $ toClayMinMax ctx b) claySizingTypeFit
        Percent f -> ClaySizingAxis (Right (CFloat f)) claySizingTypePercent
        Fixed i ->
          ClaySizingAxis
            (Left $ ClaySizingMinMax (Just $ fromIntegral i) (Just $ fromIntegral i))
            claySizingTypeFixed
   in ClaySizing
        { claySizingWidth = toClayAxis <$> xSizing,
          claySizingHeight = toClayAxis <$> ySizing
        }

toClayMinMax :: SizingBounds -> RenderM e ClaySizingMinMax
toClayMinMax ctx (SizingBounds bMin bMax) =
  ClaySizingMinMax
    { claySizingMinMaxMin = toMinMaxCFloat bMin,
      claySizingMinMaxMax = toMinMaxCFloat bMax
    }
  where
    toMinMaxCFloat = fmap (CFloat . resolveLayoutSizeValue ctx) . toMaybe

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
