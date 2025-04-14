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
import Foreign.C.Types

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

newtype RenderContext = RenderContext (Int, Int)

data RenderState = RenderState
  { renderStateParentId :: Maybe ClayElementId,
    renderStateCurrentElementId :: Maybe ClayElementId
  }

newtype RenderM e a = RenderM {unRenderM :: RWST RenderContext [e] RenderState IO a}
  deriving (Functor, Applicative, Monad, MonadState RenderState, MonadIO, MonadReader RenderContext)

runRenderM :: RenderM e a -> RenderContext -> RenderState -> IO (a, RenderState, [e])
runRenderM = runRWST . unRenderM

calculateLayout :: Element f e i -> InputState -> IO ([RenderCommand font], [e])
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

setCurrentElementId :: Element f e i -> RenderM e ()
setCurrentElementId ele = do
  clayStringId <- liftIO $ traverse toClayString (elementId ele)
  parentId <- gets renderStateParentId
  let parentIdHash = maybe 0 clayElementIdId parentId
  eid <- liftIO $ traverse (\csi -> clayHashString csi 0 parentIdHash) clayStringId
  s <- get
  put (s {renderStateCurrentElementId = eid})

declareElement :: Element f e i -> RenderM e ()
declareElement ele = do
  liftIO clayOpenElement

  isHovered <- liftIO clayHovered
  (RenderContext dims) <- ask
  let ctx = ElementContext isHovered dims

  setCurrentElementId ele

  configureElement ctx ele

  -- TODO: Declare children

  liftIO clayCloseElement

configureElement :: ElementContext -> Element f e i -> RenderM e ()
configureElement ctx e = do
  clayDecl <- toClayElementDeclaration ctx e
  liftIO $ clayConfigureOpenElement clayDecl
  pure ()

toClayElementDeclaration :: ElementContext -> Element f e i -> RenderM e ClayElementDeclaration
toClayElementDeclaration ctx e = do
  eid <- gets renderStateCurrentElementId
  pure $
    ClayElementDeclaration
      { clayElementDeclarationId = eid,
        clayElementDeclarationLayout = toClayLayoutConfig (elementStyle e) ctx,
        clayElementDeclarationBackgroundColor = undefined,
        clayElementDeclarationCornerRadius = undefined,
        clayElementDeclarationImage = undefined,
        clayElementDeclarationScroll = undefined,
        clayElementDeclarationFloating = undefined,
        clayElementDeclarationBorder = undefined,
        clayElementDeclarationCustom = undefined,
        clayElementDeclarationUserData = undefined
      }

toClayLayoutConfig :: ElementStyle -> ElementContext -> ClayLayoutConfig
toClayLayoutConfig style ctx =
  ClayLayoutConfig
    { clayLayoutConfigSizing = toClaySizing style ctx,
      clayLayoutConfigChildAlignment = toClayChildLayoutAlignment style ctx,
      clayLayoutConfigChildGap = undefined,
      clayLayoutConfigPadding = undefined,
      clayLayoutConfigLayoutDirection = undefined
    }

toClayChildLayoutAlignment :: ElementStyle -> ElementContext -> ClayChildAlignment
toClayChildLayoutAlignment style ctx =
  let childStyleAlignX = getStyleValue style ctx (childStyleChildAlignX . styleChild)
      childStyleAlignY = getStyleValue style ctx (childStyleChildAlignY . styleChild)
   in ClayChildAlignment
        { clayChildAlignmentX = case childStyleAlignX of
            Default -> Nothing
            StyleValue AlignLeft -> Just clayAlignXLeft
            StyleValue AlignRight -> Just clayAlignXRight
            StyleValue AlignCenter -> Just clayAlignXCenter,
          clayChildAlignmentY = case childStyleAlignY of
            Default -> Nothing
            StyleValue AlignTop -> Just clayAlignYTop
            StyleValue AlignBottom -> Just clayAlignYBottom
            StyleValue AlignMiddle -> Just clayAlignYCenter
        }

toClaySizing :: ElementStyle -> ElementContext -> ClaySizing
toClaySizing style ctx =
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

toClayMinMax :: ElementContext -> SizingBounds -> ClaySizingMinMax
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
