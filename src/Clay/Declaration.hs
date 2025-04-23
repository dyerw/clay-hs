{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Clay.Declaration where

import Clay.Layout
import Clay.Layout.Config (ConfigValue (..))
import Clay.Raw
import Clay.Raw.Types
import Control.Monad.IO.Class
import Control.Monad.RWS.Strict (MonadReader (ask), RWST (runRWST), asks)
import Data.Maybe (fromMaybe)
import Foreign (Word16)
import Foreign.C.Types

newtype Declaration ctx e f i c a = Declaration
  { unDeclaration ::
      RWST
        ctx
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
      MonadReader ctx
    )

runDeclaration :: Declaration ctx e f i c a -> ctx -> IO [e]
runDeclaration d ctx = do
  (_, _, events) <-
    runRWST
      (unDeclaration d)
      ctx
      ()
  pure events

type ElementDeclaration e f i c a = Declaration (ElementDeclarationContext e f i c) e f i c a

getDeclarationElement :: ElementDeclaration e f i c (Element e f i c)
getDeclarationElement = do
  (ElementDeclarationContext ele _) <- ask
  pure ele

type TextDeclaration e f i c a = Declaration (TextDeclarationContext f) e f i c a

data TextDeclarationContext f
  = TextDeclarationContext (TextConfig f) CommonDeclarationContextFields

instance HasStyle (TextDeclarationContext f) (TextStyleValues f) where
  getStyle (TextDeclarationContext cfg _) = getStyle cfg

data ElementDeclarationContext e f i c
  = ElementDeclarationContext (Element e f i c) CommonDeclarationContextFields

instance HasStyle (ElementDeclarationContext e f i c) ElementStyleValues where
  getStyle (ElementDeclarationContext ele _) = getStyle ele

data InputState = InputState
  { inputStatePointerLocation :: (Int, Int),
    inputStatePointerDown :: Bool,
    inputStateLayoutDimensions :: (Int, Int)
  }

data CommonDeclarationContextFields = CommonDeclarationContextFields
  { declarationContextInput :: InputState,
    declarationContextIsHovered :: Bool,
    declarationContextParentId :: Maybe ClayElementId,
    declarationContextElementId :: Maybe ClayElementId
  }

class IsContextDeclaration a where
  commonFields :: a -> CommonDeclarationContextFields

instance IsContextDeclaration (TextDeclarationContext f) where
  commonFields (TextDeclarationContext _ f) = f

instance IsContextDeclaration (ElementDeclarationContext e f i c) where
  commonFields (ElementDeclarationContext _ f) = f

getContextInput :: (IsContextDeclaration ctx, MonadReader ctx m) => m InputState
getContextInput = declarationContextInput . commonFields <$> ask

getContextIsHovered :: (IsContextDeclaration ctx, MonadReader ctx m) => m Bool
getContextIsHovered = declarationContextIsHovered . commonFields <$> ask

getContextParentId :: (IsContextDeclaration ctx, MonadReader ctx m) => m (Maybe ClayElementId)
getContextParentId = declarationContextParentId . commonFields <$> ask

getContextElementId :: (IsContextDeclaration ctx, MonadReader ctx m) => m (Maybe ClayElementId)
getContextElementId = declarationContextElementId . commonFields <$> ask

calculateClayElementId :: Maybe ClayElementId -> Element e f i c -> IO (Maybe ClayElementId)
calculateClayElementId parentId ele = do
  clayStringId <- traverse toClayString (getElementId ele)
  let parentIdHash = maybe 0 clayElementIdId parentId
  traverse (\csi -> clayHashString csi 0 parentIdHash) clayStringId

getConfigValue :: (IsContextDeclaration ctx, HasStyle ctx s) => (s -> ConfigValue a) -> Declaration ctx e f i c (Maybe a)
getConfigValue f = do
  style <- asks getStyle
  isHovered <- getContextIsHovered
  let baseValue = (f . styleBase) style
  let hoveredValue = (f . styleHovered) style
  pure $
    toMaybe $
      if isHovered
        then baseValue <> hoveredValue
        else baseValue

getCFloatValue :: (Integral a, HasStyle ctx s, IsContextDeclaration ctx) => (s -> ConfigValue a) -> Declaration ctx e f i c (Maybe CFloat)
getCFloatValue = fmap (fmap (CFloat . fromIntegral)) . getConfigValue

getWord16Value :: (Integral a, HasStyle ctx s, IsContextDeclaration ctx) => (s -> ConfigValue a) -> Declaration ctx e f i c (Maybe Word16)
getWord16Value = fmap (fmap fromIntegral) . getConfigValue

getWord16ValueZero :: (Integral a, HasStyle ctx s, IsContextDeclaration ctx) => (s -> ConfigValue a) -> Declaration ctx e f i c Word16
getWord16ValueZero = fmap (fromMaybe 0) . getWord16Value

resolveLayoutSizeValue :: (IsContextDeclaration ctx) => LayoutSizeValue -> Declaration ctx e f i c Float
resolveLayoutSizeValue v = do
  input <- getContextInput
  let (viewWidth, viewHeight) = inputStateLayoutDimensions input
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
