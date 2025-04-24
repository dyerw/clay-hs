{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module Clay.Layout where

import Clay.Color
import Clay.Layout.Config
import Control.Monad.Reader (Reader, asks, runReader)
import Data.Text
import GHC.Generics (Generic)

-- | e is the type of event the element can produce
-- f is the font
-- i is the image
-- c is custom data
data Clay e f i c = ClayElement (Element e f i c) | ClayText (TextConfig f)

data Element e f i c
  = Element (ElementConfig e f i c)
  | ImageElement (ImageConfig i)
  | CustomElement (CustomConfig c)

data ElementConfig e f i c = ElementConfig
  { elementConfigId :: Maybe Text,
    elementConfigStyle :: ElementStyle,
    elementConfigChildren :: [Clay e f i c]
  }

data ImageConfig i = ImageConfig
  { imageConfigId :: Maybe Text,
    imageConfigImage :: i,
    imageConfigStyle :: ElementStyle
  }

data CustomConfig c = CustomConfig
  { customConfigId :: Maybe Text,
    customConfigCustom :: c,
    customConfigStyle :: ElementStyle
  }

data TextConfig f = TextConfig
  { textConfigStyle :: TextStyle f,
    textConfigText :: Text
  }

-- | * Constructors
element :: Text -> ElementStyle -> [Clay e f i c] -> Clay e f i c
element eid style children =
  ClayElement $
    Element $
      ElementConfig (Just eid) style children

element_ :: ElementStyle -> [Clay e f i c] -> Clay e f i c
element_ style children =
  ClayElement $
    Element $
      ElementConfig Nothing style children

image :: Text -> i -> ElementStyle -> Clay e f i c
image eid img style =
  ClayElement $
    ImageElement $
      ImageConfig (Just eid) img style

image_ :: i -> ElementStyle -> Clay e f i c
image_ img style = ClayElement $ ImageElement $ ImageConfig Nothing img style

custom :: Text -> c -> ElementStyle -> Clay e f i c
custom eid ctm style =
  ClayElement $
    CustomElement $
      CustomConfig (Just eid) ctm style

custom_ :: c -> ElementStyle -> Clay e f i c
custom_ ctm style =
  ClayElement $
    CustomElement $
      CustomConfig Nothing ctm style

text :: TextStyle f -> Text -> Clay e f i c
text style txt = ClayText $ TextConfig style txt

instance HasStyle (TextConfig f) (TextStyleValues f) where
  getStyle (TextConfig {textConfigStyle}) = textConfigStyle

getElementId :: Element e f i c -> Maybe Text
getElementId (Element (ElementConfig {elementConfigId})) = elementConfigId
getElementId (ImageElement (ImageConfig {imageConfigId})) = imageConfigId
getElementId (CustomElement (CustomConfig {customConfigId})) = customConfigId

instance HasStyle (Element e f i c) ElementStyleValues where
  getStyle (Element (ElementConfig {elementConfigStyle})) = elementConfigStyle
  getStyle (ImageElement (ImageConfig {imageConfigStyle})) = imageConfigStyle
  getStyle (CustomElement (CustomConfig {customConfigStyle})) = customConfigStyle

-- | * Layout

-- | ** Style Types
data Style a = Style
  {styleHovered :: a, styleBase :: a}
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Config (Style a))

class HasStyle a s | a -> s where
  getStyle :: a -> Style s

base :: (Monoid a) => a -> Style a
base = Style mempty

hovered :: (Monoid a) => Style a -> Style a
hovered (Style h b) = Style (h <> b) mempty

type ElementStyle = Style ElementStyleValues

data ElementStyleValues = ElementStyleValues
  { styleDirection :: DirectionStyle,
    styleSizing :: SizingStyle,
    stylePadding :: PaddingStyle,
    styleChild :: ChildStyle,
    styleBorder :: BorderStyle,
    styleBackgroundColor :: ConfigValue Color,
    styleCornerRadius :: CornerRadiusStyle
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via (Config ElementStyleValues)

directionStyle :: DirectionStyle -> ElementStyle
directionStyle d = base $ mempty {styleDirection = d}

sizingStyle :: SizingStyle -> ElementStyle
sizingStyle s = base $ mempty {styleSizing = s}

paddingStyle :: PaddingStyle -> ElementStyle
paddingStyle p = base $ mempty {stylePadding = p}

childStyle :: ChildStyle -> ElementStyle
childStyle c = base $ mempty {styleChild = c}

borderStyle :: BorderStyle -> ElementStyle
borderStyle b = base $ mempty {styleBorder = b}

cornerRadiusStyle :: CornerRadiusStyle -> ElementStyle
cornerRadiusStyle c = base $ mempty {styleCornerRadius = c}

backgroundColor :: Color -> ElementStyle
backgroundColor c = base $ mempty {styleBackgroundColor = configValue c}

type TextStyle f = Style (TextStyleValues f)

data TextStyleValues f = TextStyleValues
  { textStyleTextColor :: ConfigValue Color,
    textStyleFont :: ConfigValue f,
    textStyleFontSize :: ConfigValue Int
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Config (TextStyleValues f))

-- | ** Calculated Layout Values
newtype LayoutCalculation = LayoutCalculation (Reader (Integer, Integer) Integer)

calculate :: Integer -> Integer -> LayoutCalculation -> Integer
calculate w h (LayoutCalculation r) = runReader r (w, h)

calculationUnaryOp :: (Integer -> Integer) -> (LayoutCalculation -> LayoutCalculation)
calculationUnaryOp op (LayoutCalculation x) = LayoutCalculation $ op <$> x

calculationBinaryOp :: (Integer -> Integer -> Integer) -> (LayoutCalculation -> LayoutCalculation -> LayoutCalculation)
calculationBinaryOp op (LayoutCalculation x) (LayoutCalculation y) = LayoutCalculation $ op <$> x <*> y

viewWidth :: LayoutCalculation
viewWidth = LayoutCalculation $ asks fst

viewHeight :: LayoutCalculation
viewHeight = LayoutCalculation $ asks snd

instance Num LayoutCalculation where
  fromInteger = LayoutCalculation . pure
  (+) = calculationBinaryOp (+)
  (-) = calculationBinaryOp (-)
  (*) = calculationBinaryOp (*)
  abs = calculationUnaryOp abs
  signum = calculationUnaryOp signum

-- ** Corner Radius

data CornerRadiusStyle = CornerRadiusStyle
  { cornerRadiusStyleTopLeft :: ConfigValue Int,
    cornerRadiusStyleTopRight :: ConfigValue Int,
    cornerRadiusStyleBottomLeft :: ConfigValue Int,
    cornerRadiusStyleBottomRight :: ConfigValue Int
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Config CornerRadiusStyle)

cornerRadiusTopLeft :: Int -> ElementStyle
cornerRadiusTopLeft i = cornerRadiusStyle $ mempty {cornerRadiusStyleTopLeft = configValue i}

cornerRadiusTopRight :: Int -> ElementStyle
cornerRadiusTopRight i = cornerRadiusStyle $ mempty {cornerRadiusStyleTopRight = configValue i}

cornerRadiusBottomLeft :: Int -> ElementStyle
cornerRadiusBottomLeft i = cornerRadiusStyle $ mempty {cornerRadiusStyleBottomLeft = configValue i}

cornerRadiusBottomRight :: Int -> ElementStyle
cornerRadiusBottomRight i = cornerRadiusStyle $ mempty {cornerRadiusStyleBottomRight = configValue i}

cornerRadiusTop :: Int -> ElementStyle
cornerRadiusTop i = cornerRadiusTopLeft i <> cornerRadiusTopRight i

cornerRadiusBottom :: Int -> ElementStyle
cornerRadiusBottom i = cornerRadiusBottomLeft i <> cornerRadiusBottomRight i

cornerRadiusLeft :: Int -> ElementStyle
cornerRadiusLeft i = cornerRadiusTopLeft i <> cornerRadiusBottomLeft i

cornerRadiusRight :: Int -> ElementStyle
cornerRadiusRight i = cornerRadiusBottomRight i <> cornerRadiusTopRight i

cornerRadius :: Int -> ElementStyle
cornerRadius i = cornerRadiusTop i <> cornerRadiusBottom i

-- ** Direction

newtype DirectionStyle = DirectionStyle
  { directionStyleDirection :: ConfigValue LayoutDirection
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Config DirectionStyle)

data LayoutDirection = LeftToRight | TopToBottom deriving (Eq, Show)

topToBottom :: ElementStyle
topToBottom = directionStyle $ mempty {directionStyleDirection = configValue TopToBottom}

leftToRight :: ElementStyle
leftToRight = directionStyle $ mempty {directionStyleDirection = configValue LeftToRight}

-- ** Sizing

data SizingStyle = SizingStyle
  { sizingStyleXAxis :: ConfigValue Sizing,
    sizingStyleYAxis :: ConfigValue Sizing
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via (Config SizingStyle)

data SizingBounds = SizingBounds
  { sizingBoundsMax :: ConfigValue LayoutCalculation,
    sizingBoundsMin :: ConfigValue LayoutCalculation
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via (Config SizingBounds)

minSize :: LayoutCalculation -> SizingBounds
minSize v = mempty {sizingBoundsMax = configValue v}

maxSize :: LayoutCalculation -> SizingBounds
maxSize v = mempty {sizingBoundsMin = configValue v}

-- | min max
bounds :: LayoutCalculation -> LayoutCalculation -> SizingBounds
bounds mn mx = minSize mn <> maxSize mx

data Sizing = Fit SizingBounds | Grow SizingBounds | Percent Float | Fixed LayoutCalculation

fitX :: SizingBounds -> ElementStyle
fitX b =
  sizingStyle $
    mempty
      { sizingStyleXAxis = configValue $ Fit b
      }

fitX_ :: ElementStyle
fitX_ =
  sizingStyle $
    mempty
      { sizingStyleXAxis = configValue $ Fit mempty
      }

fitY :: SizingBounds -> ElementStyle
fitY b =
  sizingStyle $
    mempty
      { sizingStyleYAxis = configValue $ Fit b
      }

-- Unbounded fitY
fitY_ :: ElementStyle
fitY_ =
  sizingStyle $
    mempty
      { sizingStyleYAxis = configValue $ Fit mempty
      }

-- | Fit on both axes
fit :: SizingBounds -> ElementStyle
fit b = fitX b <> fitY b

-- | Unbounded fit on both axes
fit_ :: ElementStyle
fit_ = fit mempty

growX :: SizingBounds -> ElementStyle
growX b =
  sizingStyle $
    mempty
      { sizingStyleXAxis = configValue $ Grow b
      }

growX_ :: ElementStyle
growX_ = growX mempty

growY :: SizingBounds -> ElementStyle
growY b =
  sizingStyle $
    mempty
      { sizingStyleYAxis = configValue $ Grow b
      }

growY_ :: ElementStyle
growY_ = growY mempty

grow :: SizingBounds -> ElementStyle
grow b = growX b <> growY b

grow_ :: ElementStyle
grow_ = grow mempty

percentX :: Float -> ElementStyle
percentX x =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleXAxis = configValue (Percent x)
            }
      }

percentY :: Float -> ElementStyle
percentY y =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleYAxis = configValue (Percent y)
            }
      }

percent :: Float -> ElementStyle
percent xy = percentX xy <> percentY xy

percentXY :: Float -> Float -> ElementStyle
percentXY x y = percentX x <> percentY y

fixedX :: LayoutCalculation -> ElementStyle
fixedX x =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleXAxis = configValue (Fixed x)
            }
      }

fixedY :: LayoutCalculation -> ElementStyle
fixedY y =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleYAxis = configValue (Fixed y)
            }
      }

fixed :: LayoutCalculation -> ElementStyle
fixed xy = fixedX xy <> fixedY xy

fixedXY :: LayoutCalculation -> LayoutCalculation -> ElementStyle
fixedXY x y = fixedX x <> fixedY y

-- ** Padding

data PaddingStyle = PaddingStyle
  { paddingStyleTop :: ConfigValue Int,
    paddingStyleRight :: ConfigValue Int,
    paddingStyleBottom :: ConfigValue Int,
    paddingStyleLeft :: ConfigValue Int
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Config PaddingStyle)

paddingTop :: Int -> ElementStyle
paddingTop value = paddingStyle $ mempty {paddingStyleTop = configValue value}

paddingRight :: Int -> ElementStyle
paddingRight value = base $ mempty {stylePadding = mempty {paddingStyleRight = configValue value}}

paddingBottom :: Int -> ElementStyle
paddingBottom value = base $ mempty {stylePadding = mempty {paddingStyleBottom = configValue value}}

paddingLeft :: Int -> ElementStyle
paddingLeft value = base $ mempty {stylePadding = mempty {paddingStyleLeft = configValue value}}

paddingY :: Int -> ElementStyle
paddingY value = base $ mempty {stylePadding = mempty {paddingStyleTop = configValue value, paddingStyleBottom = configValue value}}

paddingX :: Int -> ElementStyle
paddingX value = base $ mempty {stylePadding = mempty {paddingStyleLeft = configValue value, paddingStyleRight = configValue value}}

-- | Specified in clockwise order from the top
padding :: Int -> Int -> Int -> Int -> ElementStyle
padding top right bottom left =
  paddingTop top
    <> paddingRight right
    <> paddingBottom bottom
    <> paddingLeft left

paddingAll :: Int -> ElementStyle
paddingAll p =
  paddingTop p
    <> paddingRight p
    <> paddingBottom p
    <> paddingLeft p

data XAlignment = AlignXCenter | AlignXLeft | AlignXRight deriving (Eq, Show)

data YAlignment = AlignYCenter | AlignYTop | AlignYBottom deriving (Eq, Show)

data ChildStyle = ChildStyle
  { childStyleChildAlignX :: ConfigValue XAlignment,
    childStyleChildAlignY :: ConfigValue YAlignment,
    childStyleChildGap :: ConfigValue Int
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Config ChildStyle)

childGap :: Int -> ElementStyle
childGap i = base $ mempty {styleChild = mempty {childStyleChildGap = configValue i}}

childAlignX :: XAlignment -> ElementStyle
childAlignX a = base $ mempty {styleChild = mempty {childStyleChildAlignX = configValue a}}

childAlignY :: YAlignment -> ElementStyle
childAlignY a = base $ mempty {styleChild = mempty {childStyleChildAlignY = configValue a}}

-- | * Border
data BorderSideStyle = BorderSideStyle
  { borderSideStyleWidth :: ConfigValue Int,
    borderSideStyleColor :: ConfigValue Color
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Config BorderSideStyle)

data BorderStyle = BorderStyle
  { borderStyleTop :: BorderSideStyle,
    borderStyleBottom :: BorderSideStyle,
    borderStyleRight :: BorderSideStyle,
    borderStyleLeft :: BorderSideStyle
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Config BorderStyle)

borderTopWidth :: Int -> ElementStyle
borderTopWidth i =
  base $
    mempty
      { styleBorder =
          mempty
            { borderStyleTop = mempty {borderSideStyleWidth = configValue i}
            }
      }

borderTopColor :: Color -> ElementStyle
borderTopColor c =
  base $
    mempty
      { styleBorder =
          mempty
            { borderStyleTop = mempty {borderSideStyleColor = configValue c}
            }
      }

borderTop :: Int -> Color -> ElementStyle
borderTop i c = borderTopWidth i <> borderTopColor c

borderBottomWidth :: Int -> ElementStyle
borderBottomWidth i =
  base $
    mempty
      { styleBorder =
          mempty
            { borderStyleBottom = mempty {borderSideStyleWidth = configValue i}
            }
      }

borderBottomColor :: Color -> ElementStyle
borderBottomColor c =
  base $
    mempty
      { styleBorder =
          mempty
            { borderStyleBottom = mempty {borderSideStyleColor = configValue c}
            }
      }

borderBottom :: Int -> Color -> ElementStyle
borderBottom i c = borderBottomWidth i <> borderBottomColor c

borderYWidth :: Int -> ElementStyle
borderYWidth i = borderTopWidth i <> borderBottomWidth i

borderYColor :: Color -> ElementStyle
borderYColor c = borderTopColor c <> borderBottomColor c

borderY :: Int -> Color -> ElementStyle
borderY i c = borderBottom i c <> borderTop i c

borderRightWidth :: Int -> ElementStyle
borderRightWidth i =
  base $
    mempty
      { styleBorder =
          mempty
            { borderStyleRight = mempty {borderSideStyleWidth = configValue i}
            }
      }

borderRightColor :: Color -> ElementStyle
borderRightColor c =
  base $
    mempty
      { styleBorder =
          mempty
            { borderStyleRight = mempty {borderSideStyleColor = configValue c}
            }
      }

borderRight :: Int -> Color -> ElementStyle
borderRight i c = borderRightWidth i <> borderRightColor c

borderLeftWidth :: Int -> ElementStyle
borderLeftWidth i =
  base $
    mempty
      { styleBorder =
          mempty
            { borderStyleLeft = mempty {borderSideStyleWidth = configValue i}
            }
      }

borderLeftColor :: Color -> ElementStyle
borderLeftColor c =
  base $
    mempty
      { styleBorder =
          mempty
            { borderStyleLeft = mempty {borderSideStyleColor = configValue c}
            }
      }

borderLeft :: Int -> Color -> ElementStyle
borderLeft i c = borderRightWidth i <> borderRightColor c

borderXWidth :: Int -> ElementStyle
borderXWidth i = borderRightWidth i <> borderLeftWidth i

borderXColor :: Color -> ElementStyle
borderXColor c = borderRightColor c <> borderLeftColor c

borderX :: Int -> Color -> ElementStyle
borderX i c = borderRight i c <> borderLeft i c

borderWidth :: Int -> ElementStyle
borderWidth i = borderXWidth i <> borderYWidth i

borderColor :: Color -> ElementStyle
borderColor c = borderXColor c <> borderYColor c

border :: Int -> Color -> ElementStyle
border i c = borderWidth i <> borderColor c

-- | * Events

-- onHover :: e -> Style
-- onHover e = mempty {styleOnHover = pure e}

-- | * Text
font :: f -> TextStyle f
font f = base $ mempty {textStyleFont = configValue f}

textColor :: Color -> TextStyle f
textColor c = base $ mempty {textStyleTextColor = configValue c}

fontSize :: Int -> TextStyle f
fontSize s = base $ mempty {textStyleFontSize = configValue s}
