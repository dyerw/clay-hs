{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}

module Clay.Layout
  ( Element (..),
    element,
    element_,
    image,
    image_,
    Style (..),
    Axis (..),
    hovered,
    Color (..),
    ElementStyle,
    ElementStyleValues (..),
    TextStyle,
    PaddingStyle (..),
    SizingStyle (..),
    Sizing (..),
    SizingBounds (..),
    LayoutSizeContextVar (..),
    LayoutSizeValue (..),
    add,
    divide,
    mult,
    ChildStyle (..),
    topToBottom,
    leftToRight,
    sub,
    maxSize,
    minSize,
    bounds,
    fitX,
    fitX_,
    fitY,
    fitY_,
    fit,
    fit_,
    growX,
    growX_,
    growY,
    growY_,
    grow,
    grow_,
    percentX,
    percentY,
    percentXY,
    percent,
    fixedX,
    fixedY,
    fixedXY,
    fixed,
    paddingTop,
    paddingRight,
    paddingBottom,
    paddingLeft,
    paddingY,
    paddingX,
    padding,
    paddingAll,
    XAlignment (..),
    YAlignment (..),
    childAlignX,
    childAlignY,
    childGap,
    borderBottomWidth,
    borderBottomColor,
    borderBottom,
    borderTopWidth,
    borderTopColor,
    borderTop,
    borderYWidth,
    borderYColor,
    borderY,
    borderRightWidth,
    borderRightColor,
    borderRight,
    borderLeftWidth,
    borderLeftColor,
    borderLeft,
    borderXWidth,
    borderXColor,
    borderX,
    borderColor,
    borderWidth,
    border,
    -- onHover,
    text,
    font,
    textColor,
    fontSize,
  )
where

import Clay.Layout.Config
import Data.Text
import GHC.Generics (Generic)

-- | e is the type of event the element can produce
-- f is the font
-- i is the image
data Element e f i
  = Element
      { elementId :: Maybe Text,
        elementStyle :: ElementStyle,
        elementChildren :: [Element e f i]
      }
  | TextElement
      { textElementStyle :: TextStyle f,
        textElementText :: Text
      }
  | ImageElement
      { imageElementId :: Maybe Text,
        imageElementImage :: i,
        imageElementStyle :: ElementStyle
      }

data Color = Color
  { colorR :: Int,
    colorG :: Int,
    colorB :: Int,
    colorA :: Int
  }
  deriving (Eq, Show)

element :: Text -> ElementStyle -> [Element e f i] -> Element e f i
element eid = Element (Just eid)

element_ :: ElementStyle -> [Element e f i] -> Element e f i
element_ = Element Nothing

image :: Text -> i -> ElementStyle -> Element e f i
image eid = ImageElement (Just eid)

image_ :: i -> ElementStyle -> Element e f i
image_ = ImageElement Nothing

data LayoutSizeContextVar = ViewHeight | ViewWidth deriving (Eq, Show)

data LayoutSizeValue
  = Var LayoutSizeContextVar
  | Pixels Int
  | AddVar LayoutSizeContextVar Int
  | SubtractVar LayoutSizeContextVar Int
  | MultiplyVar LayoutSizeContextVar Int
  | DivideVar LayoutSizeContextVar Int
  deriving (Eq, Show)

sub :: LayoutSizeContextVar -> Int -> LayoutSizeValue
sub = SubtractVar

add :: LayoutSizeContextVar -> Int -> LayoutSizeValue
add = AddVar

mult :: LayoutSizeContextVar -> Int -> LayoutSizeValue
mult = MultiplyVar

divide :: LayoutSizeContextVar -> Int -> LayoutSizeValue
divide = DivideVar

data Style a = Style
  {styleHovered :: a, styleBase :: a}
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Config (Style a))

base :: (Monoid a) => a -> Style a
base = Style mempty

hovered :: (Monoid a) => Style a -> Style a
hovered (Style h b) = Style (h <> b) mempty

type ElementStyle = Style ElementStyleValues

type TextStyle f = Style (TextStyleValues f)

data ElementStyleValues = ElementStyleValues
  { styleDirection :: DirectionStyle,
    styleSizing :: SizingStyle,
    stylePadding :: PaddingStyle,
    styleChild :: ChildStyle,
    styleBorder :: BorderStyle,
    styleBackgroundColor :: ConfigValue Color,
    styleCornerRadius :: ConfigValue Int
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Config ElementStyleValues)

data Axis = XAxis | YAxis deriving (Eq, Show)

-- * Layout

-- ** Direction

newtype DirectionStyle = DirectionStyle
  { directionStyleDirection :: ConfigValue LayoutDirection
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Config DirectionStyle)

data LayoutDirection = LeftToRight | TopToBottom deriving (Eq, Show)

topToBottom :: ElementStyle
topToBottom = base $ mempty {styleDirection = mempty {directionStyleDirection = configValue TopToBottom}}

leftToRight :: ElementStyle
leftToRight = base $ mempty {styleDirection = mempty {directionStyleDirection = configValue LeftToRight}}

-- ** Sizing

data SizingStyle = SizingStyle
  { sizingStyleXAxis :: ConfigValue Sizing,
    sizingStyleYAxis :: ConfigValue Sizing
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Config SizingStyle)

data SizingBounds = SizingBounds
  { sizingBoundsMax :: ConfigValue LayoutSizeValue,
    sizingBoundsMin :: ConfigValue LayoutSizeValue
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Config SizingBounds)

minSize :: LayoutSizeValue -> SizingBounds
minSize v = mempty {sizingBoundsMax = configValue v}

maxSize :: LayoutSizeValue -> SizingBounds
maxSize v = mempty {sizingBoundsMin = configValue v}

-- | min max
bounds :: LayoutSizeValue -> LayoutSizeValue -> SizingBounds
bounds mn mx = minSize mn <> maxSize mx

data Sizing = Fit SizingBounds | Grow SizingBounds | Percent Float | Fixed Int deriving (Eq, Show)

fitX :: SizingBounds -> ElementStyle
fitX b =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleXAxis = configValue $ Fit b
            }
      }

fitX_ :: ElementStyle
fitX_ =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleXAxis = configValue $ Fit mempty
            }
      }

fitY :: SizingBounds -> ElementStyle
fitY b =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleYAxis = configValue $ Fit b
            }
      }

-- Unbounded fitY
fitY_ :: ElementStyle
fitY_ =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleYAxis = configValue $ Fit mempty
            }
      }

-- | Fit on both axes
fit :: SizingBounds -> ElementStyle
fit b = fitX b <> fitY b

-- | Unbounded fit on both axes
fit_ :: ElementStyle
fit_ = fit mempty

growX :: SizingBounds -> ElementStyle
growX b =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleXAxis = configValue $ Grow b
            }
      }

growX_ :: ElementStyle
growX_ = growX mempty

growY :: SizingBounds -> ElementStyle
growY b =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleYAxis = configValue $ Grow b
            }
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

fixedX :: Int -> ElementStyle
fixedX x =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleXAxis = configValue (Fixed x)
            }
      }

fixedY :: Int -> ElementStyle
fixedY y =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleYAxis = configValue (Fixed y)
            }
      }

fixed :: Int -> ElementStyle
fixed xy = fixedX xy <> fixedY xy

fixedXY :: Int -> Int -> ElementStyle
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

paddingStyle :: PaddingStyle -> ElementStyle
paddingStyle ps = base $ mempty {stylePadding = ps}

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

data XAlignment = AlignCenter | AlignLeft | AlignRight deriving (Eq, Show)

data YAlignment = AlignMiddle | AlignTop | AlignBottom deriving (Eq, Show)

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
text :: TextStyle f -> Text -> Element e f i
text = TextElement

data TextStyleValues f = TextStyleValues
  { textStyleTextColor :: ConfigValue Color,
    textStyleFont :: ConfigValue f,
    textStyleFontSize :: ConfigValue Int
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Config (TextStyleValues f))

font :: f -> TextStyle f
font f = base $ mempty {textStyleFont = configValue f}

textColor :: Color -> TextStyle f
textColor c = base $ mempty {textStyleTextColor = configValue c}

fontSize :: Int -> TextStyle f
fontSize s = base $ mempty {textStyleFontSize = configValue s}
