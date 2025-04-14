{-# LANGUAGE StrictData #-}

module Clay.Layout (
  Element (..),
  element,
  element_,
  image,
  image_,
  StyleValue (..),
  toMaybe,
  Style (..),
  Axis (..),
  ElementContext (..),
  hovered,
  getStyleValue,
  getSizing,
  Color (..),
  ElementStyle,
  ElementStyleValues(..),
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
  ChildStyle(..),
  resolveLayoutSizeValue,
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

import Data.Text

{- | e is the type of event the element can produce
f is the font
i is the image
-}
data Element e f i
  = Element
      { elementId :: Maybe Text
      , elementStyle :: ElementStyle
      , elementChildren :: [Element e f i]
      }
  | TextElement
      { textElementStyle :: TextStyle f
      , textElementText :: Text
      }
  | ImageElement
      { imageElementId :: Maybe Text
      , imageElementImage :: i
      , imageElementStyle :: ElementStyle
      }

data Color
  = Color
  { colorR :: Int
  , colorG :: Int
  , colorB :: Int
  , colorA :: Int
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

data StyleValue a = StyleValue a | Default deriving (Eq, Show)

instance Semigroup (StyleValue a) where
  sva <> Default = sva
  _ <> svb = svb

instance Monoid (StyleValue a) where
  mempty = Default

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

resolveLayoutSizeValue :: ElementContext -> LayoutSizeValue -> Float
resolveLayoutSizeValue ElementContext{elementContextViewSize = (viewWidth, viewHeight)} v = case v of
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

data ElementContext = ElementContext
  { elementContextIsHovered :: Bool
  , elementContextViewSize :: (Int, Int)
  }

toMaybe :: StyleValue a -> Maybe a
toMaybe (StyleValue a) = Just a
toMaybe Default = Nothing

data Style a = Style
  {styleHovered :: a, styleBase :: a}

instance (Semigroup a) => Semigroup (Style a) where
  (Style hovered1 base1) <> (Style hovered2 base2) =
    Style (hovered1 <> hovered2) (base1 <> base2)

instance (Monoid a) => Monoid (Style a) where
  mempty = Style mempty mempty

base :: (Monoid a) => a -> Style a
base = Style mempty

hovered :: (Monoid a) => Style a -> Style a
hovered (Style h b) = Style (h <> b) mempty

type ElementStyle = Style ElementStyleValues
type TextStyle f = Style (TextStyleValues f)

getStyleValue ::
  (Semigroup a) =>
  Style a ->
  ElementContext ->
  (a -> StyleValue b) ->
  StyleValue b
getStyleValue (Style h b) (ElementContext isHovered _) f =
  let derivedStyle = if isHovered then b <> h else b
   in f derivedStyle

data ElementStyleValues = ElementStyleValues
  { styleDirection :: DirectionStyle
  , styleSizing :: SizingStyle
  , stylePadding :: PaddingStyle
  , styleChild :: ChildStyle
  , styleBorder :: BorderStyle
  }
  deriving (Eq, Show)

-- TODO: Look into deriving these product Monoids with generics
instance Semigroup ElementStyleValues where
  ElementStyleValues a1 a2 a3 a4 a5 <> ElementStyleValues b1 b2 b3 b4 b5 =
    ElementStyleValues (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)

instance Monoid ElementStyleValues where
  mempty = ElementStyleValues mempty mempty mempty mempty mempty

data Axis = XAxis | YAxis deriving (Eq, Show)

getSizing :: ElementStyle -> ElementContext -> Axis -> StyleValue Sizing
getSizing style ctx axis = getStyleValue style ctx (getAxis . styleSizing)
 where
  getAxis = if axis == XAxis then sizingStyleXAxis else sizingStyleYAxis

-- * Layout

-- ** Direction

newtype DirectionStyle = DirectionStyle
  { directionStyleDirection :: StyleValue LayoutDirection
  }
  deriving (Eq, Show)

instance Semigroup DirectionStyle where
  (DirectionStyle d1) <> (DirectionStyle d2) = DirectionStyle (d1 <> d2)

instance Monoid DirectionStyle where
  mempty = DirectionStyle mempty

data LayoutDirection = LeftToRight | TopToBottom deriving (Eq, Show)

topToBottom :: ElementStyle
topToBottom = base $ mempty{styleDirection = mempty{directionStyleDirection = StyleValue TopToBottom}}

leftToRight :: ElementStyle
leftToRight = base $ mempty{styleDirection = mempty{directionStyleDirection = StyleValue LeftToRight}}

-- ** Sizing

data SizingStyle = SizingStyle
  { sizingStyleXAxis :: StyleValue Sizing
  , sizingStyleYAxis :: StyleValue Sizing
  }
  deriving (Eq, Show)

instance Semigroup SizingStyle where
  SizingStyle xAxis1 yAxis1 <> SizingStyle xAxis2 yAxis2 =
    SizingStyle (xAxis1 <> xAxis2) (yAxis1 <> yAxis2)

instance Monoid SizingStyle where
  mempty =
    SizingStyle
      { sizingStyleXAxis = mempty
      , sizingStyleYAxis = mempty
      }

data SizingBounds = SizingBounds
  { sizingBoundsMax :: StyleValue LayoutSizeValue
  , sizingBoundsMin :: StyleValue LayoutSizeValue
  }
  deriving (Eq, Show)

instance Semigroup SizingBounds where
  (SizingBounds max1 min1) <> (SizingBounds max2 min2) =
    SizingBounds (max1 <> max2) (min1 <> min2)

instance Monoid SizingBounds where
  mempty = SizingBounds mempty mempty

minSize :: LayoutSizeValue -> SizingBounds
minSize v = mempty{sizingBoundsMax = StyleValue v}

maxSize :: LayoutSizeValue -> SizingBounds
maxSize v = mempty{sizingBoundsMin = StyleValue v}

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
            { sizingStyleXAxis = StyleValue $ Fit b
            }
      }

fitX_ :: ElementStyle
fitX_ =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleXAxis = StyleValue $ Fit mempty
            }
      }

fitY :: SizingBounds -> ElementStyle
fitY b =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleYAxis = StyleValue $ Fit b
            }
      }

-- Unbounded fitY
fitY_ :: ElementStyle
fitY_ =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleYAxis = StyleValue $ Fit mempty
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
            { sizingStyleXAxis = StyleValue $ Grow b
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
            { sizingStyleYAxis = StyleValue $ Grow b
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
            { sizingStyleXAxis = StyleValue (Percent x)
            }
      }

percentY :: Float -> ElementStyle
percentY y =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleYAxis = StyleValue (Percent y)
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
            { sizingStyleXAxis = StyleValue (Fixed x)
            }
      }

fixedY :: Int -> ElementStyle
fixedY y =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleYAxis = StyleValue (Fixed y)
            }
      }

fixed :: Int -> ElementStyle
fixed xy = fixedX xy <> fixedY xy

fixedXY :: Int -> Int -> ElementStyle
fixedXY x y = fixedX x <> fixedY y

-- ** Padding

data PaddingStyle = PaddingStyle
  { paddingStyleTop :: StyleValue Int
  , paddingStyleRight :: StyleValue Int
  , paddingStyleBottom :: StyleValue Int
  , paddingStyleLeft :: StyleValue Int
  }
  deriving (Eq, Show)

instance Semigroup PaddingStyle where
  (PaddingStyle top1 right1 bottom1 left1) <> (PaddingStyle top2 right2 bottom2 left2) =
    PaddingStyle (top1 <> top2) (right1 <> right2) (bottom1 <> bottom2) (left1 <> left2)

instance Monoid PaddingStyle where
  mempty = PaddingStyle mempty mempty mempty mempty

paddingTop :: Int -> ElementStyle
paddingTop value = base $ mempty{stylePadding = mempty{paddingStyleTop = StyleValue value}}

paddingRight :: Int -> ElementStyle
paddingRight value = base $ mempty{stylePadding = mempty{paddingStyleRight = StyleValue value}}

paddingBottom :: Int -> ElementStyle
paddingBottom value = base $ mempty{stylePadding = mempty{paddingStyleBottom = StyleValue value}}

paddingLeft :: Int -> ElementStyle
paddingLeft value = base $ mempty{stylePadding = mempty{paddingStyleLeft = StyleValue value}}

paddingY :: Int -> ElementStyle
paddingY value = base $ mempty{stylePadding = mempty{paddingStyleTop = StyleValue value, paddingStyleBottom = StyleValue value}}

paddingX :: Int -> ElementStyle
paddingX value = base $ mempty{stylePadding = mempty{paddingStyleLeft = StyleValue value, paddingStyleRight = StyleValue value}}

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
  { childStyleChildAlignX :: StyleValue XAlignment
  , childStyleChildAlignY :: StyleValue YAlignment
  , childStyleChildGap :: StyleValue Int
  }
  deriving (Eq, Show)

instance Semigroup ChildStyle where
  (ChildStyle cax1 cay1 g1) <> (ChildStyle cax2 cay2 g2) =
    ChildStyle (cax1 <> cax2) (cay1 <> cay2) (g1 <> g2)

instance Monoid ChildStyle where
  mempty = ChildStyle mempty mempty mempty

childGap :: Int -> ElementStyle
childGap i = base $ mempty{styleChild = mempty{childStyleChildGap = StyleValue i}}

childAlignX :: XAlignment -> ElementStyle
childAlignX a = base $ mempty{styleChild = mempty{childStyleChildAlignX = StyleValue a}}

childAlignY :: YAlignment -> ElementStyle
childAlignY a = base $ mempty{styleChild = mempty{childStyleChildAlignY = StyleValue a}}

-- | * Border
data BorderSideStyle = BorderSideStyle
  { borderSideStyleWidth :: StyleValue Int
  , borderSideStyleColor :: StyleValue Color
  }
  deriving (Eq, Show)

instance Semigroup BorderSideStyle where
  (BorderSideStyle w1 c1) <> (BorderSideStyle w2 c2) =
    BorderSideStyle (w1 <> w2) (c1 <> c2)

instance Monoid BorderSideStyle where
  mempty = BorderSideStyle mempty mempty

data BorderStyle = BorderStyle
  { borderStyleTop :: BorderSideStyle
  , borderStyleBottom :: BorderSideStyle
  , borderStyleRight :: BorderSideStyle
  , borderStyleLeft :: BorderSideStyle
  }
  deriving (Eq, Show)

instance Semigroup BorderStyle where
  (BorderStyle t1 b1 r1 l1) <> (BorderStyle t2 b2 r2 l2) =
    BorderStyle (t1 <> t2) (b1 <> b2) (r1 <> r2) (l1 <> l2)

instance Monoid BorderStyle where
  mempty = BorderStyle mempty mempty mempty mempty

borderTopWidth :: Int -> ElementStyle
borderTopWidth i =
  base $
    mempty
      { styleBorder =
          mempty
            { borderStyleTop = mempty{borderSideStyleWidth = StyleValue i}
            }
      }

borderTopColor :: Color -> ElementStyle
borderTopColor c =
  base $
    mempty
      { styleBorder =
          mempty
            { borderStyleTop = mempty{borderSideStyleColor = StyleValue c}
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
            { borderStyleBottom = mempty{borderSideStyleWidth = StyleValue i}
            }
      }

borderBottomColor :: Color -> ElementStyle
borderBottomColor c =
  base $
    mempty
      { styleBorder =
          mempty
            { borderStyleBottom = mempty{borderSideStyleColor = StyleValue c}
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
            { borderStyleRight = mempty{borderSideStyleWidth = StyleValue i}
            }
      }

borderRightColor :: Color -> ElementStyle
borderRightColor c =
  base $
    mempty
      { styleBorder =
          mempty
            { borderStyleRight = mempty{borderSideStyleColor = StyleValue c}
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
            { borderStyleLeft = mempty{borderSideStyleWidth = StyleValue i}
            }
      }

borderLeftColor :: Color -> ElementStyle
borderLeftColor c =
  base $
    mempty
      { styleBorder =
          mempty
            { borderStyleLeft = mempty{borderSideStyleColor = StyleValue c}
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
  { textStyleTextColor :: StyleValue Color
  , textStyleFont :: StyleValue f
  , textStyleFontSize :: StyleValue Int
  }
  deriving (Eq, Show)

instance Semigroup (TextStyleValues f) where
  (TextStyleValues c1 f1 s1) <> (TextStyleValues c2 f2 s2) =
    TextStyleValues (c1 <> c2) (f1 <> f2) (s1 <> s2)

instance Monoid (TextStyleValues f) where
  mempty = TextStyleValues mempty mempty mempty

font :: f -> TextStyle f
font f = base $ mempty{textStyleFont = StyleValue f}

textColor :: Color -> TextStyle f
textColor c = base $ mempty{textStyleTextColor = StyleValue c}

fontSize :: Int -> TextStyle f
fontSize s = base $ mempty{textStyleFontSize = StyleValue s}
