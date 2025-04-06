{-# LANGUAGE StrictData #-}

module Clay.Layout (
  Element (..),
  el,
  el_,
  elI,
  extend,
  extendWith,
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
  TextStyle,
  ElementId,
  PaddingStyle (..),
  SizingStyle (..),
  Sizing (..),
  fitX,
  fitY,
  fit,
  growX,
  growY,
  grow,
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
  -- onHover,
  text,
  font,
  textColor,
  fontSize,
)
where

import Control.Monad.Reader (Reader, asks, runReader)
import Data.Monoid (Alt (..), Dual (..))
import Data.Text

{- | e is the type of event the element can produce
f is the font
i is the image
-}
data Element e f i
  = Element
      { elementId :: Maybe ElementId
      , elementStyle :: ElementStyle
      , elementChildren :: [Element e f i]
      }
  | TextElement (TextStyle f) Text

data Color
  = Color
  { colorR :: Int
  , colorG :: Int
  , colorB :: Int
  , colorA :: Int
  }
  deriving (Eq, Show)

el_ :: ElementStyle -> [Element e f i] -> Element e f i
el_ = Element Nothing

el :: Text -> ElementStyle -> [Element e f i] -> Element e f i
el eid = Element (Just (ElementId eid Nothing))

elI :: Text -> Int -> ElementStyle -> [Element e f i] -> Element e f i
elI eid eidx = Element (Just (ElementId eid (Just eidx)))

extendWith :: Element e f i -> (ElementStyle -> ElementStyle) -> Element e f i
extendWith (Element eid decl1 children) f = Element eid (f decl1) children
extendWith textElement _ = textElement

extend :: Element e f i -> ElementStyle -> Element e f i
extend e d = extendWith e (<> d)

data ElementId = ElementId Text (Maybe Int)

data StyleValue a = StyleValue a | Default deriving (Eq, Show)

instance Semigroup (StyleValue a) where
  sva <> Default = sva
  _ <> svb = svb

instance Monoid (StyleValue a) where
  mempty = Default

newtype ElementContext = ElementContext
  { elementContextIsHovered :: Bool
  }

toMaybe :: StyleValue a -> Maybe a
toMaybe (StyleValue a) = Just a
toMaybe Default = Nothing

data Style a = Style
  {styleHovered :: a, styleBase :: a}

instance (Semigroup a) => Semigroup (Style a) where
  (Style hovered1 base1) <> (Style hovered2 base2) =
    Style (hovered1 <> hovered2) (base1 <> base2)

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
getStyleValue (Style h b) (ElementContext isHovered) f =
  let derivedStyle = if isHovered then b <> h else b
   in f derivedStyle

data ElementStyleValues = ElementStyleValues
  { styleSizing :: SizingStyle
  , stylePadding :: PaddingStyle
  }
  deriving (Eq, Show)

-- TODO: Look into deriving these product Monoids with generics
instance Semigroup ElementStyleValues where
  ElementStyleValues a1 a2 <> ElementStyleValues b1 b2 =
    ElementStyleValues (a1 <> b1) (a2 <> b2)

instance Monoid ElementStyleValues where
  mempty = ElementStyleValues mempty mempty

data Axis = XAxis | YAxis deriving (Eq, Show)

getSizing :: ElementStyle -> ElementContext -> Axis -> StyleValue Sizing
getSizing style ctx axis = getStyleValue style ctx (getAxis . styleSizing)
 where
  getAxis = if axis == XAxis then sizingStyleXAxis else sizingStyleYAxis

-- * Layout

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

data Sizing = Fit | Grow | Percent Float | Fixed Int deriving (Eq, Show)

fitX :: ElementStyle
fitX =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleXAxis = StyleValue Fit
            }
      }

fitY :: ElementStyle
fitY =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleYAxis = StyleValue Fit
            }
      }

fit :: ElementStyle
fit = fitX <> fitY

growX :: ElementStyle
growX =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleXAxis = StyleValue Grow
            }
      }

growY :: ElementStyle
growY =
  base $
    mempty
      { styleSizing =
          mempty
            { sizingStyleYAxis = StyleValue Grow
            }
      }

grow :: ElementStyle
grow = growX <> growY

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
