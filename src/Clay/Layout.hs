{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module Clay.Layout
  ( Element (..),
    el,
    el_,
    elI,
    extend,
    extendWith,
    Maybe (..),
    Hoverable (..),
    resolveHoverableLast,
    Style (..),
    Color (..),
    TextStyle (..),
    ElementId,
    PaddingStyle (..),
    SizingStyle (..),
    Sizing (..),
    unLast,
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
  )
where

import Data.Monoid (Alt (..), Dual (..))
import Data.Text

-- | e is the type of event the element can produce
data Element e f i
  = Element
      { elementId :: Maybe ElementId,
        elementStyle :: Style e,
        elementChildren :: [Element e f i]
      }
  | TextElement (TextStyle f) Text

data Color = Color {colorR :: Int, colorG :: Int, colorB :: Int, colorA :: Int}

data TextStyle f = TextStyle
  { textStyleTextColor :: Color,
    textStyleFont :: f
  }

el_ :: Style e -> [Element e f i] -> Element e f i
el_ = Element Nothing

el :: Text -> Style e -> [Element e f i] -> Element e f i
el eid = Element (Just (ElementId eid Nothing))

elI :: Text -> Int -> Style e -> [Element e f i] -> Element e f i
elI eid eidx = Element (Just (ElementId eid (Just eidx)))

extendWith :: Element e f i -> (Style e -> Style e) -> Element e f i
extendWith (Element eid decl1 children) f = Element eid (f decl1) children
extendWith textElement _ = textElement

extend :: Element e f i -> Style e -> Element e f i
extend e d = extendWith e (<> d)

text :: TextStyle f -> Text -> Element e f i
text = TextElement

data ElementId = ElementId !Text !(Maybe Int)

data Hoverable a = Hoverable {hovered :: a, notHovered :: a} deriving (Eq, Show, Functor)

mkHoverable :: a -> Hoverable a
mkHoverable a = Hoverable a a

instance (Semigroup a) => Semigroup (Hoverable a) where
  Hoverable a1 a2 <> Hoverable a3 a4 = Hoverable (a1 <> a3) (a2 <> a4)

instance (Monoid a) => Monoid (Hoverable a) where
  mempty = Hoverable mempty mempty

type Last a = Dual (Alt Maybe a)

mkLast :: a -> Last a
mkLast a = Dual (Alt (Just a))

unLast :: Last a -> Maybe a
unLast = getAlt . getDual

mkHovLast :: a -> Hoverable (Last a)
mkHovLast = mkHoverable . mkLast

resolveHoverableLast :: Bool -> Hoverable (Last a) -> Maybe a
resolveHoverableLast isHovered a =
  let f = if isHovered then hovered else notHovered
   in unLast (f a)

-- | An Style is just a big record you can build up with convenience functions.
-- Element declarations can be composed monoidally like: @padding 10 10 10 10 <> onHover ElementHovered <> fitX@.
-- The right declaration will override shared properties on the left: @(paddingTop 10 <> paddingTop 20) == paddingTop 20@.
data Style e = Style
  { styleOnHover :: Last e,
    styleSizing :: SizingStyle,
    stylePadding :: PaddingStyle
  }
  deriving (Eq, Show)

instance Semigroup (Style e) where
  Style hover1 sizing1 padding1 <> Style hover2 sizing2 padding2 =
    Style (hover1 <> hover2) (sizing1 <> sizing2) (padding1 <> padding2)

instance Monoid (Style e) where
  mempty = Style mempty mempty mempty

-- * Layout

-- ** Sizing

data SizingStyle = SizingStyle
  { sizingStyleXAxis :: Hoverable (Last Sizing),
    sizingStyleYAxis :: Hoverable (Last Sizing)
  }
  deriving (Eq, Show)

instance Semigroup SizingStyle where
  SizingStyle xAxis1 yAxis1 <> SizingStyle xAxis2 yAxis2 =
    SizingStyle (xAxis1 <> xAxis2) (yAxis1 <> yAxis2)

instance Monoid SizingStyle where
  mempty =
    SizingStyle
      { sizingStyleXAxis = mempty,
        sizingStyleYAxis = mempty
      }

data Sizing = Fit | Grow | Percent Float | Fixed Int deriving (Eq, Show)

fitX :: Style e
fitX =
  mempty
    { styleSizing =
        mempty
          { sizingStyleXAxis = mkHovLast Fit
          }
    }

fitY :: Style e
fitY =
  mempty
    { styleSizing =
        mempty
          { sizingStyleYAxis = mkHovLast Fit
          }
    }

fit :: Style e
fit = fitX <> fitY

growX :: Style e
growX =
  mempty
    { styleSizing =
        mempty
          { sizingStyleXAxis = mkHovLast Grow
          }
    }

growY :: Style e
growY =
  mempty
    { styleSizing =
        mempty
          { sizingStyleYAxis = mkHovLast Grow
          }
    }

grow :: Style e
grow = growX <> growY

percentX :: Float -> Style e
percentX x =
  mempty
    { styleSizing =
        mempty
          { sizingStyleXAxis = mkHovLast (Percent x)
          }
    }

percentY :: Float -> Style e
percentY y =
  mempty
    { styleSizing =
        mempty
          { sizingStyleYAxis = mkHovLast (Percent y)
          }
    }

percent :: Float -> Style e
percent xy = percentX xy <> percentY xy

percentXY :: Float -> Float -> Style e
percentXY x y = percentX x <> percentY y

fixedX :: Int -> Style e
fixedX x =
  mempty
    { styleSizing =
        mempty
          { sizingStyleXAxis = mkHovLast (Fixed x)
          }
    }

fixedY :: Int -> Style e
fixedY y =
  mempty
    { styleSizing =
        mempty
          { sizingStyleYAxis = mkHovLast (Fixed y)
          }
    }

fixed :: Int -> Style e
fixed xy = fixedX xy <> fixedY xy

fixedXY :: Int -> Int -> Style e
fixedXY x y = fixedX x <> fixedY y

-- ** Padding

data PaddingStyle = PaddingStyle
  { paddingStyleTop :: Hoverable (Last Int),
    paddingStyleRight :: Hoverable (Last Int),
    paddingStyleBottom :: Hoverable (Last Int),
    paddingStyleLeft :: Hoverable (Last Int)
  }
  deriving (Eq, Show)

instance Semigroup PaddingStyle where
  (PaddingStyle top1 right1 bottom1 left1) <> (PaddingStyle top2 right2 bottom2 left2) =
    PaddingStyle (top1 <> top2) (right1 <> right2) (bottom1 <> bottom2) (left1 <> left2)

instance Monoid PaddingStyle where
  mempty = PaddingStyle mempty mempty mempty mempty

paddingTop :: Int -> Style e
paddingTop value = mempty {stylePadding = mempty {paddingStyleTop = mkHovLast value}}

paddingRight :: Int -> Style e
paddingRight value = mempty {stylePadding = mempty {paddingStyleRight = mkHovLast value}}

paddingBottom :: Int -> Style e
paddingBottom value = mempty {stylePadding = mempty {paddingStyleBottom = mkHovLast value}}

paddingLeft :: Int -> Style e
paddingLeft value = mempty {stylePadding = mempty {paddingStyleLeft = mkHovLast value}}

paddingY :: Int -> Style e
paddingY value = mempty {stylePadding = mempty {paddingStyleTop = mkHovLast value, paddingStyleBottom = mkHovLast value}}

paddingX :: Int -> Style e
paddingX value = mempty {stylePadding = mempty {paddingStyleLeft = mkHovLast value, paddingStyleRight = mkHovLast value}}

-- | Specified in clockwise order from the top
padding :: Int -> Int -> Int -> Int -> Style e
padding top right bottom left =
  paddingTop top
    <> paddingRight right
    <> paddingBottom bottom
    <> paddingLeft left

paddingAll :: Int -> Style e
paddingAll p =
  paddingTop p
    <> paddingRight p
    <> paddingBottom p
    <> paddingLeft p

-- * Events

-- onHover :: e -> Style e
-- onHover e = mempty {styleOnHover = pure e}
