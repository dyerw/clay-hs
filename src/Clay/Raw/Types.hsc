{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

#define CLAY_IMPLEMENTATION
#include <clay.h>
#include <clayhelper.h>

-- |
-- Module      : Clay.Raw.Types
-- Description : Direct bindings to Clay type definitions
-- License     : MIT
--
-- Direct port of all data structures in @clay.h@ with Storable instances.
-- All doc strings are copied from the C source file.
module Clay.Raw.Types where

import Foreign
import Foreign.C
import Foreign.Extra


-- * Utility Structs

-- | Note: Clay_String is not guaranteed to be null terminated. It may be if created from a literal C string, -- but it is also used to represent slices.
-- The Eq instance for this will compare length and the pointer value, not the underlying string.
data ClayString = ClayString
  { clayStringLength :: CInt,
    -- | The underlying character memory. Note: this will not be copied and will not extend the lifetime of the underlying memory.
    clayStringChars :: Ptr CChar
  } deriving (Eq, Show)
  

instance Storable ClayString where
  sizeOf _ = (#size Clay_String)
  alignment _ = (#alignment Clay_String)
  peek ptr = ClayString
    <$> (#peek Clay_String, length) ptr
    <*> (#peek Clay_String, chars) ptr
  poke ptr (ClayString l c) = do
    (#poke Clay_String, length) ptr l
    (#poke Clay_String, chars) ptr c

-- | Clay_StringSlice is used to represent non owning string slices, and includes
-- a baseChars field which points to the string this slice is derived from.
data ClayStringSlice = ClayStringSlice
  { clayStringSliceLength :: CInt,
    clayStringSliceChars :: Ptr CChar,
    -- | The source string / char* that this slice was derived from
    clayStringSliceBaseChars :: Ptr CChar
  } deriving (Eq, Show)

instance Storable ClayStringSlice where
  sizeOf _ = (#size Clay_StringSlice)
  alignment _ = (#alignment Clay_StringSlice)
  peek ptr = ClayStringSlice
    <$> (#peek Clay_StringSlice, length) ptr
    <*> (#peek Clay_StringSlice, chars) ptr
    <*> (#peek Clay_StringSlice, baseChars) ptr
  poke ptr (ClayStringSlice l c b) = do
    (#poke Clay_StringSlice, length) ptr l
    (#poke Clay_StringSlice, chars) ptr c
    (#poke Clay_StringSlice, baseChars) ptr b

data ClayArray a = ClayArray {
  clayArrayCapacity :: CInt,
  clayArrayLength :: CInt,
  clayArrayInternalArray :: Ptr a
}

instance Storable (ClayArray a) where
  -- All arrays are the same with different pointers so we
  -- can base this implementation on any of them
  sizeOf _ = (#size Clay__charArray)
  alignment _ = (#alignment Clay__charArray)
  peek ptr = do
    cap <- (#peek Clay__charArray, capacity) ptr
    len <- (#peek Clay__charArray, length) ptr
    int <- (#peek Clay__charArray, internalArray) ptr
    pure $ ClayArray cap len int
  poke ptr (ClayArray cap len int) = do
    (#poke Clay__charArray, capacity) ptr cap
    (#poke Clay__charArray, length) ptr len
    (#poke Clay__charArray, internalArray) ptr int

data ClayArraySlice a = ClayArraySlice {
  clayArraySliceLength :: CInt,
  clayArraySliceInternalArray :: Ptr a
}

instance Storable (ClayArraySlice a) where
  sizeOf _ = (#size Clay__charArraySlice)
  alignment _ = (#alignment Clay__charArraySlice)
  peek ptr = do
    len <- (#peek Clay__charArraySlice, length) ptr
    int <- (#peek Clay__charArraySlice, internalArray) ptr
    pure $ ClayArraySlice len int
  poke ptr (ClayArraySlice len int) = do
    (#poke Clay__charArraySlice, length) ptr len
    (#poke Clay__charArraySlice, internalArray) ptr int




-- | Clay_Arena is a memory arena structure that is used by clay to manage its internal allocations.
-- Rather than creating it by hand, it's easier to use Clay_CreateArenaWithCapacityAndMemory()
data ClayArena = ClayArena
  { clayArenaNextAllocation :: CUIntPtr,
    clayArenaCapacity :: CSize,
    clayArenaMemory :: Ptr CChar
  }

instance Storable ClayArena where
  sizeOf _ = (#size Clay_Arena)
  alignment _ = (#alignment Clay_Arena)
  peek ptr = ClayArena
    <$> (#peek Clay_Arena, nextAllocation) ptr
    <*> (#peek Clay_Arena, capacity) ptr
    <*> (#peek Clay_Arena, memory) ptr
  poke ptr (ClayArena n c m) = do
    (#poke Clay_Arena, nextAllocation) ptr n
    (#poke Clay_Arena, capacity) ptr c
    (#poke Clay_Arena, memory) ptr m

data ClayDimensions = ClayDimensions
  { clayDimensionsWidth :: CFloat,
    clayDimensionsHeight :: CFloat
  } deriving (Eq, Show)

instance Storable ClayDimensions where
  sizeOf _ = (#size Clay_Dimensions)
  alignment _ = (#alignment Clay_Dimensions)
  peek ptr = ClayDimensions
    <$> (#peek Clay_Dimensions, width) ptr
    <*> (#peek Clay_Dimensions, height) ptr
  poke ptr (ClayDimensions w h) = do
    (#poke Clay_Dimensions, width) ptr w
    (#poke Clay_Dimensions, height) ptr h

data ClayVector2 = ClayVector2
  { clayVector2X :: CFloat,
    clayVector2Y :: CFloat
  } deriving (Eq, Show)

instance Storable ClayVector2 where
  sizeOf _ = (#size Clay_Vector2)
  alignment _ = (#alignment Clay_Vector2)
  peek ptr = ClayVector2
    <$> (#peek Clay_Vector2, x) ptr
    <*> (#peek Clay_Vector2, y) ptr
  poke ptr (ClayVector2 x y) = do
    (#poke Clay_Vector2, x) ptr x
    (#poke Clay_Vector2, y) ptr y

-- | Internally clay conventionally represents colors as 0-255, but interpretation is up to the renderer.
data ClayColor = ClayColor
  { clayColorR :: CFloat,
    clayColorG :: CFloat,
    clayColorB :: CFloat,
    clayColorA :: CFloat
  } deriving (Eq, Show)

instance Storable ClayColor where
  sizeOf _ = (#size Clay_Color)
  alignment _ = (#alignment Clay_Color)
  peek ptr = ClayColor
    <$> (#peek Clay_Color, r) ptr
    <*> (#peek Clay_Color, g) ptr
    <*> (#peek Clay_Color, b) ptr
    <*> (#peek Clay_Color, a) ptr
  poke ptr (ClayColor r g b a) = do
    (#poke Clay_Color, r) ptr r
    (#poke Clay_Color, g) ptr g
    (#poke Clay_Color, b) ptr b
    (#poke Clay_Color, a) ptr a

data ClayBoundingBox = ClayBoundingBox
  { clayBoundingBoxX :: CFloat,
    clayBoundingBoxY :: CFloat,
    clayBoundingBoxWidth :: CFloat,
    clayBoundingBoxHeight :: CFloat
  } deriving (Eq, Show)

instance Storable ClayBoundingBox where
  sizeOf _ = (#size Clay_BoundingBox)
  alignment _ = (#alignment Clay_BoundingBox)
  peek ptr = ClayBoundingBox
    <$> (#peek Clay_BoundingBox, x) ptr
    <*> (#peek Clay_BoundingBox, y) ptr
    <*> (#peek Clay_BoundingBox, width) ptr
    <*> (#peek Clay_BoundingBox, height) ptr
  poke ptr (ClayBoundingBox x y w h) = do
    (#poke Clay_BoundingBox, x) ptr x
    (#poke Clay_BoundingBox, y) ptr y
    (#poke Clay_BoundingBox, width) ptr w
    (#poke Clay_BoundingBox, height) ptr h

-- | Primarily created via the CLAY_ID(), CLAY_IDI(), CLAY_ID_LOCAL() and CLAY_IDI_LOCAL() macros.
-- | Represents a hashed string ID used for identifying and finding specific clay UI elements, required
-- | by functions such as Clay_PointerOver() and Clay_GetElementData().
data ClayElementId = ClayElementId
  { -- | The resulting hash generated from the other fields.
    clayElementIdId :: CUInt,
    -- | A numerical offset applied after computing the hash from stringId.
    clayElementIdOffset :: CUInt,
    -- | A base hash value to start from, for example the parent element ID is used when calculating CLAY_ID_LOCAL().
    clayElementIdBaseId :: CUInt,
    -- | The string id to hash.
    clayElementIdStringId :: ClayString
  }

instance Storable ClayElementId where
  sizeOf _ = (#size Clay_ElementId)
  alignment _ = (#alignment Clay_ElementId)
  peek ptr = ClayElementId
    <$> (#peek Clay_ElementId, id) ptr
    <*> (#peek Clay_ElementId, offset) ptr
    <*> (#peek Clay_ElementId, baseId) ptr
    <*> (#peek Clay_ElementId, stringId) ptr
  poke ptr (ClayElementId i o b s) = do
    (#poke Clay_ElementId, id) ptr i
    (#poke Clay_ElementId, offset) ptr o
    (#poke Clay_ElementId, baseId) ptr b
    (#poke Clay_ElementId, stringId) ptr s

-- | Controls the "radius", or corner rounding of elements, including rectangles, borders and images.
-- | The rounding is determined by drawing a circle inset into the element corner by (radius, radius) pixels.
data ClayCornerRadius = ClayCornerRadius
  { clayCornerRadiusTopLeft :: CFloat,
    clayCornerRadiusTopRight :: CFloat,
    clayCornerRadiusBottomLeft :: CFloat,
    clayCornerRadiusBottomRight :: CFloat
  } deriving (Eq, Show)

instance Storable ClayCornerRadius where
  sizeOf _ = (#size Clay_CornerRadius)
  alignment _ = (#alignment Clay_CornerRadius)
  peek ptr = ClayCornerRadius
    <$> (#peek Clay_CornerRadius, topLeft) ptr
    <*> (#peek Clay_CornerRadius, topRight) ptr
    <*> (#peek Clay_CornerRadius, bottomLeft) ptr
    <*> (#peek Clay_CornerRadius, bottomRight) ptr
  poke ptr (ClayCornerRadius tl tr bl br) = do
    (#poke Clay_CornerRadius, topLeft) ptr tl
    (#poke Clay_CornerRadius, topRight) ptr tr
    (#poke Clay_CornerRadius, bottomLeft) ptr bl
    (#poke Clay_CornerRadius, bottomRight) ptr br 

-- * Element Configs

-- | Controls the direction in which child elements will be automatically laid out.
data ClayLayoutDirection = ClayLayoutDirection CUChar deriving (Eq, Show)   

-- | (Default) Lays out child elements from left to right with increasing x.
clayLeftToRight :: ClayLayoutDirection
clayLeftToRight = ClayLayoutDirection 0

pattern ClayLeftToRight :: ClayLayoutDirection
pattern ClayLeftToRight = ClayLayoutDirection 0
 

-- | Lays out child elements from top to bottom with increasing y.
clayTopToBottom :: ClayLayoutDirection
clayTopToBottom = ClayLayoutDirection 1

pattern ClayTopToBottom :: ClayLayoutDirection
pattern ClayTopToBottom = ClayLayoutDirection 1

instance Storable ClayLayoutDirection where
  sizeOf _ = #{size uint8_t}
  alignment _ = #{alignment uint8_t}
  peek ptr = do
    val <- peek (castPtr ptr) 
    return $ ClayLayoutDirection val
  poke ptr (ClayLayoutDirection d) = poke (castPtr ptr) d 

-- | Controls the alignment along the x axis (horizontal) of child elements.
newtype ClayLayoutAlignmentX = ClayLayoutAlignmentX CUChar deriving (Eq, Show)

-- | (Default) Aligns child elements to the left hand side of this element, offset by padding.width.left
clayAlignXLeft :: ClayLayoutAlignmentX
clayAlignXLeft = ClayLayoutAlignmentX 0

-- | Aligns child elements to the right hand side of this element, offset by padding.width.right
clayAlignXRight :: ClayLayoutAlignmentX
clayAlignXRight = ClayLayoutAlignmentX 1

-- | Aligns child elements horizontally to the center of this element
clayAlignXCenter :: ClayLayoutAlignmentX
clayAlignXCenter = ClayLayoutAlignmentX 2

instance Storable ClayLayoutAlignmentX where
  sizeOf _ = #{size uint8_t}
  alignment _ = #{alignment uint8_t}
  peek ptr = do
    val <- peek (castPtr ptr) :: IO CUChar 
    pure $ ClayLayoutAlignmentX val
  poke ptr (ClayLayoutAlignmentX a) = poke (castPtr ptr) a 

-- | Controls the alignment along the y axis (vertical) of child elements.
newtype ClayLayoutAlignmentY = ClayLayoutAlignmentY CUChar deriving (Eq, Show)

-- | (Default) Aligns child elements to the top of this element, offset by padding.width.top
clayAlignYTop :: ClayLayoutAlignmentY
clayAlignYTop = ClayLayoutAlignmentY 0

-- | Aligns child elements to the bottom of this element, offset by padding.width.bottom
clayAlignYBottom :: ClayLayoutAlignmentY
clayAlignYBottom = ClayLayoutAlignmentY 1

-- | Aligns child elements vertically to the center of this element
clayAlignYCenter :: ClayLayoutAlignmentY
clayAlignYCenter = ClayLayoutAlignmentY 2

instance Storable ClayLayoutAlignmentY where
  sizeOf _ = #{size uint8_t}
  alignment _ = #{alignment uint8_t}
  peek ptr = do
    val <- peek (castPtr ptr)
    pure $ ClayLayoutAlignmentY val
  poke ptr (ClayLayoutAlignmentY a) = poke (castPtr ptr) a

-- | Controls how the element takes up space inside its parent container.
newtype ClaySizingType = ClaySizingType CUChar deriving (Eq, Show)

-- | (default) Wraps tightly to the size of the element's contents.
claySizingTypeFit :: ClaySizingType
claySizingTypeFit = ClaySizingType 0

-- | Expands along this axis to fill available space in the parent element, sharing it with other GROW elements.
claySizingTypeGrow :: ClaySizingType
claySizingTypeGrow = ClaySizingType 1

-- | Expects 0-1 range. Clamps the axis size to a percent of the parent container's axis size minus padding and child gaps.
claySizingTypePercent :: ClaySizingType
claySizingTypePercent = ClaySizingType 2

-- | Clamps the axis size to an exact size in pixels.
claySizingTypeFixed :: ClaySizingType
claySizingTypeFixed = ClaySizingType 3

instance Storable ClaySizingType where
  sizeOf _ = #{size uint8_t}
  alignment _ = #{alignment uint8_t}
  peek ptr = do
    val <- peek (castPtr ptr) 
    pure $ ClaySizingType val
  poke ptr (ClaySizingType t) = poke (castPtr ptr) t 

-- | Controls how child elements are aligned on each axis.
data ClayChildAlignment = ClayChildAlignment
  { -- | Controls alignment of children along the x axis.
    clayChildAlignmentX :: ClayLayoutAlignmentX,
    -- | Controls alignment of children along the y axis.
    clayChildAlignmentY :: ClayLayoutAlignmentY
  } deriving (Eq, Show)

instance Storable ClayChildAlignment where
  sizeOf _ = (#size Clay_ChildAlignment)
  alignment _ = (#alignment Clay_ChildAlignment)
  peek ptr = ClayChildAlignment
    <$> (#peek Clay_ChildAlignment, x) ptr
    <*> (#peek Clay_ChildAlignment, y) ptr
  poke ptr (ClayChildAlignment x y) = do
    (#poke Clay_ChildAlignment, x) ptr x
    (#poke Clay_ChildAlignment, y) ptr y

-- | Controls the minimum and maximum size in pixels that this element is allowed to grow or shrink to,
-- | overriding sizing types such as FIT or GROW.
data ClaySizingMinMax = ClaySizingMinMax
  { -- | The smallest final size of the element on this axis will be this value in pixels.
    claySizingMinMaxMin :: CFloat,
    -- | The largest final size of the element on this axis will be this value in pixels.
    claySizingMinMaxMax :: CFloat
  } deriving (Eq, Show)

instance Storable ClaySizingMinMax where
  sizeOf _ = #{size Clay_SizingMinMax}
  alignment _ = #{alignment Clay_SizingMinMax}
  peek ptr = ClaySizingMinMax
    <$> #{peek Clay_SizingMinMax, min} ptr
    <*> #{peek Clay_SizingMinMax, max} ptr
  poke ptr (ClaySizingMinMax mn mx) = do
    #{poke Clay_SizingMinMax, min} ptr mn
    #{poke Clay_SizingMinMax, max} ptr mx

-- | Controls the sizing of this element along one axis inside its parent container.
data ClaySizingAxis = ClaySizingAxis
  { claySizingAxisSize :: Either ClaySizingMinMax CFloat,
    -- | Controls how the element takes up space inside its parent container.
    claySizingAxisType :: ClaySizingType
  } deriving (Eq, Show)

instance Storable ClaySizingAxis where
  sizeOf _ = #{size Clay_SizingAxis}
  alignment _ = #{alignment Clay_SizingAxis}
  peek ptr = do
    ty <- #{peek Clay_SizingAxis, type} ptr
    size <- case ty of
      ClaySizingType 2 -> Right <$> #{peek Clay_SizingAxis, size.percent} ptr
      _ -> Left <$> #{peek Clay_SizingAxis, size.minMax} ptr
    return $ ClaySizingAxis size ty
  poke ptr (ClaySizingAxis size ty) = do
    #{poke Clay_SizingAxis, type} ptr ty
    case size of
      Left minMax -> #{poke Clay_SizingAxis, size.minMax} ptr minMax
      Right percent -> #{poke Clay_SizingAxis, size.percent} ptr percent

-- | Controls the sizing of this element along both axes inside its parent container.
data ClaySizing = ClaySizing
  { -- | Controls the width sizing of the element, along the x axis.
    claySizingWidth :: Maybe ClaySizingAxis,
    -- | Controls the height sizing of the element, along the y axis.
    claySizingHeight :: Maybe ClaySizingAxis
  } deriving (Eq, Show)

instance Storable ClaySizing where
  sizeOf _ = #{size Clay_Sizing}
  alignment _ = #{alignment Clay_Sizing}
  peek ptr = ClaySizing
    <$> #{peek Clay_Sizing, width} ptr
    <*> #{peek Clay_Sizing, height} ptr
  poke ptr (ClaySizing w h) = do
    #{poke Clay_Sizing, width} ptr w
    #{poke Clay_Sizing, height} ptr h

-- | Controls "padding" in pixels, which is a gap between the bounding box of this element and where its children
-- | will be placed.
data ClayPadding = ClayPadding
  { clayPaddingLeft :: Word16,
    clayPaddingRight :: Word16,
    clayPaddingTop :: Word16,
    clayPaddingBottom :: Word16
  } deriving (Eq, Show)

instance Storable ClayPadding where
  sizeOf _ = #{size Clay_Padding}
  alignment _ = #{alignment Clay_Padding}
  peek ptr = ClayPadding
    <$> #{peek Clay_Padding, left} ptr
    <*> #{peek Clay_Padding, right} ptr
    <*> #{peek Clay_Padding, top} ptr
    <*> #{peek Clay_Padding, bottom} ptr
  poke ptr (ClayPadding l r t b) = do
    #{poke Clay_Padding, left} ptr l
    #{poke Clay_Padding, right} ptr r
    #{poke Clay_Padding, top} ptr t
    #{poke Clay_Padding, bottom} ptr b

-- | Controls various settings that affect the size and position of an element, as well as the sizes and positions
-- | of any child elements.
data ClayLayoutConfig = ClayLayoutConfig
  { -- | Controls the sizing of this element inside it's parent container, including FIT, GROW, PERCENT and FIXED sizing.
    clayLayoutConfigSizing :: ClaySizing,
    -- | Controls "padding" in pixels, which is a gap between the bounding box of this element and where its children will be placed.
    clayLayoutConfigPadding :: ClayPadding,
    -- | Controls the gap in pixels between child elements along the layout axis (horizontal gap for LEFT_TO_RIGHT, vertical gap for TOP_TO_BOTTOM).
    clayLayoutConfigChildGap :: Word16,
    -- | Controls how child elements are aligned on each axis.
    clayLayoutConfigChildAlignment :: ClayChildAlignment,
    -- | Controls the direction in which child elements will be automatically laid out.
    clayLayoutConfigLayoutDirection :: ClayLayoutDirection
  } deriving (Eq, Show)

instance Storable ClayLayoutConfig where
  sizeOf _ = #{size Clay_LayoutConfig}
  alignment _ = #{alignment Clay_LayoutConfig}
  peek ptr = ClayLayoutConfig
    <$> #{peek Clay_LayoutConfig, sizing} ptr
    <*> #{peek Clay_LayoutConfig, padding} ptr
    <*> #{peek Clay_LayoutConfig, childGap} ptr
    <*> #{peek Clay_LayoutConfig, childAlignment} ptr
    <*> #{peek Clay_LayoutConfig, layoutDirection} ptr
  poke ptr (ClayLayoutConfig s p cg ca ld) = do
    #{poke Clay_LayoutConfig, sizing} ptr s
    #{poke Clay_LayoutConfig, padding} ptr p
    #{poke Clay_LayoutConfig, childGap} ptr cg
    #{poke Clay_LayoutConfig, childAlignment} ptr ca
    #{poke Clay_LayoutConfig, layoutDirection} ptr ld

-- | Controls how text "wraps", that is how it is broken into multiple lines when there is insufficient horizontal space.
newtype ClayTextElementConfigWrapMode = ClayTextElementConfigWrapMode CUChar deriving (Eq, Show)

-- | (default) breaks on whitespace characters.
clayTextWrapWords :: ClayTextElementConfigWrapMode 
clayTextWrapWords = ClayTextElementConfigWrapMode 0

pattern ClayTextWrapWords :: ClayTextElementConfigWrapMode
pattern ClayTextWrapWords = ClayTextElementConfigWrapMode 0

-- | Don't break on space characters, only on newlines.
clayTextWrapNewlines :: ClayTextElementConfigWrapMode
clayTextWrapNewlines = ClayTextElementConfigWrapMode 1

pattern ClayTextWrapNewlines :: ClayTextElementConfigWrapMode
pattern ClayTextWrapNewlines = ClayTextElementConfigWrapMode 1

clayTextWrapNone :: ClayTextElementConfigWrapMode
clayTextWrapNone = ClayTextElementConfigWrapMode 2

pattern ClayTextWrapNone :: ClayTextElementConfigWrapMode
pattern ClayTextWrapNone = ClayTextElementConfigWrapMode 2

instance Storable ClayTextElementConfigWrapMode where
  sizeOf _ = #{size uint8_t}
  alignment _ = #{alignment uint8_t}
  peek ptr = ClayTextElementConfigWrapMode <$> peek (castPtr ptr) 
  poke ptr (ClayTextElementConfigWrapMode mode) = poke (castPtr ptr) mode

-- | Controls how wrapped lines of text are horizontally aligned within the outer text bounding box.
newtype ClayTextAlignment = ClayTextAlignment CUChar deriving (Eq, Show)

-- | (default) Horizontally aligns wrapped lines of text to the left hand side of their bounding box.
clayTextAlignLeft :: ClayTextAlignment
clayTextAlignLeft = ClayTextAlignment 0

pattern ClayTextAlignLeft :: ClayTextAlignment
pattern ClayTextAlignLeft = ClayTextAlignment 0

-- | Horizontally aligns wrapped lines of text to the center of their bounding box.
clayTextAlignCenter :: ClayTextAlignment
clayTextAlignCenter = ClayTextAlignment 1

pattern ClayTextAlignCenter :: ClayTextAlignment
pattern ClayTextAlignCenter = ClayTextAlignment 1

-- | Horizontally aligns wrapped lines of text to the right hand side of their bounding box.
clayTextAlignRight :: ClayTextAlignment
clayTextAlignRight = ClayTextAlignment 2

pattern ClayTextAlignRight :: ClayTextAlignment
pattern ClayTextAlignRight = ClayTextAlignment 2


instance Storable ClayTextAlignment where
  sizeOf _ = #{size uint8_t}
  alignment _ = #{alignment uint8_t}
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ ClayTextAlignment val
  poke ptr (ClayTextAlignment align) = poke (castPtr ptr) align

-- | Controls various functionality related to text elements.
data ClayTextElementConfig = ClayTextElementConfig
  { -- | A pointer that will be transparently passed through to the resulting render command.
    clayTextElementConfigUserData :: Ptr (),
    -- | The RGBA color of the font to render, conventionally specified as 0-255.
    clayTextElementConfigTextColor :: ClayColor,
    -- | An integer transparently passed to Clay_MeasureText to identify the font to use.
    -- | The debug view will pass fontId = 0 for its internal text.
    clayTextElementConfigFontId :: Word16,
    -- | Controls the size of the font. Handled by the function provided to Clay_MeasureText.
    clayTextElementConfigFontSize :: Word16,
    -- | Controls extra horizontal spacing between characters. Handled by the function provided to Clay_MeasureText.
    clayTextElementConfigLetterSpacing :: Word16,
    -- | Controls additional vertical space between wrapped lines of text.
    clayTextElementConfigLineHeight :: Word16,
    -- | Controls how text "wraps", that is how it is broken into multiple lines when there is insufficient horizontal space.
    clayTextElementConfigWrapMode :: ClayTextElementConfigWrapMode,
    -- | Controls how wrapped lines of text are horizontally aligned within the outer text bounding box.
    clayTextElementConfigTextAlignment :: ClayTextAlignment,
    -- | When set to true, clay will hash the entire text contents of this string as an identifier for its internal
    -- | text measurement cache, rather than just the pointer and length. This will incur significant performance cost for
    -- | long bodies of text.
    clayTextElementConfigHashStringContents :: Bool
  } deriving (Eq, Show)

instance Storable ClayTextElementConfig where
  sizeOf _ = #{size Clay_TextElementConfig}
  alignment _ = #{alignment Clay_TextElementConfig}
  peek ptr = ClayTextElementConfig
    <$> #{peek Clay_TextElementConfig, userData} ptr
    <*> #{peek Clay_TextElementConfig, textColor} ptr
    <*> #{peek Clay_TextElementConfig, fontId} ptr
    <*> #{peek Clay_TextElementConfig, fontSize} ptr
    <*> #{peek Clay_TextElementConfig, letterSpacing} ptr
    <*> #{peek Clay_TextElementConfig, lineHeight} ptr
    <*> #{peek Clay_TextElementConfig, wrapMode} ptr
    <*> #{peek Clay_TextElementConfig, textAlignment} ptr
    <*> #{peek Clay_TextElementConfig, hashStringContents} ptr
  poke ptr (ClayTextElementConfig ud tc fi fs ls lh wm ta hs) = do
    #{poke Clay_TextElementConfig, userData} ptr ud
    #{poke Clay_TextElementConfig, textColor} ptr tc
    #{poke Clay_TextElementConfig, fontId} ptr fi
    #{poke Clay_TextElementConfig, fontSize} ptr fs
    #{poke Clay_TextElementConfig, letterSpacing} ptr ls
    #{poke Clay_TextElementConfig, lineHeight} ptr lh
    #{poke Clay_TextElementConfig, wrapMode} ptr wm
    #{poke Clay_TextElementConfig, textAlignment} ptr ta
    #{poke Clay_TextElementConfig, hashStringContents} ptr hs

-- * Image

-- | Controls various settings related to image elements.
data ClayImageElementConfig = ClayImageElementConfig {
  -- | A transparent pointer used to pass image data through to the renderer.
  clayImageElementConfigImageData :: Ptr (),
  -- | The original dimensions of the source image, used to control aspect ratio.
  clayImageElementConfigSourceDimensions :: ClayDimensions
} deriving (Eq, Show)

instance Storable ClayImageElementConfig where
  sizeOf _ = #{size Clay_ImageElementConfig}
  alignment _ = #{alignment Clay_ImageElementConfig}
  peek ptr = ClayImageElementConfig
    <$> #{peek Clay_ImageElementConfig, imageData} ptr
    <*> #{peek Clay_ImageElementConfig, sourceDimensions} ptr
  poke ptr (ClayImageElementConfig imgd sd) = do
    #{poke Clay_ImageElementConfig, imageData} ptr imgd
    #{poke Clay_ImageElementConfig, sourceDimensions} ptr sd

-- * Floating

-- | Controls where a floating element is offset relative to its parent element.
-- Note: see https://github.com/user-attachments/assets/b8c6dfaa-c1b1-41a4-be55-013473e4a6ce for a visual explanation.
newtype ClayFloatingAttachPointType = ClayFloatingAttachPointType CUChar deriving (Eq, Show)

#{enum ClayFloatingAttachPointType, ClayFloatingAttachPointType, \
  CLAY_ATTACH_POINT_LEFT_TOP, \
  CLAY_ATTACH_POINT_LEFT_CENTER, \
  CLAY_ATTACH_POINT_LEFT_BOTTOM, \
  CLAY_ATTACH_POINT_CENTER_TOP, \
  CLAY_ATTACH_POINT_CENTER_CENTER, \
  CLAY_ATTACH_POINT_CENTER_BOTTOM, \
  CLAY_ATTACH_POINT_RIGHT_TOP, \
  CLAY_ATTACH_POINT_RIGHT_CENTER, \
  CLAY_ATTACH_POINT_RIGHT_BOTTOM}

instance Storable ClayFloatingAttachPointType where
  sizeOf _ = #{size uint8_t}
  alignment _ = #{alignment uint8_t}
  peek ptr = do
    val <- peek (castPtr ptr) :: IO CUChar 
    pure $ (ClayFloatingAttachPointType val)
  poke ptr (ClayFloatingAttachPointType i) = poke (castPtr ptr) i

-- | Controls where a floating element is offset relative to its parent element.
data ClayFloatingAttachPoints = ClayFloatingAttachPoints {
  -- | Controls the origin point on a floating element that attaches to its parent.
  clayFloatingAttachPointsElement :: ClayFloatingAttachPointType,
  -- | Controls the origin point on the parent element that the floating element attaches to.
  clayFloatingAttachPointsParent :: ClayFloatingAttachPointType
} deriving (Eq, Show)

instance Storable ClayFloatingAttachPoints where
  sizeOf _ = (#size Clay_FloatingAttachPoints)
  alignment _ = (#alignment Clay_FloatingAttachPoints)
  peek ptr = do
    ele <- (#peek Clay_FloatingAttachPoints, element) ptr
    par <- (#peek Clay_FloatingAttachPoints, parent) ptr
    pure $ ClayFloatingAttachPoints ele par
  poke ptr (ClayFloatingAttachPoints ele par) = do
    (#poke Clay_FloatingAttachPoints, element) ptr ele
    (#poke Clay_FloatingAttachPoints, parent) ptr par 

-- | Controls how mouse pointer events like hover and click are captured or passed 
-- through to elements underneath a floating element.
newtype ClayPointerCaptureMode = ClayPointerCaptureMode CUChar deriving (Eq, Show)

-- | (default) "Capture" the pointer event and don't allow events like hover and click to pass through to elements underneath.
clayPointerCaptureModeCapture :: ClayPointerCaptureMode
clayPointerCaptureModeCapture = ClayPointerCaptureMode 0

pattern ClayPointerCaptureModeCapture :: ClayPointerCaptureMode
pattern ClayPointerCaptureModeCapture = ClayPointerCaptureMode 0

-- | Transparently pass through pointer events like hover and click to elements underneath the floating element.
clayPointerCaptureModePassthrough :: ClayPointerCaptureMode
clayPointerCaptureModePassthrough = ClayPointerCaptureMode 1

pattern ClayPointerCaptureModePassthrough :: ClayPointerCaptureMode
pattern ClayPointerCaptureModePassthrough = ClayPointerCaptureMode 1

instance Storable ClayPointerCaptureMode where
  sizeOf _ = (#size uint8_t)
  alignment _ = (#alignment uint8_t)
  peek ptr = do
    val <- peek (castPtr ptr) :: IO CUChar 
    pure $ (ClayPointerCaptureMode val)
  poke ptr (ClayPointerCaptureMode i) = poke (castPtr ptr) i

-- | Controls which element a floating element is "attached" to (i.e. relative offset from).
newtype ClayFloatingAttachToElement = ClayFloatingAttachToElement CUChar deriving (Eq, Show)

-- | (default) Disables floating for this element.
clayAttachToNone :: ClayFloatingAttachToElement
clayAttachToNone = ClayFloatingAttachToElement 0

pattern ClayAttachToNone :: ClayFloatingAttachToElement
pattern ClayAttachToNone = ClayFloatingAttachToElement 0

-- | Attaches this floating element to its parent, positioned based on the .attachPoints and .offset fields.
clayAttachToParent :: ClayFloatingAttachToElement
clayAttachToParent = ClayFloatingAttachToElement 1

pattern ClayAttachToParent :: ClayFloatingAttachToElement
pattern ClayAttachToParent = ClayFloatingAttachToElement 1

-- | Attaches this floating element to an element with a specific ID, specified with the .parentId field. positioned based on the .attachPoints and .offset fields.
clayAttachToElementWithId :: ClayFloatingAttachToElement
clayAttachToElementWithId = ClayFloatingAttachToElement 2

pattern ClayAttachToElementWithId :: ClayFloatingAttachToElement
pattern ClayAttachToElementWithId = ClayFloatingAttachToElement 2

-- | Attaches this floating element to the root of the layout, which combined with the .offset field provides functionality similar to "absolute positioning".
clayAttachToRoot :: ClayFloatingAttachToElement
clayAttachToRoot = ClayFloatingAttachToElement 3

pattern ClayAttachToRoot :: ClayFloatingAttachToElement 
pattern ClayAttachToRoot = ClayFloatingAttachToElement 3

instance Storable ClayFloatingAttachToElement where
  sizeOf _ = (#size uint8_t)
  alignment _ = (#alignment uint8_t)
  peek ptr = do
    val <- peek (castPtr ptr) :: IO CUChar 
    pure $ (ClayFloatingAttachToElement val)
  poke ptr (ClayFloatingAttachToElement i) = poke (castPtr ptr) i

-- | Controls various settings related to "floating" elements, which are elements that "float" above other elements, potentially overlapping their boundaries,
-- and not affecting the layout of sibling or parent elements.
data ClayFloatingElementConfig = ClayFloatingElementConfig {
  -- | Offsets this floating element by the provided x,y coordinates from its attachPoints.
  clayFloatingElementConfigOffset :: ClayVector2,
  -- | Expands the boundaries of the outer floating element without affecting its children.
  clayFloatingElementConfigExpand :: ClayDimensions,
  -- | When used in conjunction with .attachTo = CLAY_ATTACH_TO_ELEMENT_WITH_ID, attaches this floating element to the element in the hierarchy with the provided ID.
  -- Hint: attach the ID to the other element with .id = CLAY_ID("yourId"), and specify the id the same way, with .parentId = CLAY_ID("yourId").id 
  clayFloatingElementConfigParentId :: CUInt,
  -- | Controls the z index of this floating element and all its children. Floating elements are sorted in ascending z order before output. 
  -- zIndex is also passed to the renderer for all elements contained within this floating element.
  clayFloatingElementConfigZIndex :: CShort,
  -- | Controls how mouse pointer events like hover and click are captured or passed through to elements underneath / behind a floating element.
  -- Enum is of the form CLAY_ATTACH_POINT_foo_bar. See Clay_FloatingAttachPoints for more details.
  -- Note: see https://github.com/user-attachments/assets/b8c6dfaa-c1b1-41a4-be55-013473e4a6ce
  -- and <img src="https://github.com/user-attachments/assets/ebe75e0d-1904-46b0-982d-418f929d1516 /> for a visual explanation.
  clayFloatingElementConfigAttachPoints :: ClayFloatingAttachPoints,
  -- | Controls how mouse pointer events like hover and click are captured or passed through to elements underneath a floating element.
  -- CLAY_POINTER_CAPTURE_MODE_CAPTURE (default) - "Capture" the pointer event and don't allow events like hover and click to pass through to elements underneath.
  -- CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH - Transparently pass through pointer events like hover and click to elements underneath the floating element.
  clayFloatingElementConfigPointerCaptureMode :: ClayPointerCaptureMode,
  -- | Controls which element a floating element is "attached" to (i.e. relative offset from).
  -- CLAY_ATTACH_TO_NONE (default) - Disables floating for this element.
  -- CLAY_ATTACH_TO_PARENT - Attaches this floating element to its parent, positioned based on the .attachPoints and .offset fields.
  -- CLAY_ATTACH_TO_ELEMENT_WITH_ID - Attaches this floating element to an element with a specific ID, specified with the .parentId field. positioned based on the .attachPoints and .offset fields.
  -- CLAY_ATTACH_TO_ROOT - Attaches this floating element to the root of the layout, which combined with the .offset field provides functionality similar to "absolute positioning".
  clayFloatingElementConfigAttachTo :: ClayFloatingAttachToElement
} deriving (Eq, Show)

instance Storable ClayFloatingElementConfig where
  sizeOf _ = (#size Clay_FloatingElementConfig)
  alignment _ = (#alignment Clay_FloatingElementConfig)
  peek ptr = do
    off <- (#peek Clay_FloatingElementConfig, offset) ptr
    epn <- (#peek Clay_FloatingElementConfig, expand) ptr
    pid <- (#peek Clay_FloatingElementConfig, parentId) ptr
    zin <- (#peek Clay_FloatingElementConfig, zIndex) ptr
    atp <- (#peek Clay_FloatingElementConfig, attachPoints) ptr
    pmo <- (#peek Clay_FloatingElementConfig, pointerCaptureMode) ptr
    att <- (#peek Clay_FloatingElementConfig, attachTo) ptr
    pure $ ClayFloatingElementConfig off epn pid zin atp pmo att
  poke ptr (ClayFloatingElementConfig off epn pid zin atp pmo att) = do
    (#poke Clay_FloatingElementConfig, offset) ptr off
    (#poke Clay_FloatingElementConfig, expand) ptr epn
    (#poke Clay_FloatingElementConfig, parentId) ptr pid
    (#poke Clay_FloatingElementConfig, zIndex) ptr zin
    (#poke Clay_FloatingElementConfig, attachPoints) ptr atp
    (#poke Clay_FloatingElementConfig, pointerCaptureMode) ptr pmo
    (#poke Clay_FloatingElementConfig, attachTo) ptr att

-- * Custom

-- | Controls various settings related to custom elements.
data ClayCustomElementConfig = ClayCustomElementConfig {
  -- | A transparent pointer through which you can pass custom data to the renderer.
  -- Generates CUSTOM render commands.
  clayCustomElementConfigCustomData :: Ptr ()
} deriving (Eq, Show)

instance Storable ClayCustomElementConfig where
  sizeOf _ = (#size Clay_CustomElementConfig)
  alignment _ = (#alignment Clay_CustomElementConfig)
  peek ptr = do
    cdt <- (#peek Clay_CustomElementConfig, customData) ptr
    pure $ ClayCustomElementConfig cdt
  poke ptr (ClayCustomElementConfig cdt) = do
    (#poke Clay_CustomElementConfig, customData) ptr cdt

-- * Scroll

-- | Controls the axis on which an element switches to "scrolling", which clips the contents and allows scrolling in that direction.
data ClayScrollElementConfig = ClayScrollElementConfig {
  -- | Clip overflowing elements on the X axis and allow scrolling left and right.
  clayScrollElementConfigHorizontal :: CBool,
  -- | Clip overflowing elements on the YU axis and allow scrolling up and down.
  clayScrollElementConfigVertical :: CBool
} deriving (Eq, Show)

instance Storable ClayScrollElementConfig where
  sizeOf _ = (#size Clay_ScrollElementConfig)
  alignment _ = (#size Clay_ScrollElementConfig)
  peek ptr = do
    hor <- (#peek Clay_ScrollElementConfig, horizontal) ptr
    ver <- (#peek Clay_ScrollElementConfig, vertical) ptr
    pure $ ClayScrollElementConfig hor ver
  poke ptr (ClayScrollElementConfig hor ver) = do
    (#poke Clay_ScrollElementConfig, horizontal) ptr hor
    (#poke Clay_ScrollElementConfig, vertical) ptr ver

-- * Border

-- | Controls the widths of individual element borders.
data ClayBorderWidth = ClayBorderWidth {
  clayBorderWidthLeft :: CUShort,
  clayBorderWidthRight :: CUShort,
  clayBorderWidthTop :: CUShort,
  clayBorderWidthBottom :: CUShort,
  -- | Creates borders between each child element, depending on the .layoutDirection.
  -- e.g. for LEFT_TO_RIGHT, borders will be vertical lines, and for TOP_TO_BOTTOM borders will be horizontal lines.
  -- .betweenChildren borders will result in individual RECTANGLE render commands being generated.
  clayBorderWidthBetweenChildren :: CUShort
} deriving (Eq, Show)

instance Storable ClayBorderWidth where
  sizeOf _ = (#size Clay_BorderWidth)
  alignment _ = (#alignment Clay_BorderWidth)
  peek ptr = do
    left <- (#peek Clay_BorderWidth, left) ptr
    right <- (#peek Clay_BorderWidth, right) ptr
    top <- (#peek Clay_BorderWidth, top) ptr
    bottom <- (#peek Clay_BorderWidth, bottom) ptr
    btw <- (#peek Clay_BorderWidth, betweenChildren) ptr
    pure $ ClayBorderWidth left right top bottom btw
  poke ptr (ClayBorderWidth left right top bottom btw) = do
    (#poke Clay_BorderWidth, left) ptr left
    (#poke Clay_BorderWidth, right) ptr right
    (#poke Clay_BorderWidth, top) ptr top
    (#poke Clay_BorderWidth, bottom) ptr bottom
    (#poke Clay_BorderWidth, betweenChildren) ptr btw

-- | Controls settings related to element borders.
data ClayBorderElementConfig = ClayBorderElementConfig {
  -- | Controls the color of all borders with width > 0. Conventionally represented as 0-255, but interpretation is up to the renderer.
  clayBorderElementConfigColor :: ClayColor,
  -- | Controls the widths of individual borders. At least one of these should be > 0 for a BORDER render command to be generated.
  clayBorderElementConfigWidth :: ClayBorderWidth
} deriving (Eq, Show)

instance Storable ClayBorderElementConfig where
  sizeOf _ = (#size Clay_BorderElementConfig)
  alignment _ = (#alignment Clay_BorderElementConfig)
  peek ptr = do
    col <- (#peek Clay_BorderElementConfig, color) ptr
    wid <- (#peek Clay_BorderElementConfig, width) ptr
    pure $ ClayBorderElementConfig col wid
  poke ptr (ClayBorderElementConfig col wid) = do
    (#poke Clay_BorderElementConfig, color) ptr col
    (#poke Clay_BorderElementConfig, width) ptr wid

-- * Render Command Data

-- | Render command data when commandType == CLAY_RENDER_COMMAND_TYPE_TEXT
data ClayTextRenderData = ClayTextRenderData {
  -- | A string slice containing the text to be rendered.
  -- Note: this is not guaranteed to be null terminated.
  clayTextRenderDataStringContents :: ClayStringSlice,
  -- Conventionally represented as 0-255 for each channel, but interpretation is up to the renderer.
  clayTextRenderDataTextColor :: ClayColor,
  -- An integer representing the font to use to render this text, transparently passed through from the text declaration.
  clayTextRenderDataFontId :: CUShort,
  clayTextRenderDataFontSize :: CUShort,
  -- Specifies the extra whitespace gap in pixels between each character.
  clayTextRenderDataLetterSpacing :: CUShort,
  -- The height of the bounding box for this line of text.
  clayTextRenderDataLineHeight :: CUShort
} deriving (Eq, Show)

instance Storable ClayTextRenderData where
  sizeOf _ = (#size Clay_TextRenderData)
  alignment _ = (#alignment Clay_TextRenderData)
  peek ptr = do
    sco <- (#peek Clay_TextRenderData, stringContents) ptr
    tco <- (#peek Clay_TextRenderData, textColor) ptr
    fid <- (#peek Clay_TextRenderData, fontId) ptr
    fsi <- (#peek Clay_TextRenderData, fontSize) ptr
    lsp <- (#peek Clay_TextRenderData, letterSpacing) ptr
    lih <- (#peek Clay_TextRenderData, lineHeight) ptr
    pure $ ClayTextRenderData sco tco fid fsi lsp lih
  poke ptr (ClayTextRenderData sco tco fid fsi lsp lih) = do
    (#poke Clay_TextRenderData, stringContents) ptr sco
    (#poke Clay_TextRenderData, textColor) ptr tco
    (#poke Clay_TextRenderData, fontId) ptr fid
    (#poke Clay_TextRenderData, fontSize) ptr fsi
    (#poke Clay_TextRenderData, letterSpacing) ptr lsp
    (#poke Clay_TextRenderData, lineHeight) ptr lih

-- | Render command data when commandType == CLAY_RENDER_COMMAND_TYPE_RECTANGLE
data ClayRectangleRenderData = ClayRectangleRenderData {
  -- | The solid background color to fill this rectangle with. Conventionally represented as 0-255 for each channel, but interpretation is up to the renderer.
  clayRectangleRenderDataBackgroundColor :: ClayColor,
  -- | Controls the "radius", or corner rounding of elements, including rectangles, borders and images.
  -- The rounding is determined by drawing a circle inset into the element corner by (radius, radius) pixels.
  clayRectangleRenderDataCornerRadius :: ClayCornerRadius
} deriving (Eq, Show)

instance Storable ClayRectangleRenderData where
  sizeOf _ = (#size Clay_RectangleRenderData)
  alignment _ = (#alignment Clay_RectangleRenderData)
  peek ptr = do
    col <- (#peek Clay_RectangleRenderData, backgroundColor) ptr
    cor <- (#peek Clay_RectangleRenderData, cornerRadius) ptr
    pure $ ClayRectangleRenderData col cor
  poke ptr (ClayRectangleRenderData col cor) = do
    (#poke Clay_RectangleRenderData, backgroundColor) ptr col
    (#poke Clay_RectangleRenderData, cornerRadius) ptr cor

-- | Render command data when commandType == CLAY_RENDER_COMMAND_TYPE_IMAGE
data ClayImageRenderData = ClayImageRenderData {
  -- | The tint color for this image. Note that the default value is 0,0,0,0 and should likely be interpreted
  -- as "untinted".
  -- Conventionally represented as 0-255 for each channel, but interpretation is up to the renderer.
  clayImageRenderDataBackgroundColor :: ClayColor,
  -- | Controls the "radius", or corner rounding of this image.
  -- The rounding is determined by drawing a circle inset into the element corner by (radius, radius) pixels.
  clayImageRenderDataCornerRadius :: ClayCornerRadius,
  -- | The original dimensions of the source image, used to control aspect ratio.
  clayImageRenderDataSourceDimensions :: ClayDimensions,
  -- | A pointer transparently passed through from the original element definition, typically used to represent image data.
  clayImageRenderDataImageData :: Ptr ()
} deriving (Eq, Show)

instance Storable ClayImageRenderData where
  sizeOf _ = (#size Clay_ImageRenderData)
  alignment _ = (#alignment Clay_ImageRenderData)
  peek ptr = do
    col <- (#peek Clay_ImageRenderData, backgroundColor) ptr
    cor <- (#peek Clay_ImageRenderData, cornerRadius) ptr
    dim <- (#peek Clay_ImageRenderData, sourceDimensions) ptr
    img <- (#peek Clay_ImageRenderData, imageData) ptr
    pure $ ClayImageRenderData col cor dim img
  poke ptr (ClayImageRenderData col cor dim img) = do
    (#poke Clay_ImageRenderData, backgroundColor) ptr col
    (#poke Clay_ImageRenderData, cornerRadius) ptr cor
    (#poke Clay_ImageRenderData, sourceDimensions) ptr dim
    (#poke Clay_ImageRenderData, imageData) ptr img

data ClayCustomRenderData = ClayCustomRenderData {
  clayCustomRenderDataBackgroundColor :: ClayColor,
  clayCustomRenderDataCornerRadius :: ClayCornerRadius,
  clayCustomRenderDataCustomData :: Ptr ()
} deriving (Eq, Show)

instance Storable ClayCustomRenderData where
  sizeOf _ = (#size Clay_CustomRenderData)
  alignment _ = (#alignment Clay_CustomRenderData)
  peek ptr = do
    col <- (#peek Clay_CustomRenderData, backgroundColor) ptr
    cor <- (#peek Clay_CustomRenderData, cornerRadius) ptr
    dat <- (#peek Clay_CustomRenderData, customData) ptr
    pure $ ClayCustomRenderData col cor dat
  poke ptr (ClayCustomRenderData col cor dat) = do
    (#poke Clay_CustomRenderData, backgroundColor) ptr col
    (#poke Clay_CustomRenderData, cornerRadius) ptr cor
    (#poke Clay_CustomRenderData, customData) ptr dat

data ClayScrollRenderData = ClayScrollRenderData {
  clayScrollRenderDataHorizontal :: CBool,
  clayScrollRenderDataVertical :: CBool
} deriving (Eq, Show)

instance Storable ClayScrollRenderData where
  sizeOf _ = (#size Clay_ScrollRenderData)
  alignment _ = (#alignment Clay_ScrollRenderData)
  peek ptr = do
    hor <- (#peek Clay_ScrollRenderData, horizontal) ptr
    ver <- (#peek Clay_ScrollRenderData, vertical) ptr
    pure $ ClayScrollRenderData hor ver
  poke ptr (ClayScrollRenderData hor ver) = do
    (#poke Clay_ScrollRenderData, horizontal) ptr hor
    (#poke Clay_ScrollRenderData, vertical) ptr ver

data ClayBorderRenderData = ClayBorderRenderData {
  clayBorderRenderDataColor :: ClayColor,
  clayBorderRenderDataCornerRadius :: ClayCornerRadius,
  clayBorderRenderDataWidth :: ClayBorderWidth
} deriving (Eq, Show)

instance Storable ClayBorderRenderData where
  sizeOf _ = (#size Clay_BorderRenderData)
  alignment _ = (#alignment Clay_BorderRenderData)
  peek ptr = do
    col <- (#peek Clay_BorderRenderData, color) ptr
    cor <- (#peek Clay_BorderRenderData, cornerRadius) ptr
    wid <- (#peek Clay_BorderRenderData, width) ptr
    pure $ ClayBorderRenderData col cor wid
  poke ptr (ClayBorderRenderData col cor wid) = do
    (#poke Clay_BorderRenderData, color) ptr col
    (#poke Clay_BorderRenderData, cornerRadius) ptr cor
    (#poke Clay_BorderRenderData, width) ptr wid

data ClayRenderData
  = ClayRenderDataRectangle ClayRectangleRenderData
  | ClayRenderDataText ClayTextRenderData
  | ClayRenderDataImage ClayImageRenderData
  | ClayRenderDataCustom ClayCustomRenderData
  | ClayRenderDataBorder ClayBorderRenderData
  | ClayRenderDataScroll ClayScrollRenderData
  | ClayRenderDataNone deriving (Eq, Show)

-- * Miscellaneous Stucts & Enums

data ClayScrollContainerData = ClayScrollContainerData {
  clayScrollContainerDataScrollPosition :: Ptr ClayVector2,
  clayScrollContainerDataScrollContainerDimensions :: ClayDimensions,
  clayScrollContianerDataContentDimensions :: ClayDimensions,
  clayScrollContainerDataConfig :: ClayScrollElementConfig,
  clayScrollContainerDataFound :: CBool
} deriving (Eq, Show)

instance Storable ClayScrollContainerData where
  sizeOf _ = (#size Clay_ScrollContainerData)
  alignment _ = (#alignment Clay_ScrollContainerData)
  peek ptr = do
    pos <- (#peek Clay_ScrollContainerData, scrollPosition) ptr
    sdim <- (#peek Clay_ScrollContainerData, scrollContainerDimensions) ptr
    cdim <- (#peek Clay_ScrollContainerData, contentDimensions) ptr
    con <- (#peek Clay_ScrollContainerData, config) ptr
    fnd <- (#peek Clay_ScrollContainerData, found) ptr
    pure $ ClayScrollContainerData pos sdim cdim con fnd
  poke ptr (ClayScrollContainerData pos sdim cdim con fnd) = do
    (#poke Clay_ScrollContainerData, scrollPosition) ptr pos
    (#poke Clay_ScrollContainerData, scrollContainerDimensions) ptr sdim
    (#poke Clay_ScrollContainerData, contentDimensions) ptr cdim
    (#poke Clay_ScrollContainerData, config) ptr con
    (#poke Clay_ScrollContainerData, found) ptr fnd


data ClayElementData = ClayElementData {
  clayElementDataBoundingBox :: ClayBoundingBox,
  clayElementDataFound :: CBool
} deriving (Eq, Show)

instance Storable ClayElementData where
  sizeOf _ = (#size Clay_ElementData)
  alignment _ = (#alignment Clay_ElementData)
  peek ptr = do
    box <- (#peek Clay_ElementData, boundingBox) ptr
    fnd <- (#peek Clay_ElementData, found) ptr
    pure $ ClayElementData box fnd
  poke ptr (ClayElementData box fnd) = do
    (#poke Clay_ElementData, boundingBox) ptr box
    (#poke Clay_ElementData, found) ptr fnd

newtype ClayRenderCommandType = ClayRenderCommandType CUChar deriving (Eq, Show)

clayRenderCommandTypeNone :: ClayRenderCommandType
clayRenderCommandTypeNone = ClayRenderCommandType 0

pattern ClayRenderCommandTypeNone :: ClayRenderCommandType
pattern ClayRenderCommandTypeNone = ClayRenderCommandType 0

clayRenderCommandTypeRectangle :: ClayRenderCommandType
clayRenderCommandTypeRectangle = ClayRenderCommandType 1

pattern ClayRenderCommandTypeRectangle :: ClayRenderCommandType
pattern ClayRenderCommandTypeRectangle = ClayRenderCommandType 1

clayRenderCommandTypeBorder :: ClayRenderCommandType
clayRenderCommandTypeBorder = ClayRenderCommandType 2

pattern ClayRenderCommandTypeBorder :: ClayRenderCommandType
pattern ClayRenderCommandTypeBorder = ClayRenderCommandType 2

clayRenderCommandTypeText :: ClayRenderCommandType
clayRenderCommandTypeText = ClayRenderCommandType 3

pattern ClayRenderCommandTypeText :: ClayRenderCommandType
pattern ClayRenderCommandTypeText = ClayRenderCommandType 3

clayRenderCommandTypeImage :: ClayRenderCommandType
clayRenderCommandTypeImage = ClayRenderCommandType 4

pattern ClayRenderCommandTypeImage :: ClayRenderCommandType
pattern ClayRenderCommandTypeImage = ClayRenderCommandType 4

clayRenderCommandTypeScissorStart :: ClayRenderCommandType
clayRenderCommandTypeScissorStart = ClayRenderCommandType 5

pattern ClayRenderCommandTypeScissorStart :: ClayRenderCommandType
pattern ClayRenderCommandTypeScissorStart = ClayRenderCommandType 5

clayRenderCommandTypeScissorEnd :: ClayRenderCommandType
clayRenderCommandTypeScissorEnd = ClayRenderCommandType 6

pattern ClayRenderCommandTypeScissorEnd :: ClayRenderCommandType
pattern ClayRenderCommandTypeScissorEnd = ClayRenderCommandType 6

clayRenderCommandTypeCustom :: ClayRenderCommandType
clayRenderCommandTypeCustom = ClayRenderCommandType 7

pattern ClayRenderCommandTypeCustom :: ClayRenderCommandType
pattern ClayRenderCommandTypeCustom = ClayRenderCommandType 7

instance Storable ClayRenderCommandType where
  sizeOf _ = (#size uint8_t)
  alignment _ = (#alignment uint8_t)
  peek ptr = do
    val <- peek (castPtr ptr) :: IO CUChar 
    pure $ (ClayRenderCommandType val)
  poke ptr (ClayRenderCommandType i) = poke (castPtr ptr) i

data ClayRenderCommand = ClayRenderCommand {
  clayRenderCommandBoundingBox :: ClayBoundingBox,
  clayRenderCommandRenderData :: ClayRenderData,
  clayRenderCommandUserData :: Ptr (),
  clayRenderCommandId :: CUInt,
  clayRenderCommandZIndex :: CShort,
  clayRenderCommandCommandType :: ClayRenderCommandType
} deriving (Eq, Show)

instance Storable ClayRenderCommand where
  sizeOf _ = (#size Clay_RenderCommand)
  alignment _ = (#alignment Clay_RenderCommand)
  peek ptr = do
    box <- (#peek Clay_RenderCommand, boundingBox) ptr
    dat <- (#peek Clay_RenderCommand, userData) ptr
    ide <- (#peek Clay_RenderCommand, id) ptr
    zid <- (#peek Clay_RenderCommand, zIndex) ptr
    typ <- (#peek Clay_RenderCommand, commandType) ptr

    ren <- case typ of
      ClayRenderCommandTypeRectangle -> 
          ClayRenderDataRectangle <$> (#peek Clay_RenderCommand, renderData.rectangle) ptr
      ClayRenderCommandTypeBorder -> 
          ClayRenderDataBorder <$> (#peek Clay_RenderCommand, renderData.border) ptr
      ClayRenderCommandTypeText -> 
          ClayRenderDataText <$> (#peek Clay_RenderCommand, renderData.text) ptr
      ClayRenderCommandTypeImage -> 
          ClayRenderDataImage <$> (#peek Clay_RenderCommand, renderData.image) ptr
      ClayRenderCommandTypeScissorStart -> 
          ClayRenderDataScroll <$> (#peek Clay_RenderCommand, renderData.scroll) ptr
      ClayRenderCommandTypeScissorEnd -> 
          ClayRenderDataScroll <$> (#peek Clay_RenderCommand, renderData.scroll) ptr
      ClayRenderCommandTypeCustom -> 
          ClayRenderDataCustom <$> (#peek Clay_RenderCommand, renderData.custom) ptr
      ClayRenderCommandType _ -> pure ClayRenderDataNone
    
    pure $ ClayRenderCommand box ren dat ide zid typ
  poke ptr (ClayRenderCommand box ren dat ide zid typ) = do
    (#poke Clay_RenderCommand, boundingBox) ptr box
    (#poke Clay_RenderCommand, userData) ptr dat
    (#poke Clay_RenderCommand, id) ptr ide
    (#poke Clay_RenderCommand, zIndex) ptr zid
    (#poke Clay_RenderCommand, commandType) ptr typ
    case ren of
      ClayRenderDataRectangle d -> (#poke Clay_RenderCommand, renderData) ptr d
      ClayRenderDataBorder d -> (#poke Clay_RenderCommand, renderData) ptr d
      ClayRenderDataText d -> (#poke Clay_RenderCommand, renderData) ptr d
      ClayRenderDataImage d -> (#poke Clay_RenderCommand, renderData) ptr d
      ClayRenderDataScroll d -> (#poke Clay_RenderCommand, renderData) ptr d
      ClayRenderDataCustom d -> (#poke Clay_RenderCommand, renderData) ptr d
      _ -> pure ()

newtype ClayPointerDataInteractionState = ClayPointerDataInteractionState CUChar

clayPointerDataPressedThisFrame :: ClayPointerDataInteractionState
clayPointerDataPressedThisFrame = ClayPointerDataInteractionState 0

clayPointerDataPressed :: ClayPointerDataInteractionState
clayPointerDataPressed = ClayPointerDataInteractionState 1

clayPointerDataReleasedThisFrame :: ClayPointerDataInteractionState
clayPointerDataReleasedThisFrame = ClayPointerDataInteractionState 2

clayPointerDataReleased :: ClayPointerDataInteractionState
clayPointerDataReleased = ClayPointerDataInteractionState 3

instance Storable ClayPointerDataInteractionState where
  sizeOf _ = (#size uint8_t)
  alignment _ = (#alignment uint8_t)
  peek ptr = do
    val <- peek (castPtr ptr) :: IO CUChar 
    pure $ (ClayPointerDataInteractionState val)
  poke ptr (ClayPointerDataInteractionState i) = poke (castPtr ptr) i

data ClayPointerData = ClayPointerData {
  clayPointerDataPosition :: ClayVector2,
  clayPointerDataState :: ClayPointerDataInteractionState
}

instance Storable ClayPointerData where
  sizeOf _ = (#size Clay_PointerData)
  alignment _ = (#alignment Clay_PointerData)
  peek ptr = do
    f1 <- (#peek Clay_PointerData, position) ptr
    f2 <- (#peek Clay_PointerData, state) ptr
    pure $ ClayPointerData f1 f2
  
  poke ptr (ClayPointerData f1 f2) = do
    (#poke Clay_PointerData, position) ptr f1
    (#poke Clay_PointerData, state) ptr f2

data ClayElementDeclaration = ClayElementDeclaration {
  clayElementDeclarationId :: ClayElementId,
  clayElementDeclarationLayout :: ClayLayoutConfig,
  clayElementDeclarationBackgroundColor :: Maybe ClayColor,
  clayElementDeclarationCornerRadius :: Maybe ClayCornerRadius,
  clayElementDeclarationImage :: Maybe ClayImageElementConfig,
  clayElementDeclarationFloating :: Maybe ClayFloatingElementConfig,
  clayElementDeclarationCustom :: Maybe ClayCustomElementConfig,
  clayElementDeclarationScroll :: Maybe ClayScrollElementConfig,
  clayElementDeclarationBorder :: Maybe ClayBorderElementConfig,
  clayElementDeclarationUserData :: Ptr ()
}

-- Clay allows fields on an element declaration to be unset (zero'd)
-- so we need special handling for our peek/poke
instance Storable ClayElementDeclaration where
  sizeOf _ = (#size Clay_ElementDeclaration)
  alignment _ = (#size Clay_ElementDeclaration)
  peek ptr = do
    f1 <- (#peek Clay_ElementDeclaration, id) ptr
    f2 <- (#peek Clay_ElementDeclaration, layout) ptr

    let bgColorPtr = plusPtr ptr (#offset Clay_ElementDeclaration, backgroundColor)
    f3 <- peekMaybe bgColorPtr
    
    let cornerRadiusPtr = plusPtr ptr (#offset Clay_ElementDeclaration, cornerRadius)
    f4 <- peekMaybe cornerRadiusPtr
    
    let imagePtr = plusPtr ptr (#offset Clay_ElementDeclaration, image)
    f5 <- peekMaybe imagePtr
    
    let floatingPtr = plusPtr ptr (#offset Clay_ElementDeclaration, floating)
    f6 <- peekMaybe floatingPtr
    
    let customPtr = plusPtr ptr (#offset Clay_ElementDeclaration, custom)
    f7 <- peekMaybe customPtr
    
    let scrollPtr = plusPtr ptr (#offset Clay_ElementDeclaration, scroll)
    f8 <- peekMaybe scrollPtr
    
    let borderPtr = plusPtr ptr (#offset Clay_ElementDeclaration, border)
    f9 <- peekMaybe borderPtr

    f10 <- (#peek Clay_ElementDeclaration, userData) ptr 
    pure $ ClayElementDeclaration f1 f2 f3 f4 f5 f6 f7 f8 f9 f10
  poke ptr (ClayElementDeclaration f1 f2 f3 f4 f5 f6 f7 f8 f9 f10) = do
    zeroMemory ptr (sizeOf (undefined :: ClayElementDeclaration))

    (#poke Clay_ElementDeclaration, id) ptr f1
    (#poke Clay_ElementDeclaration, layout) ptr f2
    maybe (pure ()) ((#poke Clay_ElementDeclaration, backgroundColor) ptr) f3
    maybe (pure ()) ((#poke Clay_ElementDeclaration, cornerRadius) ptr) f4
    maybe (pure ()) ((#poke Clay_ElementDeclaration, image) ptr) f5
    maybe (pure ()) ((#poke Clay_ElementDeclaration, floating) ptr) f6
    maybe (pure ()) ((#poke Clay_ElementDeclaration, custom) ptr) f7
    maybe (pure ()) ((#poke Clay_ElementDeclaration, scroll) ptr) f8
    maybe (pure ()) ((#poke Clay_ElementDeclaration, border) ptr) f9
    (#poke Clay_ElementDeclaration, userData) ptr f10


newtype ClayErrorType = ClayErrorType CUChar deriving (Eq, Show)

clayErrorTypeTextMeasurementFunctionNotProvided :: ClayErrorType
clayErrorTypeTextMeasurementFunctionNotProvided = ClayErrorType 0

clayErrorTypeArenaCapacityExceeded :: ClayErrorType
clayErrorTypeArenaCapacityExceeded = ClayErrorType 1

clayErrorTypeElementsCapacityExceeded :: ClayErrorType
clayErrorTypeElementsCapacityExceeded = ClayErrorType 2

clayErrorTypeTextMeasurementCapacityExceeded :: ClayErrorType
clayErrorTypeTextMeasurementCapacityExceeded = ClayErrorType 3

clayErrorTypeFloatingContainerParentNotFound :: ClayErrorType
clayErrorTypeFloatingContainerParentNotFound = ClayErrorType 4

clayErrorTypePercentageOver1 :: ClayErrorType
clayErrorTypePercentageOver1 = ClayErrorType 5

clayErrorTypeInternalError :: ClayErrorType
clayErrorTypeInternalError = ClayErrorType 6

instance Storable ClayErrorType where
  sizeOf _ = (#size uint8_t)
  alignment _ = (#alignment uint8_t)
  peek ptr = do
    val <- peek (castPtr ptr) :: IO CUChar 
    pure $ (ClayErrorType val)
  poke ptr (ClayErrorType i) = poke (castPtr ptr) i

data ClayErrorData = ClayErrorData {
  clayErrorDataErrorType :: ClayErrorType,
  clayErrorDataErrorText :: ClayString,
  clayErrorDataUserData :: Ptr ()
} deriving (Eq, Show)

instance Storable ClayErrorData where
  sizeOf _ = (#size Clay_ErrorData)
  alignment _ = (#alignment Clay_ErrorData)
  peek ptr = do
    f1 <- (#peek Clay_ErrorData, errorType) ptr
    f2 <- (#peek Clay_ErrorData, errorText) ptr
    f3 <- (#peek Clay_ErrorData, userData) ptr
    pure $ ClayErrorData f1 f2 f3
  poke ptr (ClayErrorData f1 f2 f3) = do
    (#poke Clay_ErrorData, errorType) ptr f1
    (#poke Clay_ErrorData, errorText) ptr f2
    (#poke Clay_ErrorData, userData) ptr f3

type ClayErrorHandlerFunction = ClayErrorData -> IO ()

data ClayErrorHandler = ClayErrorHandler {
  clayErrorHandlerErrorHandlerFunction :: FunPtr ClayErrorHandlerFunction,
  clayErrorHandlerUserData :: Ptr ()
}

instance Storable ClayErrorHandler where
  sizeOf _ = (#size Clay_ErrorHandler)
  alignment _ = (#alignment Clay_ErrorHandler)
  peek ptr = do
    f1 <- (#peek Clay_ErrorHandler, errorHandlerFunction) ptr
    f2 <- (#peek Clay_ErrorHandler, userData) ptr
    pure $ ClayErrorHandler f1 f2
  
  poke ptr (ClayErrorHandler f1 f2) = do
    (#poke Clay_ErrorHandler, errorHandlerFunction) ptr f1
    (#poke Clay_ErrorHandler, userData) ptr f2

type ClayErrorHandlerFunctionPtr = (Ptr ClayErrorData) -> IO ()

data ClayHelperErrorHandlerWrapper = ClayHelperErrorHandlerWrapper {
  clayHelperErrorHandlerWrapperErrorHandlerFunction :: FunPtr ClayErrorHandlerFunctionPtr
}

instance Storable ClayHelperErrorHandlerWrapper where
  sizeOf _ = (#size ClayHelper_ErrorHandlerWrapper)
  alignment _ = (#alignment ClayHelper_ErrorHandlerWrapper)
  peek ptr = do
    h <- (#peek ClayHelper_ErrorHandlerWrapper, errorHandlerFunction) ptr
    pure $ ClayHelperErrorHandlerWrapper h
  poke ptr (ClayHelperErrorHandlerWrapper h) = do
    (#poke ClayHelper_ErrorHandlerWrapper, errorHandlerFunction) ptr h

data ClayBooleanWarnings = ClayBooleanWarnings {
  clayBooleanWarningsMaxElementsExceeded :: CBool,
  clayBooleanWarningsMaxRenderCommandsExceeded :: CBool,
  clayBooleanWarningsMaxTextMeasureCacheExceeded :: CBool,
  clayBooleanWarningsTextMeasurementFunctionNotSet :: CBool
} deriving (Eq, Show)

instance Storable ClayBooleanWarnings where
  sizeOf _ = (#size Clay_BooleanWarnings)
  alignment _ = (#alignment Clay_BooleanWarnings)
  peek ptr = do
    maxEles <- (#peek Clay_BooleanWarnings, maxElementsExceeded) ptr
    maxRend <- (#peek Clay_BooleanWarnings, maxRenderCommandsExceeded) ptr
    maxText <- (#peek Clay_BooleanWarnings, maxTextMeasureCacheExceeded) ptr
    textMes <- (#peek Clay_BooleanWarnings, textMeasurementFunctionNotSet) ptr
    pure $ ClayBooleanWarnings maxEles maxRend maxText textMes
  poke ptr (ClayBooleanWarnings maxEles maxRend maxText textMes) = do
    (#poke Clay_BooleanWarnings, maxElementsExceeded) ptr maxEles
    (#poke Clay_BooleanWarnings, maxRenderCommandsExceeded) ptr maxRend
    (#poke Clay_BooleanWarnings, maxTextMeasureCacheExceeded) ptr maxText 
    (#poke Clay_BooleanWarnings, textMeasurementFunctionNotSet) ptr textMes 

data ClayWarning = ClayWarning {
  clayWarningBaseMessage :: ClayString,
  clayWarningDynamicMessage :: ClayString
}

instance Storable ClayWarning where
  sizeOf _ = (#size Clay__Warning)
  alignment _ = (#alignment Clay__Warning)
  peek ptr = do
    bm <- (#peek Clay__Warning, baseMessage) ptr
    dm <- (#peek Clay__Warning, dynamicMessage) ptr
    pure $ ClayWarning bm dm
  poke ptr (ClayWarning bm dm) = do
    (#poke Clay__Warning, baseMessage) ptr bm
    (#poke Clay__Warning, dynamicMessage) ptr dm

data ClaySharedElementConfig = ClaySharedElementConfig {
  claySharedElementConfigBackgroundColor :: ClayColor,
  claySharedElementConfigCornerRadius :: ClayCornerRadius,
  claySharedElementConfigUserData :: Ptr ()
}

instance Storable ClaySharedElementConfig where
  sizeOf _ = (#size Clay_SharedElementConfig)
  alignment _ = (#alignment Clay_SharedElementConfig)
  peek ptr = do
    bgc <- (#peek Clay_SharedElementConfig, backgroundColor) ptr
    crd <- (#peek Clay_SharedElementConfig, cornerRadius) ptr
    dat <- (#peek Clay_SharedElementConfig, userData) ptr
    pure $ ClaySharedElementConfig bgc crd dat
  poke ptr (ClaySharedElementConfig bgc crd dat) = do
    (#poke Clay_SharedElementConfig, backgroundColor) ptr bgc
    (#poke Clay_SharedElementConfig, cornerRadius) ptr crd
    (#poke Clay_SharedElementConfig, userData) ptr dat

newtype ClayElementConfigType = ClayElementConfigType CUChar

clayElementConfigTypeNone :: ClayElementConfigType
clayElementConfigTypeNone = ClayElementConfigType 0

pattern ClayElementConfigTypeNone :: ClayElementConfigType
pattern ClayElementConfigTypeNone = ClayElementConfigType 0

clayElementConfigTypeBorder :: ClayElementConfigType
clayElementConfigTypeBorder = ClayElementConfigType 1

pattern ClayElementConfigTypeBorder :: ClayElementConfigType
pattern ClayElementConfigTypeBorder = ClayElementConfigType 1

clayElementConfigTypeFloating :: ClayElementConfigType
clayElementConfigTypeFloating = ClayElementConfigType 2

pattern ClayElementConfigTypeFloating :: ClayElementConfigType
pattern ClayElementConfigTypeFloating = ClayElementConfigType 2

clayElementConfigTypeScroll :: ClayElementConfigType
clayElementConfigTypeScroll = ClayElementConfigType 3

pattern ClayElementConfigTypeScroll :: ClayElementConfigType
pattern ClayElementConfigTypeScroll = ClayElementConfigType 3

clayElementConfigTypeImage :: ClayElementConfigType
clayElementConfigTypeImage = ClayElementConfigType 4

pattern ClayElementConfigTypeImage :: ClayElementConfigType
pattern ClayElementConfigTypeImage = ClayElementConfigType 4

clayElementConfigTypeText :: ClayElementConfigType
clayElementConfigTypeText = ClayElementConfigType 5

pattern ClayElementConfigTypeText :: ClayElementConfigType
pattern ClayElementConfigTypeText = ClayElementConfigType 5

clayElementConfigTypeCustom :: ClayElementConfigType
clayElementConfigTypeCustom = ClayElementConfigType 6

pattern ClayElementConfigTypeCustom :: ClayElementConfigType
pattern ClayElementConfigTypeCustom = ClayElementConfigType 6

clayElementConfigTypeShared :: ClayElementConfigType
clayElementConfigTypeShared = ClayElementConfigType 7

pattern ClayElementConfigTypeShared :: ClayElementConfigType
pattern ClayElementConfigTypeShared = ClayElementConfigType 7

instance Storable ClayElementConfigType where
  sizeOf _ = (#size uint8_t)
  alignment _ = (#alignment uint8_t)
  peek ptr = do
    val <- peek (castPtr ptr) :: IO CUChar 
    pure $ (ClayElementConfigType val)
  poke ptr (ClayElementConfigType i) = poke (castPtr ptr) i

data ClayElementConfigUnion =
  ClayElementConfigText ClayTextElementConfig |
  ClayElementConfigImage ClayImageElementConfig |
  ClayElementConfigFloating ClayFloatingElementConfig |
  ClayElementConfigCustom ClayCustomElementConfig |
  ClayElementConfigScroll ClayScrollElementConfig | 
  ClayElementConfigBorder ClayBorderElementConfig |
  ClayElementConfigShared ClaySharedElementConfig |
  ClayElementConfigNone

data ClayElementConfig = ClayElementConfig {
  clayElementConfigType :: ClayElementConfigType,
  clayElementConfigConfig :: ClayElementConfigUnion
}

instance Storable ClayElementConfig where
  sizeOf _ = (#size Clay_ElementConfig)
  alignment _ = (#alignment Clay_ElementConfig)
  peek ptr = do
    t <- (#peek Clay_ElementConfig, type) ptr
    c <- case t of
            ClayElementConfigTypeText -> 
              ClayElementConfigText <$> (#peek Clay_ElementConfig, config) ptr
            ClayElementConfigTypeImage -> 
              ClayElementConfigImage <$> (#peek Clay_ElementConfig, config) ptr
            ClayElementConfigTypeFloating -> 
              ClayElementConfigFloating <$> (#peek Clay_ElementConfig, config) ptr
            ClayElementConfigTypeCustom -> 
              ClayElementConfigCustom <$> (#peek Clay_ElementConfig, config) ptr
            ClayElementConfigTypeScroll -> 
              ClayElementConfigScroll <$> (#peek Clay_ElementConfig, config) ptr
            ClayElementConfigTypeBorder -> 
              ClayElementConfigBorder <$> (#peek Clay_ElementConfig, config) ptr
            ClayElementConfigTypeShared -> 
              ClayElementConfigShared <$> (#peek Clay_ElementConfig, config) ptr
            _ -> pure ClayElementConfigNone
    pure $ ClayElementConfig t c
  poke ptr (ClayElementConfig t c) = do
    (#poke Clay_ElementConfig, type) ptr t
    case c of
        ClayElementConfigText c' -> (#poke Clay_ElementConfig, config) ptr c'
        ClayElementConfigImage c' -> (#poke Clay_ElementConfig, config) ptr c'
        ClayElementConfigFloating c' -> (#poke Clay_ElementConfig, config) ptr c'
        ClayElementConfigCustom c' -> (#poke Clay_ElementConfig, config) ptr c'
        ClayElementConfigScroll c' -> (#poke Clay_ElementConfig, config) ptr c'
        ClayElementConfigBorder c' -> (#poke Clay_ElementConfig, config) ptr c'
        ClayElementConfigShared c' -> (#poke Clay_ElementConfig, config) ptr c'
        ClayElementConfigNone -> pure ()

data ClayWrappedTextLine = ClayWrappedTextLine {
  clayWrappedTextLineDimensions :: ClayDimensions,
  clayWrappedTextLineLine  :: ClayString
}

instance Storable ClayWrappedTextLine where
  sizeOf _ = (#size Clay__WrappedTextLine)
  alignment _ = (#alignment Clay__WrappedTextLine)
  peek ptr = do
    d <- (#peek Clay__WrappedTextLine, dimensions) ptr
    l <- (#peek Clay__WrappedTextLine, line) ptr
    pure $ ClayWrappedTextLine d l
  poke ptr (ClayWrappedTextLine d l) = do
    (#poke Clay__WrappedTextLine, dimensions) ptr d
    (#poke Clay__WrappedTextLine, line) ptr l

data ClayTextElementData = ClayTextElementData {
  clayTextElementDataText :: ClayString,
  clayTextElementDataPreferredDimensions :: ClayDimensions,
  clayTextElementDataElementIndex :: CInt,
  clayTextElementDataWrappedLines :: ClayArraySlice ClayWrappedTextLine
}

instance Storable ClayTextElementData where
  sizeOf _ = (#size Clay__TextElementData)
  alignment _ = (#alignment Clay__TextElementData)
  peek ptr = do
    t <- (#peek Clay__TextElementData, text) ptr
    p <- (#peek Clay__TextElementData, preferredDimensions) ptr
    e <- (#peek Clay__TextElementData, elementIndex) ptr
    w <- (#peek Clay__TextElementData, wrappedLines) ptr
    pure $ ClayTextElementData t p e w
  poke ptr (ClayTextElementData t p e w) = do
    (#poke Clay__TextElementData, text) ptr t
    (#poke Clay__TextElementData, preferredDimensions) ptr p
    (#poke Clay__TextElementData, elementIndex) ptr e
    (#poke Clay__TextElementData, wrappedLines) ptr w

data ClayLayoutElementChildren = ClayLayoutElementChildren {
  clayLayoutElementChildrenElements :: CInt,
  clayLayoutElementChildrenLength :: CUShort
}

instance Storable ClayLayoutElementChildren where
  sizeOf _ = (#size Clay__LayoutElementChildren)
  alignment _ = (#alignment Clay__LayoutElementChildren)
  peek ptr = do
    e <- (#peek Clay__LayoutElementChildren, elements) ptr
    l <- (#peek Clay__LayoutElementChildren, length) ptr
    pure $ ClayLayoutElementChildren e l
  poke ptr (ClayLayoutElementChildren e l) = do
    (#poke Clay__LayoutElementChildren, elements) ptr e
    (#poke Clay__LayoutElementChildren, length) ptr l

data ClayChildrenOrTextContent =
  ClayChildrenContent ClayLayoutElementChildren | 
  ClayTextContent (Ptr ClayTextElementData)

data ClayLayoutElement = ClayLayoutElement {
  clayLayoutElementChildrenOrTextContent :: ClayChildrenOrTextContent,
  clayLayoutElementDimensions :: ClayDimensions,
  clayLayoutElementMinDimensions :: ClayDimensions,
  clayLayoutElementLayoutConfig :: Ptr ClayLayoutConfig,
  clayLayoutElementElementConfigs :: ClayArraySlice ClayElementConfig,
  clayLayoutElementId :: CUInt
}

pokeClayLayoutElement :: Ptr ClayLayoutElement -> ClayLayoutElement -> IO ()
pokeClayLayoutElement ptr (ClayLayoutElement c d m l e i) = do
  case c of
    ClayChildrenContent c' -> (#poke Clay_LayoutElement, childrenOrTextContent) ptr c'
    ClayTextContent c' -> (#poke Clay_LayoutElement, childrenOrTextContent) ptr c'
  (#poke Clay_LayoutElement, dimensions) ptr d
  (#poke Clay_LayoutElement, minDimensions) ptr m
  (#poke Clay_LayoutElement, layoutConfig) ptr l
  (#poke Clay_LayoutElement, elementConfigs) ptr e
  (#poke Clay_LayoutElement, id) ptr i

-- | In order to get a Storable instance for @ClayLayoutElement@ you have to
-- specify if it has children or text content based on context.
newtype ClayLayoutElementWithChildren = ClayLayoutElementWithChildren ClayLayoutElement

instance Storable ClayLayoutElementWithChildren where
  sizeOf _ = (#size Clay_LayoutElement)
  alignment _ = (#size Clay_LayoutElement)
  peek ptr = do
    c <- ClayChildrenContent <$> (#peek Clay_LayoutElement, childrenOrTextContent) ptr
    d <- (#peek Clay_LayoutElement, dimensions) ptr
    m <- (#peek Clay_LayoutElement, minDimensions) ptr
    l <- (#peek Clay_LayoutElement, layoutConfig) ptr
    e <- (#peek Clay_LayoutElement, elementConfigs) ptr
    i <- (#peek Clay_LayoutElement, id) ptr
    pure $ ClayLayoutElementWithChildren $ ClayLayoutElement c d m l e i
  poke ptr (ClayLayoutElementWithChildren l) = pokeClayLayoutElement (castPtr ptr) l

newtype ClayLayoutElementWithText = ClayLayoutElementWithText ClayLayoutElement

instance Storable ClayLayoutElementWithText where
  sizeOf _ = (#size Clay_LayoutElement)
  alignment _ = (#size Clay_LayoutElement)
  peek ptr = do
    c <- ClayTextContent <$> (#peek Clay_LayoutElement, childrenOrTextContent) ptr
    d <- (#peek Clay_LayoutElement, dimensions) ptr
    m <- (#peek Clay_LayoutElement, minDimensions) ptr
    l <- (#peek Clay_LayoutElement, layoutConfig) ptr
    e <- (#peek Clay_LayoutElement, elementConfigs) ptr
    i <- (#peek Clay_LayoutElement, id) ptr
    pure $ ClayLayoutElementWithText $ ClayLayoutElement c d m l e i
  poke ptr (ClayLayoutElementWithText l) = pokeClayLayoutElement (castPtr ptr) l

data ClayScrollContainerDataInternal = ClayScrollContainerDataInternal {
  clayScrollContainerDataInternalLayoutElement :: ClayLayoutElementWithChildren,
  clayScrollContainerDataInternalBoundingBox :: ClayBoundingBox, 
  clayScrollContainerDataInternalContentSize :: ClayDimensions,
  clayScrollContainerDataInternalScrollOrigin :: ClayVector2,
  clayScrollContainerDataInternalPointerOrigin :: ClayVector2,
  clayScrollContainerDataInternalScrollMomentum :: ClayVector2, 
  clayScrollContainerDataInternalScrollPosition :: ClayVector2,
  clayScrollContainerDataInternalPreviousDelta :: ClayVector2,
  clayScrollContainerDataInternalMomentumTime :: CFloat,
  clayScrollContainerDataInternalElementId :: CInt,
  clayScrollContainerDataInternalOpenThisFrame :: CBool, 
  clayScrollContainerDataInternalPointerScrollActive :: CBool
}

instance Storable ClayScrollContainerDataInternal where
  sizeOf _ = (#size Clay__ScrollContainerDataInternal)
  alignment _ = (#alignment Clay__ScrollContainerDataInternal)
  peek ptr = do
    f1 <- (#peek Clay__ScrollContainerDataInternal, layoutElement) ptr
    f2 <- (#peek Clay__ScrollContainerDataInternal, boundingBox) ptr
    f3 <- (#peek Clay__ScrollContainerDataInternal, contentSize) ptr
    f4 <- (#peek Clay__ScrollContainerDataInternal, scrollOrigin) ptr
    f5 <- (#peek Clay__ScrollContainerDataInternal, pointerOrigin) ptr
    f6 <- (#peek Clay__ScrollContainerDataInternal, scrollMomentum) ptr
    f7 <- (#peek Clay__ScrollContainerDataInternal, scrollPosition) ptr
    f8 <- (#peek Clay__ScrollContainerDataInternal, previousDelta) ptr
    f9 <- (#peek Clay__ScrollContainerDataInternal, momentumTime) ptr
    f10 <- (#peek Clay__ScrollContainerDataInternal, elementId) ptr
    f11 <- (#peek Clay__ScrollContainerDataInternal, openThisFrame) ptr
    f12 <- (#peek Clay__ScrollContainerDataInternal, pointerScrollActive) ptr
    pure $ ClayScrollContainerDataInternal f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12
  poke ptr (ClayScrollContainerDataInternal f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12) = do
    (#poke Clay__ScrollContainerDataInternal, layoutElement) ptr f1
    (#poke Clay__ScrollContainerDataInternal, boundingBox) ptr f2
    (#poke Clay__ScrollContainerDataInternal, contentSize) ptr f3
    (#poke Clay__ScrollContainerDataInternal, scrollOrigin) ptr f4
    (#poke Clay__ScrollContainerDataInternal, pointerOrigin) ptr f5
    (#poke Clay__ScrollContainerDataInternal, scrollMomentum) ptr f6
    (#poke Clay__ScrollContainerDataInternal, scrollPosition) ptr f7
    (#poke Clay__ScrollContainerDataInternal, previousDelta) ptr f8
    (#poke Clay__ScrollContainerDataInternal, momentumTime) ptr f9 
    (#poke Clay__ScrollContainerDataInternal, elementId) ptr f10
    (#poke Clay__ScrollContainerDataInternal, openThisFrame) ptr f11
    (#poke Clay__ScrollContainerDataInternal, pointerScrollActive) ptr f12

data ClayDebugElementData = ClayDebugElementData {
  clayDebugElementDataCollision :: CBool,
  clayDebugElementDataCollapsed :: CBool
}

instance Storable ClayDebugElementData where
  sizeOf _ = (#size Clay__DebugElementData)
  alignment _ = (#alignment Clay__DebugElementData)
  peek ptr = do
    f1 <- (#peek Clay__DebugElementData, collision) ptr
    f2 <- (#peek Clay__DebugElementData, collapsed) ptr
    pure $ ClayDebugElementData f1 f2
  poke ptr (ClayDebugElementData f1 f2) = do
    (#poke Clay__DebugElementData, collision) ptr f1
    (#poke Clay__DebugElementData, collapsed) ptr f2

data ClayLayoutElementHashMapItem = ClayLayoutElementHashMapItem {
  clayLayoutElementHashMapItemBoundingBox :: ClayBoundingBox,
  clayLayoutElementHashMapItemElementId :: ClayElementId,
  clayLayoutElementHashMapItemLayoutElement :: Ptr ClayLayoutElement,
  clayLayoutElementHashMapItemOnHoverFunction :: FunPtr (ClayElementId -> ClayPointerData -> CIntPtr -> IO ()),
  clayLayoutElementHashMapItemHoverFunctionUserData :: CIntPtr,
  clayLayoutElementHashMapItemNextIndex :: CInt,
  clayLayoutElementHashMapItemGeneration :: CUInt,
  clayLayoutElementHashMapItemIdAlias :: CUInt,
  clayLayoutElementHashMapItemDebugData :: Ptr ClayDebugElementData
}

instance Storable ClayLayoutElementHashMapItem where
  sizeOf _ = (#size Clay_LayoutElementHashMapItem)
  alignment _ = (#alignment Clay_LayoutElementHashMapItem)
  peek ptr = do
    f1 <- (#peek Clay_LayoutElementHashMapItem, boundingBox) ptr
    f2 <- (#peek Clay_LayoutElementHashMapItem, elementId) ptr
    f3 <- (#peek Clay_LayoutElementHashMapItem, layoutElement) ptr
    f4 <- (#peek Clay_LayoutElementHashMapItem, onHoverFunction) ptr
    f5 <- (#peek Clay_LayoutElementHashMapItem, hoverFunctionUserData) ptr
    f6 <- (#peek Clay_LayoutElementHashMapItem, nextIndex) ptr
    f7 <- (#peek Clay_LayoutElementHashMapItem, generation) ptr
    f8 <- (#peek Clay_LayoutElementHashMapItem, idAlias) ptr
    f9 <- (#peek Clay_LayoutElementHashMapItem, debugData) ptr
    pure $ ClayLayoutElementHashMapItem f1 f2 f3 f4 f5 f6 f7 f8 f9
  
  poke ptr (ClayLayoutElementHashMapItem f1 f2 f3 f4 f5 f6 f7 f8 f9) = do
    (#poke Clay_LayoutElementHashMapItem, boundingBox) ptr f1
    (#poke Clay_LayoutElementHashMapItem, elementId) ptr f2
    (#poke Clay_LayoutElementHashMapItem, layoutElement) ptr f3
    (#poke Clay_LayoutElementHashMapItem, onHoverFunction) ptr f4
    (#poke Clay_LayoutElementHashMapItem, hoverFunctionUserData) ptr f5
    (#poke Clay_LayoutElementHashMapItem, nextIndex) ptr f6
    (#poke Clay_LayoutElementHashMapItem, generation) ptr f7
    (#poke Clay_LayoutElementHashMapItem, idAlias) ptr f8
    (#poke Clay_LayoutElementHashMapItem, debugData) ptr f9

data ClayMeasuredWord = ClayMeasuredWord {
  clayMeasuredWordStartOffset :: CInt,
  clayMeasuredWordLength :: CInt,
  clayMeasuredWordWidth :: CFloat,
  clayMeasuredWordNext :: CInt
}

instance Storable ClayMeasuredWord where
  sizeOf _ = (#size Clay__MeasuredWord)
  alignment _ = (#alignment Clay__MeasuredWord)
  peek ptr = do
    f1 <- (#peek Clay__MeasuredWord, startOffset) ptr
    f2 <- (#peek Clay__MeasuredWord, length) ptr
    f3 <- (#peek Clay__MeasuredWord, width) ptr
    f4 <- (#peek Clay__MeasuredWord, next) ptr
    pure $ ClayMeasuredWord f1 f2 f3 f4
  
  poke ptr (ClayMeasuredWord f1 f2 f3 f4) = do
    (#poke Clay__MeasuredWord, startOffset) ptr f1
    (#poke Clay__MeasuredWord, length) ptr f2
    (#poke Clay__MeasuredWord, width) ptr f3
    (#poke Clay__MeasuredWord, next) ptr f4

data ClayMeasureTextCacheItem = ClayMeasureTextCacheItem {
  clayMeasureTextCacheItemUnwrappedDimensions :: ClayDimensions,
  clayMeasureTextCacheItemMeasuredWordsStartIndex :: CInt,
  clayMeasureTextCacheItemContainsNewlines :: CBool,
  -- Hash map data
  clayMeasureTextCacheItemId :: CUInt,
  clayMeasureTextCacheItemNextIndex :: CInt,
  clayMeasureTextCacheItemGeneration :: CUInt
}

instance Storable ClayMeasureTextCacheItem where
  sizeOf _ = (#size Clay__MeasureTextCacheItem)
  alignment _ = (#alignment Clay__MeasureTextCacheItem)
  peek ptr = do
    f1 <- (#peek Clay__MeasureTextCacheItem, unwrappedDimensions) ptr
    f2 <- (#peek Clay__MeasureTextCacheItem, measuredWordsStartIndex) ptr
    f3 <- (#peek Clay__MeasureTextCacheItem, containsNewlines) ptr
    f4 <- (#peek Clay__MeasureTextCacheItem, id) ptr
    f5 <- (#peek Clay__MeasureTextCacheItem, nextIndex) ptr
    f6 <- (#peek Clay__MeasureTextCacheItem, generation) ptr
    pure $ ClayMeasureTextCacheItem f1 f2 f3 f4 f5 f6
  
  poke ptr (ClayMeasureTextCacheItem f1 f2 f3 f4 f5 f6) = do
    (#poke Clay__MeasureTextCacheItem, unwrappedDimensions) ptr f1
    (#poke Clay__MeasureTextCacheItem, measuredWordsStartIndex) ptr f2
    (#poke Clay__MeasureTextCacheItem, containsNewlines) ptr f3
    (#poke Clay__MeasureTextCacheItem, id) ptr f4
    (#poke Clay__MeasureTextCacheItem, nextIndex) ptr f5
    (#poke Clay__MeasureTextCacheItem, generation) ptr f6

data ClayLayoutElementTreeNode = ClayLayoutElementTreeNode {
  clayLayoutElementTreeNodeLayoutElement :: Ptr ClayLayoutElement,
  clayLayoutElementTreeNodePosition :: ClayVector2,
  clayLayoutElementTreeNodeNextChildOffset :: ClayVector2
}

instance Storable ClayLayoutElementTreeNode where
  sizeOf _ = (#size Clay__LayoutElementTreeNode)
  alignment _ = (#alignment Clay__LayoutElementTreeNode)
  peek ptr = do
    f1 <- (#peek Clay__LayoutElementTreeNode, layoutElement) ptr
    f2 <- (#peek Clay__LayoutElementTreeNode, position) ptr
    f3 <- (#peek Clay__LayoutElementTreeNode, nextChildOffset) ptr
    pure $ ClayLayoutElementTreeNode f1 f2 f3
  
  poke ptr (ClayLayoutElementTreeNode f1 f2 f3) = do
    (#poke Clay__LayoutElementTreeNode, layoutElement) ptr f1
    (#poke Clay__LayoutElementTreeNode, position) ptr f2
    (#poke Clay__LayoutElementTreeNode, nextChildOffset) ptr f3

data ClayLayoutElementTreeRoot = ClayLayoutElementTreeRoot {
  clayLayoutElementTreeRootLayoutElementIndex :: CInt,
  clayLayoutElementTreeRootParentId :: CUInt,
  clayLayoutElementTreeRootClipElementId :: CUInt,
  clayLayoutElementTreeRootZIndex :: CShort,
  clayLayoutElementTreeRootPointerOffset :: ClayVector2
}

instance Storable ClayLayoutElementTreeRoot where
  sizeOf _ = (#size Clay__LayoutElementTreeRoot)
  alignment _ = (#alignment Clay__LayoutElementTreeRoot)
  peek ptr = do
    f1 <- (#peek Clay__LayoutElementTreeRoot, layoutElementIndex) ptr
    f2 <- (#peek Clay__LayoutElementTreeRoot, parentId) ptr
    f3 <- (#peek Clay__LayoutElementTreeRoot, clipElementId) ptr
    f4 <- (#peek Clay__LayoutElementTreeRoot, zIndex) ptr
    f5 <- (#peek Clay__LayoutElementTreeRoot, pointerOffset) ptr
    pure $ ClayLayoutElementTreeRoot f1 f2 f3 f4 f5
  
  poke ptr (ClayLayoutElementTreeRoot f1 f2 f3 f4 f5) = do
    (#poke Clay__LayoutElementTreeRoot, layoutElementIndex) ptr f1
    (#poke Clay__LayoutElementTreeRoot, parentId) ptr f2
    (#poke Clay__LayoutElementTreeRoot, clipElementId) ptr f3
    (#poke Clay__LayoutElementTreeRoot, zIndex) ptr f4
    (#poke Clay__LayoutElementTreeRoot, pointerOffset) ptr f5

data ClayContext = ClayContext {
  clayContextMaxElementCount :: CInt,
  clayContextMaxMeasureTextCacheWordCount :: CInt,
  clayContextWarningsEnabled :: CBool,
  clayContextErrorHandler :: ClayErrorHandler,
  clayContextBooleanWarnings :: ClayBooleanWarnings,
  clayContextWarnings :: ClayArray ClayWarning,
  clayContextPointerInfo :: ClayPointerData,
  clayContextLayoutDimensions :: ClayDimensions,
  clayContextDynamicElementIndexBaseHash :: ClayElementId,
  clayContextDynamicElementIndex :: CUInt,
  clayContextDebugModeEnabled :: CBool,
  clayContextDisableCulling :: CBool,
  clayContextExternalScrollHandlingEnabled :: CBool,
  clayContextDebugSelectedElementId :: CUInt,
  clayContextGeneration :: CUInt,
  clayContextArenaResetOffset :: CUIntPtr,
  clayContextMeasureTextUserData :: Ptr (),
  clayContextQueryScrollOffsetUserData :: Ptr (),
  clayContextInternalArena :: ClayArena,
  -- Layout Elements / Render Commands
  clayContextLayoutElements :: ClayArray ClayLayoutElement,
  clayContextRenderCommands :: ClayArray ClayRenderCommand,
  clayContextOpenLayoutElementStack :: ClayArray CInt,
  clayContextLayoutElementChildren :: ClayArray CInt,
  clayContextLayoutElementChildrenBuffer :: ClayArray CInt,
  clayContextTextElementDataArray :: ClayArray ClayTextElementData,
  clayContextImageElementPointers :: ClayArray CInt,
  clayContextReusableElementIndexBuffer :: ClayArray CInt,
  clayContextLayoutElementClipElementIds :: ClayArray CInt,
  -- Configs
  clayContextLayoutConfigs :: ClayArray ClayLayoutConfig,
  clayContextElementConfigs :: ClayArray ClayElementConfig,
  clayContextTextElementConfigs :: ClayArray ClayTextElementConfig,
  clayContextImageElementConfigs :: ClayArray ClayImageElementConfig,
  clayContextFloatingElementConfigs :: ClayArray ClayFloatingElementConfig,
  clayContextScrollElementConfigs :: ClayArray ClayScrollElementConfig,
  clayContextCustomElementConfigs :: ClayArray ClayCustomElementConfig,
  clayContextBorderElementConfigs :: ClayArray ClayBorderElementConfig,
  clayContextSharedElementConfigs :: ClayArray ClaySharedElementConfig,
  -- Misc Data Structures
  clayContextLayoutElementIdStrings :: ClayArray ClayString,
  clayContextWrappedTextLines :: ClayArray ClayWrappedTextLine,
  clayContextLayoutElementTreeNodeArray1 :: ClayArray ClayLayoutElementTreeNode,
  clayContextLayoutElementTreeRoots :: ClayArray ClayLayoutElementTreeRoot,
  clayContextLayoutElementsHashMapInternal :: ClayArray ClayLayoutElementHashMapItem,
  clayContextLayoutElementsHashMap :: ClayArray CInt,
  clayContextMeasureTextHashMapInternal :: ClayArray ClayMeasureTextCacheItem,
  clayContextMeasureTextHashMapInternalFreeList :: ClayArray CInt,
  clayContextMeasaureTextHashMap :: ClayArray CInt,
  clayContextMeasuredWords :: ClayArray ClayMeasuredWord,
  clayContextMeasureWordsFreeList :: ClayArray CInt,
  clayContextOpenClipElementStack :: ClayArray CInt,
  clayContextPointerOverIds :: ClayArray ClayElementId,
  clayContextScrollContainerDatas :: ClayArray ClayScrollContainerDataInternal,
  clayContextTreeNodeVisited :: ClayArray CBool,
  clayContextDynamicStringData :: ClayArray CChar,
  clayContextDebugElementData :: ClayArray ClayDebugElementData
}

instance Storable ClayContext where
  sizeOf _ = (#size Clay_Context)
  alignment _ = (#alignment Clay_Context)
  peek ptr = do
    f1 <- (#peek Clay_Context, maxElementCount) ptr
    f2 <- (#peek Clay_Context, maxMeasureTextCacheWordCount) ptr
    f3 <- (#peek Clay_Context, warningsEnabled) ptr
    f4 <- (#peek Clay_Context, errorHandler) ptr
    f5 <- (#peek Clay_Context, booleanWarnings) ptr
    f6 <- (#peek Clay_Context, warnings) ptr
    f7 <- (#peek Clay_Context, pointerInfo) ptr
    f8 <- (#peek Clay_Context, layoutDimensions) ptr
    f9 <- (#peek Clay_Context, dynamicElementIndexBaseHash) ptr
    f10 <- (#peek Clay_Context, dynamicElementIndex) ptr
    f11 <- (#peek Clay_Context, debugModeEnabled) ptr
    f12 <- (#peek Clay_Context, disableCulling) ptr
    f13 <- (#peek Clay_Context, externalScrollHandlingEnabled) ptr
    f14 <- (#peek Clay_Context, debugSelectedElementId) ptr
    f15 <- (#peek Clay_Context, generation) ptr
    f16 <- (#peek Clay_Context, arenaResetOffset) ptr
    f17 <- (#peek Clay_Context, measureTextUserData) ptr
    f18 <- (#peek Clay_Context, queryScrollOffsetUserData) ptr
    f19 <- (#peek Clay_Context, internalArena) ptr
    f20 <- (#peek Clay_Context, layoutElements) ptr
    f21 <- (#peek Clay_Context, renderCommands) ptr
    f22 <- (#peek Clay_Context, openLayoutElementStack) ptr
    f23 <- (#peek Clay_Context, layoutElementChildren) ptr
    f24 <- (#peek Clay_Context, layoutElementChildrenBuffer) ptr
    f25 <- (#peek Clay_Context, textElementData) ptr
    f26 <- (#peek Clay_Context, imageElementPointers) ptr
    f27 <- (#peek Clay_Context, reusableElementIndexBuffer) ptr
    f28 <- (#peek Clay_Context, layoutElementClipElementIds) ptr
    f29 <- (#peek Clay_Context, layoutConfigs) ptr
    f30 <- (#peek Clay_Context, elementConfigs) ptr
    f31 <- (#peek Clay_Context, textElementConfigs) ptr
    f32 <- (#peek Clay_Context, imageElementConfigs) ptr
    f33 <- (#peek Clay_Context, floatingElementConfigs) ptr
    f34 <- (#peek Clay_Context, scrollElementConfigs) ptr
    f35 <- (#peek Clay_Context, customElementConfigs) ptr
    f36 <- (#peek Clay_Context, borderElementConfigs) ptr
    f37 <- (#peek Clay_Context, sharedElementConfigs) ptr
    f38 <- (#peek Clay_Context, layoutElementIdStrings) ptr
    f39 <- (#peek Clay_Context, wrappedTextLines) ptr
    f40 <- (#peek Clay_Context, layoutElementTreeNodeArray1) ptr
    f41 <- (#peek Clay_Context, layoutElementTreeRoots) ptr
    f42 <- (#peek Clay_Context, layoutElementsHashMapInternal) ptr
    f43 <- (#peek Clay_Context, layoutElementsHashMap) ptr
    f44 <- (#peek Clay_Context, measureTextHashMapInternal) ptr
    f45 <- (#peek Clay_Context, measureTextHashMapInternalFreeList) ptr
    f46 <- (#peek Clay_Context, measureTextHashMap) ptr
    f47 <- (#peek Clay_Context, measuredWords) ptr
    f48 <- (#peek Clay_Context, measuredWordsFreeList) ptr
    f49 <- (#peek Clay_Context, openClipElementStack) ptr
    f50 <- (#peek Clay_Context, pointerOverIds) ptr
    f51 <- (#peek Clay_Context, scrollContainerDatas) ptr
    f52 <- (#peek Clay_Context, treeNodeVisited) ptr
    f53 <- (#peek Clay_Context, dynamicStringData) ptr
    f54 <- (#peek Clay_Context, debugElementData) ptr
    pure $ ClayContext f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 f33 f34 f35 f36 f37 f38 f39 f40 f41 f42 f43 f44 f45 f46 f47 f48 f49 f50 f51 f52 f53 f54
  
  poke ptr (ClayContext f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 f33 f34 f35 f36 f37 f38 f39 f40 f41 f42 f43 f44 f45 f46 f47 f48 f49 f50 f51 f52 f53 f54) = do
    (#poke Clay_Context, maxElementCount) ptr f1
    (#poke Clay_Context, maxMeasureTextCacheWordCount) ptr f2
    (#poke Clay_Context, warningsEnabled) ptr f3
    (#poke Clay_Context, errorHandler) ptr f4
    (#poke Clay_Context, booleanWarnings) ptr f5
    (#poke Clay_Context, warnings) ptr f6
    (#poke Clay_Context, pointerInfo) ptr f7
    (#poke Clay_Context, layoutDimensions) ptr f8
    (#poke Clay_Context, dynamicElementIndexBaseHash) ptr f9
    (#poke Clay_Context, dynamicElementIndex) ptr f10
    (#poke Clay_Context, debugModeEnabled) ptr f11
    (#poke Clay_Context, disableCulling) ptr f12
    (#poke Clay_Context, externalScrollHandlingEnabled) ptr f13
    (#poke Clay_Context, debugSelectedElementId) ptr f14
    (#poke Clay_Context, generation) ptr f15
    (#poke Clay_Context, arenaResetOffset) ptr f16
    (#poke Clay_Context, measureTextUserData) ptr f17
    (#poke Clay_Context, queryScrollOffsetUserData) ptr f18
    (#poke Clay_Context, internalArena) ptr f19
    (#poke Clay_Context, layoutElements) ptr f20
    (#poke Clay_Context, renderCommands) ptr f21
    (#poke Clay_Context, openLayoutElementStack) ptr f22
    (#poke Clay_Context, layoutElementChildren) ptr f23
    (#poke Clay_Context, layoutElementChildrenBuffer) ptr f24
    (#poke Clay_Context, textElementData) ptr f25
    (#poke Clay_Context, imageElementPointers) ptr f26
    (#poke Clay_Context, reusableElementIndexBuffer) ptr f27
    (#poke Clay_Context, layoutElementClipElementIds) ptr f28
    (#poke Clay_Context, layoutConfigs) ptr f29
    (#poke Clay_Context, elementConfigs) ptr f30
    (#poke Clay_Context, textElementConfigs) ptr f31
    (#poke Clay_Context, imageElementConfigs) ptr f32
    (#poke Clay_Context, floatingElementConfigs) ptr f33
    (#poke Clay_Context, scrollElementConfigs) ptr f34
    (#poke Clay_Context, customElementConfigs) ptr f35
    (#poke Clay_Context, borderElementConfigs) ptr f36
    (#poke Clay_Context, sharedElementConfigs) ptr f37
    (#poke Clay_Context, layoutElementIdStrings) ptr f38
    (#poke Clay_Context, wrappedTextLines) ptr f39
    (#poke Clay_Context, layoutElementTreeNodeArray1) ptr f40
    (#poke Clay_Context, layoutElementTreeRoots) ptr f41
    (#poke Clay_Context, layoutElementsHashMapInternal) ptr f42
    (#poke Clay_Context, layoutElementsHashMap) ptr f43
    (#poke Clay_Context, measureTextHashMapInternal) ptr f44
    (#poke Clay_Context, measureTextHashMapInternalFreeList) ptr f45
    (#poke Clay_Context, measureTextHashMap) ptr f46
    (#poke Clay_Context, measuredWords) ptr f47
    (#poke Clay_Context, measuredWordsFreeList) ptr f48
    (#poke Clay_Context, openClipElementStack) ptr f49
    (#poke Clay_Context, pointerOverIds) ptr f50
    (#poke Clay_Context, scrollContainerDatas) ptr f51
    (#poke Clay_Context, treeNodeVisited) ptr f52
    (#poke Clay_Context, dynamicStringData) ptr f53
    (#poke Clay_Context, debugElementData) ptr f54
