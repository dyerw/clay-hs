{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Clay.Raw
-- Description : Direct bindings to Clay functions
-- License     : MIT
-- Maintainer  : liamd94@gmail.com
--
-- Direct ports of all functions in @clay.h@. Where the original function
-- took or received structs by value it is instead bound to a helper function
-- that passes the struct by pointer. Functions peeking those values into managed
-- memory are exposed so calling code doesn't have to fuss with pointers.
module Clay.Raw
  ( clayMinMemorySize,
    clayCreateArenaWithCapacityAndMemory,
    clayInitialize,
    claySetPointerState,
    clayGetCurrentContext,
    clayBeginLayout,
    clayEndLayout,
    clayOpenElement,
    clayConfigureOpenElement,
    clayCloseElement,
    clayHashString,
    toClayString,
    fromClayString,
    arrayToList,
  )
where

import Clay.Raw.Types
import Control.Exception (bracket)
import Foreign
import Foreign.C

foreign import capi "clay.h Clay_MinMemorySize" clayMinMemorySize :: IO CInt

foreign import capi "clayhelper.h ClayHelper_CreateArenaWithCapacityAndMemory"
  clayCreateArenaWithCapacityAndMemoryHelper ::
    CSize ->
    Ptr () ->
    IO (Ptr ClayArena)

clayCreateArenaWithCapacityAndMemory :: CSize -> Ptr () -> IO ClayArena
clayCreateArenaWithCapacityAndMemory capacity memory =
  bracket
    (clayCreateArenaWithCapacityAndMemoryHelper capacity memory)
    free
    peek

foreign import capi "clayhelper.h ClayHelper_SetPointerState"
  claySetPointerStateHelper :: Ptr ClayVector2 -> CBool -> IO ()

claySetPointerState :: ClayVector2 -> CBool -> IO ()
claySetPointerState position pointerDown = alloca $ \ptr -> do
  poke ptr position
  claySetPointerStateHelper ptr pointerDown

foreign import capi "clayhelper.h ClayHelper_Initialize"
  clayInitializeHelper ::
    Ptr ClayArena ->
    Ptr ClayDimensions ->
    Ptr ClayHelperErrorHandlerWrapper ->
    IO (Ptr ClayContext)

foreign import ccall "wrapper"
  mkClayErrorHandlerFunctionPtr :: ClayErrorHandlerFunctionPtr -> IO (FunPtr ClayErrorHandlerFunctionPtr)

wrapErrorHandler :: ClayErrorHandlerFunction -> ClayErrorHandlerFunctionPtr
wrapErrorHandler f ptr = do
  err <- peek ptr
  f err

clayInitialize :: ClayArena -> ClayDimensions -> ClayErrorHandlerFunction -> IO (Ptr ClayContext)
clayInitialize arena dims errorHandler = alloca $ \arenaPtr -> alloca $ \dimsPtr -> alloca $ \errorHandlerPtr -> do
  fnPtr <- mkClayErrorHandlerFunctionPtr (wrapErrorHandler errorHandler)
  poke arenaPtr arena
  poke dimsPtr dims
  poke errorHandlerPtr $ ClayHelperErrorHandlerWrapper fnPtr
  clayInitializeHelper arenaPtr dimsPtr errorHandlerPtr

foreign import capi "clay.h Clay_GetCurrentContext"
  clayGetCurrentContext :: IO (Ptr ClayContext)

-- foreign import capi "clay.h Clay_SetCurrentContext"
--   claySetCurrentContext :: Ptr ClayContext -> IO ()

-- foreign import capi "clayhelper.h ClayHelper_UpdateScrollContainers"
--   clayUpdateScrollContainersHelper ::
--     CBool ->
--     Ptr ClayVector2 ->
--     CFloat ->
--     IO ()

-- clayUpdateScrollContainers :: CBool -> ClayVector2 -> CFloat -> IO ()
-- clayUpdateScrollContainers b v f = alloca $ \ptr -> do
--   poke ptr v
--   clayUpdateScrollContainersHelper b ptr f

-- foreign import capi "clayhelper.h ClayHelper_SetLayoutDimensions"
--   claySetLayoutDimensionsHelper :: Ptr ClayDimensions -> IO ()

-- claySetLayoutDimensions :: ClayDimensions -> IO ()
-- claySetLayoutDimensions dims = alloca $ \ptr -> do
--   poke ptr dims
--   claySetLayoutDimensionsHelper ptr

foreign import capi "clay.h Clay_BeginLayout"
  clayBeginLayout :: IO ()

foreign import capi "clay.h Clay__OpenElement"
  clayOpenElement :: IO ()

foreign import capi "clay.h Clay__ConfigureOpenElementPtr"
  clayConfigureOpenElementPtr :: Ptr ClayElementDeclaration -> IO ()

clayConfigureOpenElement :: ClayElementDeclaration -> IO ()
clayConfigureOpenElement decl = alloca $ \ptr -> do
  poke ptr decl
  clayConfigureOpenElementPtr ptr

foreign import capi "clay.h Clay__CloseElement"
  clayCloseElement :: IO ()

foreign import capi "clayhelper.h ClayHelper_HashString"
  clayHashStringHelper :: Ptr ClayString -> CInt -> CInt -> IO (Ptr ClayElementId)

-- seed is parent id
clayHashString :: ClayString -> CInt -> CInt -> IO ClayElementId
clayHashString label offset seed = alloca $ \labelPtr -> do
  poke labelPtr label
  bracket
    (clayHashStringHelper labelPtr offset seed)
    free
    peek

foreign import capi "clayhelper.h ClayHelper_EndLayout"
  clayEndLayoutHelper :: IO (Ptr (ClayArray ClayRenderCommand))

clayEndLayout :: IO (ClayArray ClayRenderCommand)
clayEndLayout = bracket clayEndLayoutHelper free peek

-- foreign import capi "clayhelper.h ClayHelper_GetElementId"
--   clayGetElementDataHelper :: Ptr ClayString -> IO (Ptr ClayElementId)

-- foreign import capi "clayhelper.h ClayHelper_GetElementIdWithIndex"
--   clayGetElementIdWithIndex :: Ptr ClayString -> CInt -> Ptr ClayElementId

-- * Clay Strings

toClayString :: String -> IO ClayString
toClayString s = do
  (ptr, len) <- newCStringLen s
  pure $ ClayString (fromIntegral len) ptr

fromClayString :: ClayString -> IO String
fromClayString (ClayString len ptr) = peekCStringLen (ptr, fromIntegral len)

-- * Clay Arrays

arrayToList :: forall a. (Storable a) => ClayArray a -> IO [a]
arrayToList arr = do
  let size = sizeOf (undefined :: a)
  let ptr = clayArrayInternalArray arr
  let len = clayArrayLength arr
  traverse (\i -> peek (plusPtr ptr (i * size))) [0 .. (fromIntegral len - 1)]