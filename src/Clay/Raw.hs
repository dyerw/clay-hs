{-# LANGUAGE CApiFFI #-}

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

clayInitialize :: ClayArena -> ClayDimensions -> ClayHelperErrorHandlerWrapper -> IO (Ptr ClayContext)
clayInitialize arena dims errorHandler = alloca $ \arenaPtr -> alloca $ \dimsPtr -> alloca $ \errorHandlerPtr -> do
  poke arenaPtr arena
  poke dimsPtr dims
  poke errorHandlerPtr errorHandler
  clayInitializeHelper arenaPtr dimsPtr errorHandlerPtr

-- foreign import capi "clay.h Clay_GetCurrentContext"
--   clayGetCurrentContext :: IO (Ptr ClayContext)

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

-- foreign import capi "clay.h Clay_BeginLayout"
--   clayBeginLayout :: IO ()

-- foreign import capi "clayhelper.h ClayHelper_EndLayout"
--   clayEndLayoutHelper :: IO (Ptr ClayRenderCommandArray)

-- foreign import capi "clayhelper.h ClayHelper_GetElementId"
--   clayGetElementDataHelper :: Ptr ClayString -> IO (Ptr ClayElementId)

-- foreign import capi "clayhelper.h ClayHelper_GetElementIdWithIndex"
--   clayGetElementIdWithIndex :: Ptr ClayString -> CInt -> Ptr ClayElementId