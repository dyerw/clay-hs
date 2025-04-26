{-# LANGUAGE NamedFieldPuns #-}

module Clay.PointerProxyMap where

import qualified Data.Map as M
import Foreign

-- Clay wants you to pass pointers to custom structures and then return the pointers in render
-- commands. However, since we're living in managed memory land we don't need to do that and we can
-- basically just keep integer references to arbitrary data and instead of reading out of memory we
-- just look it up in a map. This is just a helper to let us interface with the Clay API in this
-- manner. I suppose it's a sort of pseudo-managed-memory structure in Haskell's managed memory.
data PointerProxyMap a = PointerProxyMap {ptrMap :: M.Map WordPtr a, nextPtr :: WordPtr}

init :: PointerProxyMap a
init = PointerProxyMap mempty 0

insert :: PointerProxyMap a -> a -> (PointerProxyMap a, Ptr ())
insert PointerProxyMap {ptrMap, nextPtr} a =
  let nextMap = M.insert nextPtr a ptrMap
   in (PointerProxyMap nextMap nextPtr, wordPtrToPtr (nextPtr + 1))

lookup :: PointerProxyMap a -> Ptr () -> Maybe a
lookup PointerProxyMap {ptrMap} ptr = M.lookup (ptrToWordPtr ptr) ptrMap
