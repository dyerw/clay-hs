module RawSpec where

import Clay.Raw
import Clay.Raw.Types
import Foreign
import Foreign.C
import Test.Hspec

spec :: Spec
spec = do
  describe "clayMinMemorySize" $ do
    it "returns the default min memory size" $ do
      minMemorySize <- clayMinMemorySize
      minMemorySize `shouldBe` (6051264 :: CInt)

  describe "clayCreateArenaWithCapacityAndMemory" $ do
    it "creates an arena with capacity and memory" $ do
      ptr <- malloc
      arena <- clayCreateArenaWithCapacityAndMemory (512 :: CSize) ptr
      clayArenaCapacity arena `shouldBe` 512
      clayArenaMemory arena `shouldBe` castPtr ptr
      free ptr

  describe "clayInitialize" $ do
    it "returns a context that reflects passed data" $ do
        minMemorySize <- fromIntegral <$> clayMinMemorySize
        arenaMem <- mallocBytes minMemorySize
        arena <- clayCreateArenaWithCapacityAndMemory 
            (fromIntegral minMemorySize) arenaMem
        let dims = ClayDimensions 500 500
        context <- peek <$> clayInitialize arena dims undefined
        1 `shouldBe` 12
