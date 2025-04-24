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
      ctx <- initClay
      clayContextMaxElementCount ctx `shouldBe` 8192
      clayContextLayoutDimensions ctx `shouldBe` defaultDimensions

  describe "claySetPointerState" $ do
    it "the context reflects the changed pointer state" $ do
      _ <- initClay
      let pos = ClayVector2 200 200
      claySetPointerState pos 1
      ctx <- clayGetCurrentContext >>= peek
      let pointerData = clayContextPointerInfo ctx
      clayPointerDataPosition pointerData `shouldBe` pos

  describe "clayGetCurrentContext" $ do
    it "returns a context that reflects intialized data" $ do
      _ <- initClay
      ctx <- clayGetCurrentContext >>= peek
      clayContextLayoutDimensions ctx `shouldBe` defaultDimensions

  -- describe "ClayArray" $ do
  --   it "converts an array to a list"

  describe "Layouts" $ do
    it "returns render commands for a blue box" $ do
      _ <- initClay
      clayBeginLayout
      clayOpenElement

      let sizing =
            ClaySizing
              { claySizingWidth = Just $ ClaySizingAxis (Right 0.5) claySizingTypePercent,
                claySizingHeight = Just $ ClaySizingAxis (Right 0.5) claySizingTypePercent
              }

      let layoutConfig =
            ClayLayoutConfig
              { clayLayoutConfigSizing = sizing,
                clayLayoutConfigPadding = ClayPadding 0 0 0 0,
                clayLayoutConfigChildGap = 0,
                clayLayoutConfigChildAlignment = ClayChildAlignment clayAlignXCenter clayAlignYCenter,
                clayLayoutConfigLayoutDirection = clayLeftToRight
              }

      elementIdStr <- toClayString "some-element-id"
      elementId <- clayHashString elementIdStr 0 0

      let elementDeclaration =
            ClayElementDeclaration
              { clayElementDeclarationId = Just elementId,
                clayElementDeclarationLayout = Just layoutConfig,
                clayElementDeclarationBackgroundColor = Just $ ClayColor 0 0 255 255,
                clayElementDeclarationCornerRadius = Just $ ClayCornerRadius 10 10 10 10,
                clayElementDeclarationImage = Nothing,
                clayElementDeclarationFloating = Nothing,
                clayElementDeclarationCustom = Nothing,
                clayElementDeclarationScroll = Nothing,
                clayElementDeclarationBorder = Nothing,
                clayElementDeclarationUserData = nullPtr
              }

      clayConfigureOpenElement elementDeclaration
      clayCloseElement

      renderCommands <- clayEndLayout

      renderCommandsList <- arrayToList renderCommands

      length renderCommandsList `shouldBe` 1

      let command = head renderCommandsList
      clayRenderCommandBoundingBox command `shouldBe` ClayBoundingBox 0 0 250 250
      let renderData = clayRenderCommandRenderData command
      case renderData of
        ClayRenderDataRectangle rectangleData -> do
          clayRectangleRenderDataBackgroundColor rectangleData `shouldBe` ClayColor 0 0 255 255
          clayRectangleRenderDataCornerRadius rectangleData `shouldBe` ClayCornerRadius 10 10 10 10
        _ -> error "renderData is not a rectangle"
  where
    defaultDimensions :: ClayDimensions
    defaultDimensions = ClayDimensions 500 500

    initClay :: IO ClayContext
    initClay = do
      minMemorySize <- fromIntegral <$> clayMinMemorySize
      arenaMem <- mallocBytes minMemorySize
      arena <-
        clayCreateArenaWithCapacityAndMemory
          (fromIntegral minMemorySize)
          arenaMem
      let handleError = print
      ctxPtr <- clayInitialize arena defaultDimensions handleError
      peek ctxPtr
