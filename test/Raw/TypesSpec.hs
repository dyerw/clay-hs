module Raw.TypesSpec where


import Clay.Raw
import Clay.Raw.Types
import Foreign
import Test.Hspec

spec :: Spec
spec = do
  describe "ClayColor" $ do
    it "Storable preserves equality" $ do
        let blue = ClayColor 0 0 255 255
        ptr <- malloc
        poke ptr blue
        peekedBlue <- peek ptr :: IO ClayColor
        peekedBlue `shouldBe` blue

  describe "ClayRectangleRenderData" $ do
    it "Storable preserves equality" $ do
        let rectangle = ClayRectangleRenderData {
            clayRectangleRenderDataBackgroundColor = ClayColor 50 100 255 255,
            clayRectangleRenderDataCornerRadius = ClayCornerRadius 5 10 15 20
        }
        ptr <- malloc
        poke ptr rectangle 
        peekedRect <- peek ptr :: IO ClayRectangleRenderData 
        peekedRect `shouldBe` rectangle

  describe "ClayRenderCommand" $ do
    it "Storable preserves equality" $ do
        let renderData = ClayRenderDataRectangle $ ClayRectangleRenderData {
            clayRectangleRenderDataBackgroundColor = ClayColor 50 100 255 255,
            clayRectangleRenderDataCornerRadius = ClayCornerRadius 5 10 15 20
        }
        let command = ClayRenderCommand {
            clayRenderCommandBoundingBox = ClayBoundingBox 0 0 250 250,
            clayRenderCommandRenderData = renderData,
            clayRenderCommandUserData = nullPtr,
            clayRenderCommandId = 123,
            clayRenderCommandZIndex = 1,
            clayRenderCommandCommandType = clayRenderCommandTypeRectangle
        }
        ptr <- malloc
        poke ptr command
        peekedCommand <- peek ptr :: IO ClayRenderCommand
        peekedCommand `shouldBe` command

  describe "ClayElementDeclaration" $ do
    it "reads and writes the optional fields via its Storable instance" $ do
        let sizing = ClaySizing {
            claySizingWidth = ClaySizingAxis (Right 0.5) claySizingTypePercent,
            claySizingHeight = ClaySizingAxis (Right 0.5) claySizingTypePercent
        }

        let layoutConfig = ClayLayoutConfig {
            clayLayoutConfigSizing = sizing,
            clayLayoutConfigPadding = ClayPadding 0 0 0 0,
            clayLayoutConfigChildGap = 0,
            clayLayoutConfigChildAlignment = ClayChildAlignment clayAlignXCenter clayAlignYCenter,
            clayLayoutConfigLayoutDirection = clayLeftToRight
        }

        elementIdStr <- toClayString "some-element-id"
        elementId <- clayHashString elementIdStr 0 0
        let elementDeclaration = ClayElementDeclaration {
                clayElementDeclarationId = elementId,
                clayElementDeclarationLayout = layoutConfig,
                clayElementDeclarationBackgroundColor = Just $ ClayColor 0 0 255 255,
                clayElementDeclarationCornerRadius = Nothing,
                clayElementDeclarationImage = Nothing,
                clayElementDeclarationFloating = Nothing,
                clayElementDeclarationCustom = Nothing,
                clayElementDeclarationScroll = Nothing,
                clayElementDeclarationBorder = Nothing,
                clayElementDeclarationUserData = nullPtr
        }
        ptr <- malloc
        poke ptr elementDeclaration
        elementDeclaration' <- peek ptr

        -- Something in -> something out
        clayElementDeclarationBackgroundColor elementDeclaration' `shouldBe` clayElementDeclarationBackgroundColor elementDeclaration
        -- Nothing in -> nothing out
        clayElementDeclarationCornerRadius elementDeclaration' `shouldBe` clayElementDeclarationCornerRadius elementDeclaration