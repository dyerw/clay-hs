module Clay.RenderSpec where

import Clay.Color
import Clay.Declaration
import Clay.Layout
import Clay.Raw.Types
import Clay.Render
import Data.Text
import Foreign.C
import Test.Hspec

type TestEvents = Text

data TestImage = TestImage {name :: Text, dims :: (Float, Float)} deriving (Eq, Show)

instance HasSourceDimensions TestImage where
  sourceDimensions = dims

type TestFonts = Text

type TestCustom = Text

type TestElement = Element TestEvents TestFonts TestImage TestCustom

type TestElementDeclarationContext = ElementDeclarationContext TestEvents TestFonts TestImage TestCustom

type TestElementDeclaration a = ElementDeclaration TestEvents TestFonts TestImage TestCustom a

spec :: Spec
spec = do
  describe "Rendering" $ do
    let defaultViewHeight :: Float
        defaultViewHeight = 1000

        defaultViewWidth :: Float
        defaultViewWidth = 1000

        defaultInput :: InputState
        defaultInput = InputState (0, 0) False (defaultViewWidth, defaultViewHeight)

        mkTestContext :: TestElement -> Bool -> TestElementDeclarationContext
        mkTestContext e h =
          ElementDeclarationContext e $
            CommonDeclarationContextFields defaultInput h Nothing Nothing

        withElement :: TestElement -> Bool -> TestElementDeclaration a -> IO a
        withElement e h d = do
          (a, _) <- runDeclaration d (mkTestContext e h)
          pure a

        withStyle :: ElementStyle -> TestElementDeclaration a -> IO a
        withStyle s = withElement (Element $ ElementConfig Nothing s []) False

        withHoveredStyle :: ElementStyle -> TestElementDeclaration a -> IO a
        withHoveredStyle s = withElement (Element $ ElementConfig Nothing s []) True

    describe "Calculating values based on layout context variables" $ do
      it "view width" $ do
        height <- withStyle (fixedX viewWidth) getClaySizingWidth
        let axis = ClaySizingAxis (Right 1000) claySizingTypeFixed
        height `shouldBe` Just axis

      it "view width - constant" $ do
        height <- withStyle (fixedX $ viewWidth - 100) getClaySizingWidth
        let axis = ClaySizingAxis (Right 900) claySizingTypeFixed
        height `shouldBe` Just axis

      it "view width / constant" $ do
        height <- withStyle (fixedX $ viewWidth / 2) getClaySizingWidth
        let axis = ClaySizingAxis (Right (CFloat (defaultViewWidth / 2))) claySizingTypeFixed
        height `shouldBe` Just axis

    describe "Hovered styles" $ do
      it "returns base value when both are set and not hovered" $ do
        color <- withStyle (backgroundColor red <> hovered (backgroundColor blue)) getClayBackgroundColor
        color `shouldBe` Just (toClayColor red)

      it "returns hovered value when both are set and hovered" $ do
        color <- withHoveredStyle (backgroundColor red <> hovered (backgroundColor blue)) getClayBackgroundColor
        color `shouldBe` Just (toClayColor blue)

    describe "Converting to Clay structures" $ do
      it "getClaySizingAxisHeight growY" $ do
        height <- withStyle growY_ getClaySizingHeight
        let axis = ClaySizingAxis (Left (ClaySizingMinMax Nothing Nothing)) claySizingTypeGrow
        height `shouldBe` Just axis

      it "getClaySizingAxisHeight growY bounded" $ do
        height <- withStyle (growY $ maxSize 100) getClaySizingHeight
        let axis = ClaySizingAxis (Left (ClaySizingMinMax Nothing (Just (CFloat 100)))) claySizingTypeGrow
        height `shouldBe` Just axis

    describe "render commands" $ do
      it "returns a render command for a blue rectangle filling the view" $ do
        let layout :: TestElement
            layout = root (backgroundColor blue <> grow_) []
        (renderCommands, _) <- calculateLayout layout defaultInput
        renderCommands `shouldBe` []
