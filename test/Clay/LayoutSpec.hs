module Clay.LayoutSpec where

import Clay.Layout
import Test.Hspec

spec :: Spec
spec = do
  describe "Style" $ do
    it "latter fields override previous" $ do
      let style = fitX <> growX

      let xAxisSizing :: Maybe Sizing
          xAxisSizing = getUI (sizing style XAxis) UIContext{isHovered = False}

      xAxisSizing `shouldBe` Just Grow
    it "different fields are preserved" $ do
      let style = fitX <> growY
      let yAxisSizing :: Maybe Sizing
          yAxisSizing = getUI (sizing style YAxis) UIContext{isHovered = False}
      let xAxisSizing :: Maybe Sizing
          xAxisSizing = getUI (sizing style XAxis) UIContext{isHovered = False}

      xAxisSizing `shouldBe` Just Fit
      yAxisSizing `shouldBe` Just Grow
