module Clay.LayoutSpec where

import Clay.Layout
import Test.Hspec

spec :: Spec
spec = do
  describe "Style" $ do
    it "getStyleValue returns non-hovered value when context not hovered" $ do
      let style = Style (StyleValue False) (StyleValue True)
      let sv = getStyleValue style (ElementContext False) id
      sv `shouldBe` StyleValue True

    it "getStyleValue returns hovered value when context is hovered" $ do
      let style = Style (StyleValue False) (StyleValue True)
      let sv = getStyleValue style (ElementContext True) id
      sv `shouldBe` StyleValue False

    it "returns base value when hovered and no hovered value set" $ do
      let style = growX

      let xAxisSizing :: StyleValue Sizing
          xAxisSizing = getSizing style (ElementContext True) XAxis

      xAxisSizing `shouldBe` StyleValue Grow

    it "returns hovered value when hovered and hovered value set" $ do
      let style = hovered growX
          xAxisSizingHovered = getSizing style (ElementContext True) XAxis

      xAxisSizingHovered `shouldBe` StyleValue Grow

    it "returns default value when not hovered and hovered value set" $ do
      let style = hovered growX
          xAxisSizingNotHovered = getSizing style (ElementContext False) XAxis

      xAxisSizingNotHovered `shouldBe` Default

    it "returns correct value when not hovered and hovered and not hovered value set" $ do
      let style = hovered growX <> fitX
          xAxisSizingNotHovered = getSizing style (ElementContext False) XAxis

      xAxisSizingNotHovered `shouldBe` StyleValue Fit

    it "returns correct value when not hovered and hovered and not hovered value set" $ do
      let style = hovered growX <> fitX
          xAxisSizingNotHovered = getSizing style (ElementContext True) XAxis

      xAxisSizingNotHovered `shouldBe` StyleValue Grow

    it "latter fields override previous" $ do
      let style = fitX <> growX

      let xAxisSizing :: StyleValue Sizing
          xAxisSizing = getSizing style (ElementContext False) XAxis

      xAxisSizing `shouldBe` StyleValue Grow

    it "different fields are preserved" $ do
      let style = fitX <> growY
      let yAxisSizing :: StyleValue Sizing
          yAxisSizing = getSizing style (ElementContext False) YAxis
      let xAxisSizing :: StyleValue Sizing
          xAxisSizing = getSizing style (ElementContext False) XAxis

      xAxisSizing `shouldBe` StyleValue Fit
      yAxisSizing `shouldBe` StyleValue Grow
