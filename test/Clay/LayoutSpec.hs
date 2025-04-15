module Clay.LayoutSpec where

import Clay.Layout
import Test.Hspec

spec :: Spec
spec = do
  describe "Style" $ do
    it "getStyleValue returns non-hovered value when context not hovered" $ do
      let style = Style (StyleValue False) (StyleValue True)
      let sv = getStyleValue style (ElementContext False (0, 0)) id
      sv `shouldBe` StyleValue True

    it "getStyleValue returns hovered value when context is hovered" $ do
      let style = Style (StyleValue False) (StyleValue True)
      let sv = getStyleValue style (ElementContext True (0, 0)) id
      sv `shouldBe` StyleValue False

    it "returns base value when hovered and no hovered value set" $ do
      let style = growX_

      let xAxisSizing :: StyleValue Sizing
          xAxisSizing = getSizing style (ElementContext True (0, 0)) XAxis

      xAxisSizing `shouldBe` StyleValue (Grow mempty)

    it "returns hovered value when hovered and hovered value set" $ do
      let style = hovered growX_
          xAxisSizingHovered = getSizing style (ElementContext True (0, 0)) XAxis

      xAxisSizingHovered `shouldBe` StyleValue (Grow mempty)

    it "returns default value when not hovered and hovered value set" $ do
      let style = hovered growX_
          xAxisSizingNotHovered = getSizing style (ElementContext False (0, 0)) XAxis

      xAxisSizingNotHovered `shouldBe` Default

    it "returns correct value when not hovered and hovered and not hovered value set" $ do
      let style = hovered growX_ <> fitX_
          xAxisSizingNotHovered = getSizing style (ElementContext False (0, 0)) XAxis

      xAxisSizingNotHovered `shouldBe` StyleValue (Fit mempty)

    it "returns correct value when not hovered and hovered and not hovered value set" $ do
      let style = hovered growX_ <> fitX_
          xAxisSizingNotHovered = getSizing style (ElementContext True (0, 0)) XAxis

      xAxisSizingNotHovered `shouldBe` StyleValue (Grow mempty)

    it "latter fields override previous" $ do
      let style = fitX_ <> growX_

      let xAxisSizing :: StyleValue Sizing
          xAxisSizing = getSizing style (ElementContext False (0, 0)) XAxis

      xAxisSizing `shouldBe` StyleValue (Grow mempty)

    it "different fields are preserved" $ do
      let style = fitX_ <> growY_
      let yAxisSizing :: StyleValue Sizing
          yAxisSizing = getSizing style (ElementContext False (0, 0)) YAxis
      let xAxisSizing :: StyleValue Sizing
          xAxisSizing = getSizing style (ElementContext False (0, 0)) XAxis

      xAxisSizing `shouldBe` StyleValue (Fit mempty)
      yAxisSizing `shouldBe` StyleValue (Grow mempty)
