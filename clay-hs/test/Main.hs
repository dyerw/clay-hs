module Main (main) where

import qualified Clay.RenderSpec
import qualified Raw.TypesSpec
import qualified RawSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Raw" RawSpec.spec
  describe "Raw.Types" Raw.TypesSpec.spec
  describe "Clay.Render" Clay.RenderSpec.spec
