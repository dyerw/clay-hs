module Main (main) where

import qualified RawSpec
import qualified Raw.TypesSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Raw" RawSpec.spec
  describe "Raw.Types" Raw.TypesSpec.spec