module Main (main) where

import qualified RawSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Raw" RawSpec.spec