
import Sally.SAT (var, Var(..), Fml(..))
import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "CNF" $ do
    it "Make Simple Fml" $ do
      (var 123) `shouldBe` (FVar (Var 123))
