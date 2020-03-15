
import Sally.SAT (var, Var(..), Fml(..))
import Sally.CNF (CFml(..), removeNot, toCNF, makeAlias, toDIMACS, fromDIMACS)
import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "SAT" $ do
    it "Make Simple Fml" $ do
      (var 123) `shouldBe` (FVar (Var 123))
      (var "str") `shouldBe` (FVar (Var "str"))
    it "Make And/Or Fml" $ do
      (And [(var 123), (var 456)]) `shouldBe` (And [(FVar (Var 123)), (FVar (Var 456))])
      (Or [(var 123), (var 456)]) `shouldBe` (Or [(FVar (Var 123)), (FVar (Var 456))])
  describe "CNF" $ do
    it "Make And Fml" $ do
      let cnf = toCNF $ (And [(var 123), (var 456)])
      cnf `shouldBe` [[CAff (Var 123)], [CAff (Var 456)]]
    it "Make Or Fml" $ do
      let cnf = toCNF $ (Or [(var 123), (var 456)])
      cnf `shouldSatisfy` \cnf -> 
        case cnf of
          [[CAff (TmpVar _),CAff (Var 123)],[CNot (TmpVar _),CAff (Var 456)]] -> True
          _ -> False
