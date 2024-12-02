module Tests.IPF (ipfTests) where

import Control.Category ((>>>))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalState)
import Data.Function ((&))
import Data.Map qualified as M
import IPF.Expr qualified as Expr
import IPF.Inference (inferComp)
import IPF.Inference qualified as Infer
import IPF.Type qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

shftn :: T.Neg -> T.Pos
shftn = T.ShiftN >>> T.Pos

shftp :: T.Pos -> T.Neg
shftp = T.ShiftP >>> T.Neg

pvar :: T.Var -> T.Pos
pvar = T.Var >>> T.Pos

exampleEnv :: Infer.Env
exampleEnv =
  M.fromList
    [ ( "head",
        shftn $
          T.Neg $
            T.Forall "a" $
              T.Neg $
                T.Arrow (T.Pos $ T.Lst $ pvar "a") (shftp $ pvar "a")
      ),
      ( "id",
        shftn $
          T.Neg $
            T.Forall "a" $
              T.Neg $
                T.Arrow (pvar "a") (shftp $ pvar "a")
      ),
      ("ids", T.Pos $ T.Lst $ exampleEnv M.! "id")
    ]

lookupAsNeg :: Expr.Var -> T.Neg
lookupAsNeg x =
  exampleEnv M.! x
    & T.ShiftP
    & T.Neg

ipfTests :: TestTree
ipfTests =
  testGroup
    "IPF"
    [ paperExample
        "head ids"
        (Expr.Let "t" (Expr.Var "head") [Expr.Var "ids"] (Expr.Ret (Expr.Var "t")))
        (Just (lookupAsNeg "id" & T.normalise))
    ]

paperExample :: String -> Expr.Comp -> Maybe T.Neg -> TestTree
paperExample name c expectedRes = testCase name $ do
  let maybeRes =
        evalState
          (runReaderT (inferComp mempty c) exampleEnv & runExceptT)
          1
  case (fst >>> T.normalise) <$> maybeRes of
    Left err -> case expectedRes of
      Nothing -> pure ()
      _ -> fail $ show err
    Right res -> case expectedRes of
      Just res'
        | res == res' -> pure ()
        | otherwise -> fail $ show (res, res')
      _ -> fail $ show res
