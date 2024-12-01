module IPF.Inference
  ( Error (..),
    inferValue,
    inferComp,
  )
where

import Control.Category ((>>>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT (..), mapExceptT)
import Control.Monad.Reader (ReaderT, local)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State (State)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first, second)
import Data.Bitraversable (bitraverse)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Sequence qualified as Sq
import IPF.Ctx (Ctx)
import IPF.Ctx qualified as Ctx
import IPF.Expr qualified as Expr
import IPF.SubTyping qualified as SubTy
import IPF.Type qualified as T

data Error
  = SubTyError SubTy.Error
  | MissingVar Expr.Var
  | NoInferArgs [Expr.Value] T.Neg

type Env = M.Map Expr.Var T.Pos

type InferM = ReaderT Env (ExceptT Error (State T.Ext))

subTyM :: SubTy.SubTypeM a -> InferM a
subTyM = mapExceptT (fmap (first SubTyError)) >>> lift

ctxM :: Ctx.CtxM a -> InferM a
ctxM = SubTy.ctxError >>> subTyM

lookupVar :: Expr.Var -> InferM T.Pos
lookupVar x = asks (M.!? x) >>= maybe (throwError (MissingVar x)) pure

inferValue :: Ctx -> Expr.Value -> InferM (T.Pos, Ctx)
inferValue ctx = \case
  Expr.Var x -> lookupVar x & fmap (,ctx)
  Expr.Thunk c -> inferComp ctx c & fmap (first T.ShiftN)

inferComp :: Ctx -> Expr.Comp -> InferM (T.Neg, Ctx)
inferComp ctx = \case
  -- ALambdaAbs
  Expr.Abs x p c ->
    inferComp ctx c
      & local (M.insert x p)
      & fmap (first (T.Arrow p >>> T.Neg))
  -- AGen
  Expr.TAbs x n ->
    inferComp (ctx Sq.:|> Ctx.Var x) n
      >>= bitraverse pure (Ctx.stripVarRight x >>> ctxM)
  -- AReturn
  Expr.Ret v -> inferValue ctx v & fmap (first (T.ShiftP >>> T.Neg))
  -- AUnambiguouslet
  Expr.Let x v ss t -> do
    (m, ctx') <- inferValue ctx v & fmap (first (T.ShiftP >>> T.Neg))
    (q, ctx'') <- inferArgs ctx' ss m & fmap (first T.ShiftN)
    -- TODO whats the difference between FEV() and ground?
    SubTy.ensureGround q & subTyM
    ctx''' <- Ctx.restrict ctx'' ctx & ctxM
    inferComp ctx''' t & local (M.insert x q)
  -- Aambiguouslet
  Expr.ALet x p v ss t -> do
    (m, ctx') <- inferValue ctx v & fmap (first (T.ShiftP >>> T.Neg))
    (q, ctx'') <- inferArgs ctx' ss m & fmap (first T.ShiftN)
    ctx''' <- SubTy.pos ctx'' p q & subTyM
    ctx4 <- SubTy.pos ctx''' (Ctx.applyPos ctx''' q) p & subTyM
    ctx5 <- Ctx.restrict ctx4 ctx & ctxM
    inferComp ctx5 t & local (M.insert x p)

inferArgs :: Ctx -> [Expr.Value] -> T.Neg -> InferM (T.Neg, Ctx)
inferArgs ctx =
  curry $
    second T.unNeg >>> \case
      -- ASpineNil
      ([], n) -> pure (T.Neg n, ctx)
      -- ASpineCons
      (v : s, T.Arrow q n) -> do
        (p, ctx') <- inferValue ctx v
        ctx'' <- SubTy.pos ctx' p (Ctx.applyPos ctx' q) & subTyM
        inferArgs ctx'' s (Ctx.applyNeg ctx'' n)
      (s, T.Forall x n)
        -- ASpineTypeAbsIn
        | T.varFreeInNeg x n -> do
            x' <- SubTy.fresh
            inferArgs (ctx Sq.:|> Ctx.Unsolved x') s (T.substVarNeg x (T.Ext x') n)
        -- ASpineTypeAbsNotIn
        | otherwise -> inferArgs ctx s n
      -- Otherwise
      (s, n) -> throwError (NoInferArgs s (T.Neg n))
