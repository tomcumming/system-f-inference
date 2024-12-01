module IPF.Ctx
  ( Ctx,
    Ctx' (..),
    Error (..),
    CtxM,
    ensureVarIn,
    splitAtExt,
    applyNeg,
    applyPos,
    wellFormedNeg,
    wellFormedPos,
    substVarPos,
    substVarNeg,
    stripExtRight,
  )
where

import Control.Category ((>>>))
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Sequence qualified as Sq
import IPF.Type qualified as T

type Ctx = Sq.Seq Ctx'

data Ctx'
  = Var T.Var
  | Unsolved T.Ext
  | Solved T.Ext T.Pos
  deriving (Eq, Show)

data Error
  = VarMissing T.Var
  | ExtMissing T.Ext
  | NoExtRight Ctx T.Ext

type CtxM = Either Error

ensureVarIn :: T.Var -> Ctx -> CtxM ()
ensureVarIn x = \case
  Sq.Empty -> throwError (VarMissing x)
  (_ Sq.:|> Var y) | x == y -> pure ()
  (ctx Sq.:|> _) -> ensureVarIn x ctx

splitAtExt :: T.Ext -> Ctx -> CtxM (Ctx, Maybe T.Pos, Ctx)
splitAtExt x = \case
  Sq.Empty -> throwError (ExtMissing x)
  ctx Sq.:|> Solved y p | x == y -> pure (ctx, Just p, Sq.Empty)
  ctx Sq.:|> Unsolved y | x == y -> pure (ctx, Nothing, Sq.Empty)
  ctx Sq.:|> c -> do
    (ctxl, found, ctxr) <- splitAtExt x ctx
    pure (ctxl, found, ctxr Sq.:|> c)

type Subst = M.Map T.Ext T.Pos

asSubst :: Ctx -> Subst
asSubst = Sq.reverse >>> foldMap go
  where
    go = \case
      Solved x p -> M.singleton x p
      _ -> mempty

singleSubst :: Subst -> T.Pos -> T.Pos
singleSubst s = \case
  T.Ext x | Just p <- s M.!? x -> p
  p -> p

applyNeg :: Ctx -> T.Neg -> T.Neg
applyNeg ctx = T.cataNeg (singleSubst (asSubst ctx)) T.Neg

applyPos :: Ctx -> T.Pos -> T.Pos
applyPos ctx = T.cataPos (singleSubst (asSubst ctx)) T.Neg

substVarPos :: T.Var -> T.Pos -> T.Pos -> T.Pos
substVarPos x p = fmap (substVarNeg x p)

substVarNeg :: T.Var -> T.Pos -> T.Neg -> T.Neg
substVarNeg x p =
  T.unNeg >>> \case
    T.Forall y n
      | x == y -> T.Forall y n & T.Neg
    n -> bimap (substVarPos x p) (substVarNeg x p) n & T.Neg

wellFormedPos :: Ctx -> T.Pos -> CtxM ()
wellFormedPos ctx = \case
  T.Var x -> ensureVarIn x ctx
  T.Ext {} -> pure ()
  T.ShiftN n -> wellFormedNeg ctx n

wellFormedNeg :: Ctx -> T.Neg -> CtxM ()
wellFormedNeg ctx =
  T.unNeg >>> \case
    T.Arrow p n -> wellFormedPos ctx p >> wellFormedNeg ctx n
    T.Forall x n -> wellFormedNeg (ctx Sq.:|> Var x) n
    T.ShiftP p -> wellFormedPos ctx p

stripExtRight :: T.Ext -> Ctx -> CtxM Ctx
stripExtRight x = \case
  (ctx Sq.:|> Unsolved y) | x == y -> pure ctx
  (ctx Sq.:|> Solved y _) | x == y -> pure ctx
  ctx -> throwError (NoExtRight ctx x)
