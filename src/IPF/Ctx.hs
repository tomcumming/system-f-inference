module IPF.Ctx
  ( Ctx,
    Ctx' (..),
    Error (..),
    CtxM,
    varIn,
    ensureVarIn,
    splitAtExt,
    applyNeg,
    applyPos,
    wellFormedNeg,
    wellFormedPos,
    stripExtRight,
    stripVarRight,
    restrict,
  )
where

import Control.Category ((>>>))
import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Foldable (toList)
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
  | NoVarRight Ctx T.Var
  | RestrictMissing T.Ext Ctx
  deriving (Show)

type CtxM = Either Error

varIn :: T.Var -> Ctx -> Bool
varIn x = \case
  Sq.Empty -> False
  (_ Sq.:|> Var y) | x == y -> True
  (ctx Sq.:|> _) -> varIn x ctx

ensureVarIn :: T.Var -> Ctx -> CtxM ()
ensureVarIn x = varIn x >>> flip unless (throwError (VarMissing x))

splitAtExt :: T.Ext -> Ctx -> CtxM (Ctx, Maybe T.Pos, Ctx)
splitAtExt x = \case
  Sq.Empty -> throwError (ExtMissing x)
  ctx Sq.:|> Solved y p | x == y -> pure (ctx, Just p, Sq.Empty)
  ctx Sq.:|> Unsolved y | x == y -> pure (ctx, Nothing, Sq.Empty)
  ctx Sq.:|> c -> do
    (ctxl, found, ctxr) <- splitAtExt x ctx
    pure (ctxl, found, ctxr Sq.:|> c)

asSubst :: Ctx -> T.Subst
asSubst = Sq.reverse >>> foldMap go
  where
    go = \case
      Solved x p -> M.singleton x p
      _ -> mempty

applyNeg :: Ctx -> T.Neg -> T.Neg
applyNeg ctx = T.cataNeg (T.singleSubst (asSubst ctx) >>> T.Pos) T.Neg

applyPos :: Ctx -> T.Pos -> T.Pos
applyPos ctx = T.cataPos (T.singleSubst (asSubst ctx) >>> T.Pos) T.Neg

wellFormedPos :: Ctx -> T.Pos -> CtxM ()
wellFormedPos ctx =
  T.unPos >>> \case
    T.Var x -> ensureVarIn x ctx
    T.Ext {} -> pure ()
    T.Lst p -> wellFormedPos ctx p
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

stripVarRight :: T.Var -> Ctx -> CtxM Ctx
stripVarRight x = \case
  (ctx Sq.:|> Var y) | x == y -> pure ctx
  ctx -> throwError (NoVarRight ctx x)

extsIn :: Ctx -> M.Map T.Ext (Maybe T.Pos)
extsIn = foldMap $ \case
  Unsolved x -> M.singleton x Nothing
  Solved x p -> M.singleton x (Just p)
  Var {} -> mempty

restrict :: Ctx -> Ctx -> CtxM Ctx
restrict ctx1 = toList >>> traverse go >>> fmap Sq.fromList
  where
    exts = extsIn ctx1
    goExt x = case exts M.!? x of
      Nothing -> Left (RestrictMissing x ctx1)
      Just Nothing -> Unsolved x & pure
      Just (Just p) -> Solved x p & pure

    go = \case
      Solved x _ -> goExt x
      Unsolved x -> goExt x
      Var x -> Var x & pure
