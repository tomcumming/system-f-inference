module IPF.SubTyping
  ( subTypePos,
    subTypeNeg,
  )
where

import Control.Category ((>>>))
import Control.Monad (void)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT, state)
import Control.Monad.Trans (lift)
import Data.Bifunctor (bimap, first)
import Data.Bitraversable (bisequenceA)
import Data.Function ((&))
import Data.Sequence qualified as Sq
import IPF.Ctx (Ctx)
import IPF.Ctx qualified as Ctx
import IPF.Type qualified as T

data Error
  = CtxError Ctx.Error
  | AlreadySolved T.Ext
  | TypeNotGround T.Pos
  | NoPosSubType T.Pos T.Pos
  | NoNegSubType T.Neg T.Neg
  | UnexpectedCtx Ctx.Ctx

type SubTypeM = StateT T.Ext (Either Error)

ctxError :: Ctx.CtxM a -> SubTypeM a
ctxError = first CtxError >>> lift

ensureGround :: T.Pos -> SubTypeM ()
ensureGround t = T.cataPos go (bisequenceA >>> void) t
  where
    go = \case
      T.Ext {} -> throwError (TypeNotGround t)
      p -> sequenceA p & void

fresh :: SubTypeM T.Ext
fresh = state (\x -> (x, succ x))

subTypePos :: Ctx -> T.Pos -> T.Pos -> SubTypeM Ctx
subTypePos ctx = curry $ \case
  -- ARefl
  (T.Var x, T.Var y) | x == y -> do
    Ctx.ensureVarIn x ctx & ctxError
    pure ctx
  -- Ainst
  (p, T.Ext x) -> do
    ensureGround p
    (ctxl, ctxr) <-
      ctxError (Ctx.splitAtExt x ctx) >>= \case
        (_, Just _, _) -> throwError (AlreadySolved x)
        (ctxl, _, ctxr) -> pure (ctxl, ctxr)
    Ctx.wellFormedPos ctxl p & ctxError
    pure (ctxl <> Sq.singleton (Ctx.Solved x p) <> ctxr)
  -- AShiftNeg
  (T.ShiftN n, T.ShiftN m) -> do
    ctx' <- subTypeNeg ctx m n
    subTypeNeg ctx' n (Ctx.applyNeg ctx' m)
  -- No match
  (p, q) -> throwError (NoPosSubType p q)

subTypeNeg :: Ctx -> T.Neg -> T.Neg -> SubTypeM Ctx
subTypeNeg ctx =
  curry $
    bimap T.unNeg T.unNeg >>> \case
      -- AForallR
      (n, T.Forall x m) ->
        subTypeNeg (ctx Sq.:|> Ctx.Var x) (T.Neg n) m >>= \case
          (ctx' Sq.:|> Ctx.Var y) | x == y -> pure ctx'
          ctx' -> throwError (UnexpectedCtx ctx')
      -- AForallL
      (T.Forall x n, m) -> do
        -- We are sure m is not a Forall as it is matched above
        x' <- fresh
        subTypeNeg
          (ctx Sq.:|> undefined)
          (Ctx.substVarNeg x (T.Ext x') n)
          (T.Neg m)
          >>= (Ctx.stripExtRight x' >>> ctxError)
      -- AArrow
      (T.Arrow p n, T.Arrow q m) -> do
        ctx' <- subTypePos ctx p q
        subTypeNeg ctx' (Ctx.applyNeg ctx' n) m
      -- AShiftPos
      (T.ShiftP p, T.ShiftP q) -> do
        ctx' <- subTypePos ctx p q
        subTypePos ctx' (Ctx.applyPos ctx' p) q
      -- No match
      (n, m) -> throwError (NoNegSubType (T.Neg n) (T.Neg m))
