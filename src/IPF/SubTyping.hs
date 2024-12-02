module IPF.SubTyping
  ( Error (..),
    SubTypeM,
    ctxError,
    ensureGround,
    fresh,
    pos,
    neg,
  )
where

import Control.Category ((>>>))
import Control.Monad (void)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.State (State)
import Control.Monad.State.Class (MonadState, state)
import Data.Bifunctor (bimap)
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
  deriving (Show)

type SubTypeM = ExceptT Error (State T.Ext)

ctxError :: Ctx.CtxM a -> SubTypeM a
ctxError = either (CtxError >>> throwError) pure

-- "We call types that do not contain any existential variables ground"
ensureGround :: T.Pos -> SubTypeM ()
ensureGround t = T.cataPos go (bisequenceA >>> void) t
  where
    go = \case
      T.Ext {} -> throwError (TypeNotGround t)
      p -> sequenceA p & void

fresh :: (MonadState T.Ext m) => m T.Ext
fresh = state (\x -> (x, succ x))

pos :: Ctx -> T.Pos -> T.Pos -> SubTypeM Ctx
pos ctx =
  curry $
    bimap T.unPos T.unPos >>> \case
      -- Added List rule, TODO this should probably be invariant
      (T.Lst p, T.Lst q) -> pos ctx p q
      -- ARefl
      (T.Var x, T.Var y) | x == y -> do
        Ctx.ensureVarIn x ctx & ctxError
        pure ctx
      -- Ainst
      (p, T.Ext x) -> do
        ensureGround (T.Pos p)
        (ctxl, ctxr) <-
          ctxError (Ctx.splitAtExt x ctx) >>= \case
            (_, Just _, _) -> throwError (AlreadySolved x)
            (ctxl, _, ctxr) -> pure (ctxl, ctxr)
        Ctx.wellFormedPos ctxl (T.Pos p) & ctxError
        pure (ctxl <> Sq.singleton (Ctx.Solved x (T.Pos p)) <> ctxr)
      -- AShiftNeg
      (T.ShiftN n, T.ShiftN m) -> do
        ctx' <- neg ctx m n
        neg ctx' n (Ctx.applyNeg ctx' m)
      -- No match
      (p, q) -> throwError (NoPosSubType (T.Pos p) (T.Pos q))

neg :: Ctx -> T.Neg -> T.Neg -> SubTypeM Ctx
neg ctx =
  curry $
    bimap T.unNeg T.unNeg >>> \case
      -- AForallR
      (n, T.Forall x m) ->
        neg (ctx Sq.:|> Ctx.Var x) (T.Neg n) m >>= \case
          (ctx' Sq.:|> Ctx.Var y) | x == y -> pure ctx'
          ctx' -> throwError (UnexpectedCtx ctx')
      -- AForallL
      (T.Forall x n, m) -> do
        -- We are sure m is not a Forall as it is matched above
        x' <- fresh
        neg
          (ctx Sq.:|> Ctx.Unsolved x')
          (T.substVarNeg x (T.Pos (T.Ext x')) n)
          (T.Neg m)
          >>= (Ctx.stripExtRight x' >>> ctxError)
      -- AArrow
      (T.Arrow p n, T.Arrow q m) -> do
        ctx' <- pos ctx p q
        neg ctx' (Ctx.applyNeg ctx' n) m
      -- AShiftPos
      (T.ShiftP p, T.ShiftP q) -> do
        ctx' <- pos ctx p q
        pos ctx' (Ctx.applyPos ctx' p) q
      -- No match
      (n, m) -> throwError (NoNegSubType (T.Neg n) (T.Neg m))
