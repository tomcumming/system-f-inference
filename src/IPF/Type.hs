module IPF.Type
  ( Var,
    Ext,
    Pos,
    Neg (..),
    Neg' (..),
    Pos' (..),
    cataPos,
    cataNeg,
    varFreeInPos,
    varFreeInNeg,
    Subst,
    singleSubst,
    substVarPos,
    substVarNeg,
  )
where

import Control.Category ((>>>))
import Control.Monad.Writer (execWriter, tell)
import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.Bifunctor (Bifunctor, bimap)
import Data.Bitraversable (Bitraversable, bitraverse)
import Data.Functor.Identity (runIdentity)
import Data.Function ((&))
import qualified Data.Map as M

type Var = String

type Ext = Int

data Pos' n
  = Var Var
  | Ext Ext
  | ShiftN n
  deriving (Functor, Foldable, Traversable, Eq, Show)

data Neg' p n
  = Arrow p n
  | Forall Var n
  | ShiftP p
  deriving (Functor, Eq, Show)

instance Bifunctor Neg' where
  bimap fp fn = bitraverse (fp >>> pure) (fn >>> pure) >>> runIdentity

instance Bifoldable Neg' where
  bifoldMap fp fn = bitraverse (fp >>> tell) (fn >>> tell) >>> execWriter

instance Bitraversable Neg' where
  bitraverse fp fn = \case
    Arrow p n -> Arrow <$> fp p <*> fn n
    Forall x n -> Forall x <$> fn n
    ShiftP p -> ShiftP <$> fp p

newtype Neg = Neg {unNeg :: Neg' (Pos' Neg) Neg}
  deriving (Eq, Show)

type Pos = Pos' Neg

cataPos :: (Pos' n -> p) -> (Neg' p n -> n) -> Pos -> p
cataPos fp fn = fmap (cataNeg fp fn) >>> fp

cataNeg :: (Pos' n -> p) -> (Neg' p n -> n) -> Neg -> n
cataNeg fp fn =
  unNeg
    >>> bimap (cataPos fp fn) (cataNeg fp fn)
    >>> fn

varFreeInNeg :: Var -> Neg -> Bool
varFreeInNeg x =
  unNeg >>> \case
    Arrow p n -> varFreeInPos x p || varFreeInNeg x n
    Forall y n
      | x == y -> False
      | otherwise -> varFreeInNeg x n
    ShiftP p -> varFreeInPos x p

varFreeInPos :: Var -> Pos -> Bool
varFreeInPos x = \case
  Var y -> x == y
  Ext {} -> False
  ShiftN n -> varFreeInNeg x n

type Subst = M.Map Ext Pos

singleSubst :: Subst -> Pos -> Pos
singleSubst s = \case
  Ext x | Just p <- s M.!? x -> p
  p -> p

substVarPos :: Var -> Pos -> Pos -> Pos
substVarPos x p = fmap (substVarNeg x p)

substVarNeg :: Var -> Pos -> Neg -> Neg
substVarNeg x p =
  unNeg >>> \case
    Forall y n
      | x == y -> Forall y n & Neg
    n -> bimap (substVarPos x p) (substVarNeg x p) n & Neg
