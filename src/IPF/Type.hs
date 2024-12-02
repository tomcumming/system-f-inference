module IPF.Type
  ( Var,
    Ext,
    Pos (..),
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
    normalise,
  )
where

import Control.Category ((>>>))
import Control.Monad.Writer (execWriter, tell)
import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.Bifunctor (Bifunctor, bimap)
import Data.Bitraversable (Bitraversable, bitraverse)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.Map qualified as M

type Var = String

type Ext = Int

data Pos' n p
  = Var Var
  | Ext Ext
  | Lst p
  | ShiftN n
  deriving (Functor, Foldable, Traversable, Eq, Show)

instance Bifunctor Pos' where
  bimap fp fn = bitraverse (fp >>> pure) (fn >>> pure) >>> runIdentity

instance Bifoldable Pos' where
  bifoldMap fp fn = bitraverse (fp >>> tell) (fn >>> tell) >>> execWriter

instance Bitraversable Pos' where
  bitraverse fn fp = \case
    Var x -> Var x & pure
    Ext x -> Ext x & pure
    Lst p -> Lst <$> fp p
    ShiftN n -> ShiftN <$> fn n

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

newtype Neg = Neg {unNeg :: Neg' Pos Neg}
  deriving (Eq, Show)

newtype Pos = Pos {unPos :: Pos' Neg Pos}
  deriving (Eq, Show)

cataPos :: (Pos' n p -> p) -> (Neg' p n -> n) -> Pos -> p
cataPos fp fn =
  unPos
    >>> bimap (cataNeg fp fn) (cataPos fp fn)
    >>> fp

cataNeg :: (Pos' n p -> p) -> (Neg' p n -> n) -> Neg -> n
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
varFreeInPos x =
  unPos >>> \case
    Var y -> x == y
    Ext {} -> False
    Lst p -> varFreeInPos x p
    ShiftN n -> varFreeInNeg x n

type Subst = M.Map Ext Pos

singleSubst :: Subst -> Pos' Neg Pos -> Pos' Neg Pos
singleSubst s = \case
  Ext x | Just (Pos p) <- s M.!? x -> p
  p -> p

substVarPos :: Var -> Pos -> Pos -> Pos
substVarPos x p =
  unPos >>> \case
    Var y | x == y -> p
    p' -> bimap (substVarNeg x p) (substVarPos x p) p' & Pos

substVarNeg :: Var -> Pos -> Neg -> Neg
substVarNeg x p =
  unNeg >>> \case
    Forall y n
      | x == y -> Forall y n & Neg
    n -> bimap (substVarPos x p) (substVarNeg x p) n & Neg

-- TODO why do i need this?
normalise :: Neg -> Neg
normalise =
  unNeg >>> \case
    ShiftP (Pos (ShiftN n)) -> normalise n
    n -> bimap normalisePos normalise n & Neg
  where
    normalisePos = unPos >>> bimap normalise normalisePos >>> Pos
