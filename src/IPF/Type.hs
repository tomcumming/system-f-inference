module IPF.Type
  ( Var,
    Ext,
    Pos,
    Neg (..),
    Neg' (..),
    Pos' (..),
    cataPos,
    cataNeg,
  )
where

import Control.Category ((>>>))
import Control.Monad.Writer (execWriter, tell)
import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.Bifunctor (Bifunctor, bimap)
import Data.Bitraversable (Bitraversable, bitraverse)
import Data.Functor.Identity (runIdentity)

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
