{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Monoid.Generic
    ( genericMappend
    , genericMempty
    , GenericSemigroup(..)
    , GenericMonoid(..)
    ) where

import Data.Semigroup.Generic
import GHC.Generics
import GHC.TypeLits

-- | A newtype which allows you to using the @DerivingVia@ extension
-- to reduce boilerplate.
--
-- @
-- data X = X [Int] String
--   deriving (Generic, Show)
--   deriving Semigroup via GenericSemigroup X
--   deriving Monoid    via GenericMonoid X
-- @
--
-- Note: Do NOT attempt to @derive Semigroup via GenericMonoid@. That will lead
-- to infinite recursion.
newtype GenericMonoid a = GenericMonoid a
    deriving Show

instance Semigroup a => Semigroup (GenericMonoid a) where
    GenericMonoid a <> GenericMonoid b = GenericMonoid $ a <> b

instance
    (Semigroup a, Generic a, MemptyProduct (Rep a))
    => Monoid (GenericMonoid a) where
    mempty = GenericMonoid genericMempty

-- | A generic @`mempty`@ function which works for product types where each
-- contained type is itself a @`Monoid`@. It simply calls @`mempty`@ for
-- each field.
--
-- If you don't want to use the @deriving via@ mechanism, use this function
-- to implement the `Monoid` type class.
genericMempty :: (Generic a, MemptyProduct (Rep a)) => a
genericMempty = to genericMempty'

class MemptyProduct f where
    genericMempty' :: f k

instance MemptyProduct c => MemptyProduct (D1 md c) where
    genericMempty' = M1 genericMempty'

instance MemptyProduct s => MemptyProduct (C1 md s) where
    genericMempty' = M1 genericMempty'

instance
    (TypeError (Text "You can't use `genericMempty` for sum types"))
    => MemptyProduct (a :+: b) where
    genericMempty' = undefined

instance (MemptyProduct a, MemptyProduct b) => MemptyProduct (a :*: b) where
    genericMempty' = genericMempty' :*: genericMempty'

instance Monoid t => MemptyProduct (S1 m (Rec0 t)) where
    genericMempty' = M1 (K1 mempty)
