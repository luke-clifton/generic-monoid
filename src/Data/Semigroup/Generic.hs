{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Semigroup.Generic
    ( genericMappend
    , GenericSemigroup(..)
    ) where

import GHC.TypeLits
import Data.Semigroup
import GHC.Generics

-- | A newtype which allows you to using the @DerivingVia@ extension
-- to reduce boilerplate.
--
-- @@@
-- data X = X [Int] String
--   deriving (Generic, Show)
--   deriving Semigroup via GenericSemigroup X
-- @@@
newtype GenericSemigroup a = GenericSemigroup a

instance
    (Generic a, MappendProduct (Rep a))
    => Semigroup (GenericSemigroup a) where
    (GenericSemigroup a) <> (GenericSemigroup b)
        = GenericSemigroup $ genericMappend a b

-- | A generic @`<>`@ function which works for product types where each
-- contained type is itself a @`Semigroup`@. It simply calls @`<>`@ for
-- each field.
--
-- If you don't want to use the @deriving via@ mechanism, use this function
-- to implement the `Semigroup` type class.
genericMappend :: (Generic a, MappendProduct (Rep a)) => a -> a -> a
genericMappend a b = to $ from a `genericMappend'` from b

class MappendProduct f where
    genericMappend' :: f k -> f k -> f k

instance
    (TypeError (Text "You can't use `genericMappend` for sum types"))
    => MappendProduct (a :+: b) where
    genericMappend' = undefined

instance MappendProduct c => MappendProduct (D1 md c) where
    genericMappend' (M1 a) (M1 b) = M1 (genericMappend' a b)

instance MappendProduct s => MappendProduct (C1 mc s) where
    genericMappend' (M1 a) (M1 b) = M1 (genericMappend' a b)

instance (MappendProduct a, MappendProduct b) => MappendProduct (a :*: b) where
    genericMappend' (a :*: b) (a' :*: b')
        = genericMappend' a a' :*: genericMappend' b b'

instance Semigroup t => MappendProduct (S1 m (Rec0 t)) where
    genericMappend' (M1 (K1 a)) (M1 (K1 b)) = M1 (K1 (a <> b))
