# Generic Monoid (and Semigroup)

This library provides a method of deriving `Semigroup` and `Monoid` instances
for your large product types. It does this using GHC generics, and can provides
a mechanism for using the `DerivingVia` extension to reduce boilerplate.

It only works if each field of your product type is itself a `Semigroup`/`Monoid`.

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE DeriveGeneric      #-}

import GHC.Generics
import Data.Monoid.Generic

data BigProduct = BigProduct
    { theList   :: [Int]
    , theSum    :: Sum Double
    , theString :: String
    } deriving (Generic, Eq)
    deriving Semigroup via GenericSemigroup BigProduct
    deriving Monoid    via GenericMonoid BigProduct

useIt :: Bool
useIt = (mempty <> mempty) == BigProduct [] 0 ""
```
