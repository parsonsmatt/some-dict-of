{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SomeDictOf
    ( module SomeDictOf
    -- * Re-exports
    , module Data.Functor.Identity
    , module Data.Proxy
    , module Data.Constraint
    ) where

import Data.Constraint
import Data.Functor.Identity
import Data.Kind
import Data.Proxy

-- | A datatype that carries evidence of type class membership for some
-- type, along with a datatype indexed by that type. This is confusing, so
-- let's look at some examples.
--
-- The alias @'SomeDict' clazz@ uses 'Proxy' for the datatype index. This means
-- that the wrapper is a @'Proxy' :: 'Proxy' a@, and you know that the type
-- @a@ has an instance of @clazz a@. We can construct a value
--
-- @
-- showSomeDict :: SomeDictOf Identity Show
-- showSomeDict =
--     SomeDict (Identity 3 :: Identity Int)
-- @
--
-- A value of @'SomeDict' 'Show'@ contains 'Proxy' value. When you pattern
-- match on the 'SomeDict', you know that the given @'Proxy' :: 'Proxy' a@
-- has an instance of 'Show'.
--
-- For the most part, this isn't useful,
--
-- @since 0.1.0.0
data SomeDictOf (f :: k -> Type) (c :: k -> Constraint) where
    SomeDictOf :: c a => f a -> SomeDictOf f c

-- | In the case where you only need evidence that the dictionary exists,
-- then you can use this type. By carrying a 'Proxy' instead of a real
-- value, we can summon these up whenever we have an instance of the type in
-- question.
--
-- @since 0.1.0.0
type SomeDict = SomeDictOf Proxy

-- | Construct a 'SomeDict' with the type in question. Use with
-- @TypeApplications@.
--
-- Example:
--
-- @
-- showDict :: SomeDict Show
-- showDict = someDict @Int
--
-- entityDict :: SomeDict PersistEntity
-- entityDict = someDict @User
-- @
--
-- @since 0.1.0.0
someDict :: forall a c. c a => SomeDict c
someDict = SomeDictOf (Proxy :: Proxy a)

-- | Construct a 'SomeDict' based on a 'Dict' from the "Data.Constraint" module.
--
-- @since 0.1.0.0
fromDict :: forall a c. Dict (c a) -> SomeDict c
fromDict Dict = SomeDictOf (Proxy @a)

-- | Unpack a 'SomeDictOf' and attain access to the evidence that the
-- underlying type satisfies the instance in question.
--
-- @since 0.1.0.0
withSomeDictOf
    :: SomeDictOf f c
    -> (forall a. c a => f a -> r)
    -> r
withSomeDictOf (SomeDictOf p) k =
    k p

-- | Transform the type index used in the 'SomeDictOf'. The provided function is
-- unable to inspect the type, but you will have the class operations available
-- to it.
--
-- Example:
--
-- @
-- provideMempty ;: SomeDictOf Proxy Monoid -> SomeDictOf Identity Monoid
-- provideMempty = mapSomeDictOf (\proxy -> Identity mempty)
-- @
--
-- @since 0.1.0.0
mapSomeDictOf
    :: (forall a. c a => f a -> g a)
    -> SomeDictOf f c
    -> SomeDictOf g c
mapSomeDictOf f (SomeDictOf fa) =
    SomeDictOf (f fa)

-- |  Not really sure what this might be useful for.
--
-- Examples:
--
-- @
-- shows :: SomeDictOf [] Show
-- shows = SomeDictOf [True, False, True]
--
-- forgotten :: [SomeDictOf Proxy Show]
-- forgotten = forgetContents show
--
-- main = do
--     forM forgotten $ \(SomeDictOf Proxy) -> do
--          print 10
-- @
--
-- The above program should output 10 three times.
--
-- @since 0.1.0.0
forgetContents
    :: forall f c. Functor f
    => SomeDictOf f c
    -> f (SomeDictOf Proxy c)
forgetContents (SomeDictOf (xs :: f a)) =
    fmap (\(_ :: a) -> SomeDictOf (Proxy @a)) xs
