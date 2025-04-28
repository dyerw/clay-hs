{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Clay.Layout.Config where

import GHC.Generics
  ( C1,
    D1,
    Generic (Rep, from, to),
    K1 (..),
    M1 (M1),
    Rec0,
    S1,
    (:*:) (..),
  )

newtype Config a = Config a

instance (Generic a, GenericConfig (Rep a)) => Semigroup (Config a) where
  (Config a) <> (Config b) =
    Config $
      to $
        genericOverride (from a) (from b)

instance (Generic a, GenericConfig (Rep a)) => Monoid (Config a) where
  mempty = Config $ to genericDefault

class GenericConfig f where
  genericOverride :: f a -> f a -> f a
  genericDefault :: f a

instance (GenericConfig c) => GenericConfig (D1 md c) where
  genericOverride (M1 a) (M1 b) = M1 (genericOverride a b)
  genericDefault = M1 genericDefault

instance (GenericConfig c) => GenericConfig (C1 md c) where
  genericOverride (M1 a) (M1 b) = M1 (genericOverride a b)
  genericDefault = M1 genericDefault

instance (GenericConfig a, GenericConfig b) => GenericConfig (a :*: b) where
  genericOverride (a1 :*: b1) (a2 :*: b2) =
    genericOverride a1 a2 :*: genericOverride b1 b2
  genericDefault = genericDefault :*: genericDefault

instance (Monoid t) => GenericConfig (S1 m (Rec0 t)) where
  genericOverride (M1 (K1 a)) (M1 (K1 b)) = M1 (K1 (a <> b))
  genericDefault = M1 (K1 mempty)

newtype ConfigValue a = ConfigValue {toMaybe :: Maybe a}
  deriving (Eq, Show)

configValue :: a -> ConfigValue a
configValue = ConfigValue . Just

instance Semigroup (ConfigValue a) where
  cv <> (ConfigValue Nothing) = cv
  _ <> cv = cv

instance Monoid (ConfigValue a) where
  mempty = ConfigValue Nothing
