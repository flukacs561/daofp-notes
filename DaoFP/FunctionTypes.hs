module DaoFP.FunctionTypes where

import Prelude hiding (either, curry, uncurry)

-- The elimination rule for functions types is the existence of the `apply' function
apply :: (a -> b, a) -> b
apply (f, x) = f x

{-
The introduction rule for function types is a bijection between arrows mapping into the function object c -> (a->b) and
functions cxa -> b.

Given h : c -> (a->b) we can form hxid : cxa -> (a->b)xa and compose with `apply': cxa -> (a->b)xa -> b.
This gives the correspoinding arrow.

The other direction is assumed as part of the universal property.

This somewhat foreshadows the currying adjunction.
-}
curry :: ((c, a) -> b) -> (c -> (a -> b))
curry f = \c -> \a -> f (c, a)

uncurry :: (c -> (a -> b)) -> ((c, a) -> b)
uncurry f = \(a, b) -> f a b

-- Then the introduction rule can be formulated as: h = curry f.

-- Now we can construct the introduction rules of products and coproducts explicitly.
mapOut :: (a -> c, b -> c) -> (Either a b -> c)
mapOut (f, g) = \aorb -> case aorb of
  Left a -> f a
  Right b -> g b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right y) = g y

unEither :: (Either a b -> c) -> (a -> c, b -> c)
unEither h = (h . Left, h . Right)

(&&&) :: (c -> a) -> (c -> b) -> (c -> (a, b))
(f &&& g) x = (f x, g x)

infixl 8 &&&

fork :: (c -> (a, b)) -> (c -> a, c -> b)
fork h = (fst . h, snd . h)

class Bifunctor f where
  bimap :: (a -> a') -> (b -> b') -> f a b -> f a' b'

instance Bifunctor Either where
  bimap f g = either (Left . f) (Right . g)

instance Bifunctor (,) where
  bimap f g = (f . fst) &&& (g . snd)

class Profunctor f where
  dimap :: (a' -> a) -> (b -> b') -> f a b -> f a' b'

instance Profunctor (->) where
  dimap f g h = g . h . f

-- Products ans coproducts distribute over one another
dist :: Either (b, a) (c, a) -> (Either b c, a)
dist (Left (b, a)) = (Left b, a)
dist (Right (c, a)) = (Right c, a)

dist' :: Either (b, a) (c, a) -> (Either b c, a)
dist' = either f g
  where
    f :: (b, a) -> (Either b c, a)
    f = f' &&& f''
    f' :: (b, a) -> Either b c
    f' = Left . fst
    f'' :: (b, a) -> a
    f'' = snd
    g :: (c, a) -> (Either b c, a)
    g = g' &&& g''
    g' :: (c, a) -> Either b c
    g' = Right . fst
    g'' :: (c, a) -> a
    g'' = snd

dist'' :: Either (b, a) (c, a) -> (Either b c, a)
dist'' = either ((Left . fst) &&& snd) ((Right . fst) &&& snd)

undist :: (Either b c, a) -> Either (b, a) (c, a)
undist (Left b, a) = Left (b, a)
undist (Right c, a) = Right (c, a)

undist' :: (Either b c, a) -> Either (b, a) (c, a)
undist' = uncurry f
  where
    f :: Either b c -> a -> Either (b, a) (c, a)
    f = either g h
    g :: b -> a -> Either (b, a) (c, a)
    g = curry g'
    g' :: (b, a) -> Either (b, a) (c, a)
    g' = Left
    h :: c -> a -> Either (b, a) (c, a)
    h = curry h'
    h' :: (c, a) -> Either (b, a) (c, a)
    h' = Right

undist'' :: (Either b c, a) -> Either (b, a) (c, a)
undist'' = uncurry $ either (curry Left) (curry Right)

-- Ex. 6.3.1.
-- 2xa = a+a
ex631 :: (Bool, a) -> Either a a
ex631 (True, a) = Left a
ex631 (False, a) = Right a

-- Note that Bool is isomorphic to Either () ()
ex631' :: (Either () (), a) -> Either a a
ex631' = uncurry $ either (curry (Left . leftUnitor)) (curry (Right . leftUnitor))

leftUnitor :: ((), a) -> a
leftUnitor ((), a) = a
