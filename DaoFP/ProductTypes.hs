module DaoFP.ProductTypes where

data Pair a b = Pair {fst :: a, snd :: b}

bimap :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
bimap f g (Pair x y) = Pair (f x) (g y)

-- Ex. 5.1.2.
ex512 :: Either b (a, b) -> (Either () a, b)
ex512 x = (f x, g x)
  where
    f :: Either b (a, b) -> Either () a
    f (Left b) = Left ()
    f (Right (a, b)) = Right a

    g :: Either b (a, b) -> b
    g (Left b) = b
    g (Right (a, b)) = b

-- Ex. 5.1.3.
ex513 :: Either b (a, b) -> (Either () a, b)
ex513 (Left b) = (Left (), b)
ex513 (Right (a, b)) = (Right a, b)

-- Ex. 5.1.4.
maybeAB :: Either b (a, b) -> (Maybe a, b)
maybeAB (Left b) = (Nothing, b)
maybeAB (Right (a, b)) = (Just a, b)

class Monoid m where
  mappend :: (m, m) -> m
  mempty :: () -> m
