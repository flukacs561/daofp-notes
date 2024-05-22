{-# LANGUAGE GADTs #-}

module DaoFP.SumTypes where

import Prelude hiding (Bool, True, False, Either, Left, Right )
import Data.Void (Void, absurd)


-- Bool

data Bool where
  True :: Bool
  False :: Bool

-- Ex. 4.11.
boolToBool1 :: Bool -> Bool
boolToBool1 b = False

boolToBool2 :: Bool -> Bool
boolToBool2 b = True

idBool :: Bool -> Bool
idBool = id

-- Sum Types

data Either a b where
  Left :: a -> Either a b
  Right :: b -> Either a b

-- Ex. 4.4.1.
aPlus0IsA :: Either a Void -> a
aPlus0IsA (Left x) = x

aIsAPlus0 :: a -> Either a Void
aIsAPlus0 = Left

-- Ex. 4.4.3.
sumCommutes :: forall a b. Either a b -> Either b a
sumCommutes (Left x) = Right x
sumCommutes (Right y) = Left y
