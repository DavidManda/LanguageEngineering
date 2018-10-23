{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

data (f:+:g) a = L(f a) | R(g a)
infixr 5 :+:

data Fix f = In (f (Fix f))

inop :: Fix f -> f (Fix f)
inop (In x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg.(fmap (cata alg)).inop

instance (Functor f, Functor g) => Functor (f:+:g) where
  fmap f (L x) = L (fmap f x)
  fmap f (R x) = R (fmap f x)

data Val k = Val Int

data Add k = Add k k

data Sub k = Sub k k

data Mul k = Mul k k

instance Functor Val where
  fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add x y) = Add (f x) (f y)

instance Functor Sub where
  fmap f (Sub x y) = Sub (f x) (f y)

instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)

evalAddSub :: Fix(Val :+: Add :+: Sub) -> Int
evalAddSub = cata alg where
  alg (L (Val x)) = x
  alg (R (L (Add x y))) = x + y
  alg (R (R (Sub x y))) = x - y

  -- In (L (Val 2))

class Functor f => Alg f a where
  alg :: f a -> a

instance Alg Val Int where
  alg (Val x) = x

instance Alg Add Int where
  alg (Add x y) = x + y

instance Alg Sub Int where
  alg (Sub x y) = x - y

instance Alg Mul Int where
  alg (Mul x y) = x * y

instance (Alg f a, Alg g a) => Alg (f:+:g) a where
  alg (L x) = alg x
  alg (R x) = alg x

evalAddSub' :: Fix(Val :+: Add :+: Sub) -> Int
evalAddSub' = cata alg' where
  alg' (L x) = alg x
  alg' (R x) = alg x

cati :: Alg f a => Fix f -> a
cati = alg.(fmap (cata alg)).inop

type Expr = Fix (Val :+: Add :+: Sub :+: Mul)

eval :: Expr -> Int
eval = cati

val x = In (L (Val x))
add x y = In (R (L (Add x y)))
sub x y = In (R (R (L (Sub x y))))
mul x y = In (R (R (R (Mul x y))))

newtype Depth = Depth Int

instance Alg Val Depth where
  alg (Val x) = Depth 0

instance Alg Add Depth where
  alg (Add (Depth x) (Depth y)) = Depth (1 + max x y)

instance Alg Mul Depth where
  alg (Mul (Depth x) (Depth y)) = Depth (1 + max x y)

instance Alg Sub Depth where
  alg (Sub (Depth x) (Depth y)) = Depth (1 + max x y)

fromDepth :: Depth -> Int
fromDepth (Depth x) = x

evalDepth :: Expr -> Int
evalDepth = fromDepth.cati

-- (add (mul (val 3) (sub (val 2) (val 3))) ((val 2)))
