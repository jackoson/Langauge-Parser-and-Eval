{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Eval where

num::Int->Int
num i = i
plus::Int->Int->Int
plus a b = a + b

eval'::Int->Int
eval' x = x

example1 = eval' $ plus (plus (num 1) (plus (num 2) (num 3))) (num 4)

data Expr' = Num' Int
           | Plus' Expr' Expr'

instance Show Expr' where
  show (Num' i) = show i
  show (Plus' a b) = "(" ++ show a ++ " + " ++ show b ++ ")"

eval'' :: Expr'->Int
eval'' (Num' i) = i
eval'' (Plus' a b) = (eval'' a) + (eval'' b)

example2 = eval'' $ Plus' (Plus' (Num' 1) (Plus' (Num' 2) (Num' 3))) (Num' 4)

data Expr k = Num Int
            | Plus k k
            | Times k k
              deriving Show

instance Functor Expr where
  fmap _ (Num i) = Num i
  fmap f (Plus a b) = Plus (f a) (f b)
  fmap f (Times a b) = Times (f a) (f b)

data Fix f = In (f (Fix f))
deriving instance (Show (f (Fix f))) => Show (Fix f)

inop (In a) = a

eval''' :: Fix Expr -> Int
eval''' (In (Num i)) = i
eval''' (In (Plus a b)) = eval''' a + eval''' b
eval''' (In (Times a b)) = eval''' a * eval''' b

example3 = eval''' $ In (Plus (In (Plus (In (Num 1)) (In (Plus (In (Num 2)) (In (Num 3)))))) (In (Num 4)))

cata alg = alg . fmap (cata alg) . inop

eval :: Fix Expr -> Int
eval = cata alg

alg::Expr Int -> Int
alg (Num i) = i
alg (Plus a b ) = a + b
alg (Times a b ) = a * b

example4 = eval $ In (Plus (In (Plus (In (Num 1)) (In (Plus (In (Num 2)) (In (Num 3)))))) (In (Num 4)))
