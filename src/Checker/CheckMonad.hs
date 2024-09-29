{-# LANGUAGE InstanceSigs #-}

module Checker.CheckMonad ( Check (..) ) where

data Check a = Check a | SemanticError String

instance Functor Check where
  fmap :: (a -> b) -> Check a -> Check b
  fmap f (Check x) = Check $ f x
  fmap _ (SemanticError line) = SemanticError line

instance Applicative Check where
  pure :: a -> Check a
  pure = Check
  (<*>) :: Check (a -> b) -> Check a -> Check b
  (<*>) (Check f) (Check x) = Check $ f x
  (<*>) _ (SemanticError line) = SemanticError line
  (<*>) (SemanticError line) _ = SemanticError line
     
instance Monad Check where
  (>>=) :: Check a -> (a -> Check b) -> Check b
  (>>=) (Check x) f = f x
  (>>=) (SemanticError line) _ = SemanticError line

