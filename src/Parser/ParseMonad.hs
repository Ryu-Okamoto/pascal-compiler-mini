{-# LANGUAGE InstanceSigs #-}

module Parser.ParseMonad ( Parse (..) ) where

data Parse a = Parse a | SyntaxError Int

instance Functor Parse where
  fmap :: (a -> b) -> Parse a -> Parse b
  fmap f (Parse x) = Parse $ f x
  fmap _ (SyntaxError line) = SyntaxError line

instance Applicative Parse where
  pure :: a -> Parse a
  pure = Parse
  (<*>) :: Parse (a -> b) -> Parse a -> Parse b
  (<*>) (Parse f) (Parse x) = Parse $ f x
  (<*>) _ (SyntaxError line) = SyntaxError line
  (<*>) (SyntaxError line) _ = SyntaxError line
     
instance Monad Parse where
  (>>=) :: Parse a -> (a -> Parse b) -> Parse b
  (>>=) (Parse x) f = f x
  (>>=) (SyntaxError line) _ = SyntaxError line

