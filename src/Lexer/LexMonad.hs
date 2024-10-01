{-# LANGUAGE InstanceSigs #-}

module Lexer.LexMonad ( Lex (..) ) where

data Lex a = Lex a | LexicalError String

instance Functor Lex where
  fmap :: (a -> b) -> Lex a -> Lex b
  fmap f (Lex x) = Lex $ f x
  fmap _ (LexicalError line) = LexicalError line

instance Applicative Lex where
  pure :: a -> Lex a
  pure = Lex
  (<*>) :: Lex (a -> b) -> Lex a -> Lex b
  (<*>) (Lex f) (Lex x) = Lex $ f x
  (<*>) _ (LexicalError line) = LexicalError line
  (<*>) (LexicalError line) _ = LexicalError line
     
instance Monad Lex where
  (>>=) :: Lex a -> (a -> Lex b) -> Lex b
  (>>=) (Lex x) f = f x
  (>>=) (LexicalError line) _ = LexicalError line


