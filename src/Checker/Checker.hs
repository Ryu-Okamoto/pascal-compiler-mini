module Checker.Checker ( run ) where

import Token ( Token (..) )
import AST
import qualified Parser.Parser as Parser ( run )
import Checker.CheckMonad ( Check (..) )

run :: AST -> Check AST
run _ = SemanticError ""