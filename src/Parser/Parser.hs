module Parser.Parser ( run ) where

import Token ( Token (..) )
import AST
import Parser.ParseMonad ( Parse (..) )

run :: [Token] -> Parse AST
run _ = SyntaxError 0