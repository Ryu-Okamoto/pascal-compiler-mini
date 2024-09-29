module Lexer.Lexer ( run ) where

import Token ( Token )

run :: FilePath -> IO [Token]
run _ = return [] 