module Lexer.Lexer ( run ) where

import Token ( Token )
import Lexer.LexMonad ( Lex (..) )

run :: FilePath -> IO (Lex [Token])
run = fmap split2Tokens . readFile

split2Tokens :: String -> Lex [Token]
split2Tokens sourceCode = return []
