module AST.Generator ( generate ) where

import AST.StrictLL1 ( EBNF )

generate :: FilePath -> FilePath -> IO ()
generate ebnfFilePath outputPath = do
  ebnfFile <- readFile ebnfFilePath
  -- TODO
  writeFile outputPath ebnfFile

parse :: String -> EBNF
parse = undefined
