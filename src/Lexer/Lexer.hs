module Lexer.Lexer ( run ) where

import Token ( Token (..), buildToken )
import Lexer.LexMonad ( Lex (..) )

import Data.Char ( isNumber, isAlpha )

run :: FilePath -> IO (Lex [Token])
run pasFilePath = splitToTokens <$> readFile pasFilePath

splitToTokens :: String -> Lex [Token]
splitToTokens sourceCode = tokenizeForLines codeLines startLine
  where
    codeLines = lines $ removeComments sourceCode
    startLine = 1

-- HACK: This implementation is redundant to evaluate restrictly.
tokenizeForLines :: [String] -> Int -> Lex [Token]
tokenizeForLines []    _          = return []
tokenizeForLines (h:t) lineNumber = do
  tokens <- lineToTokens h lineNumber
  rest <- tokenizeForLines t (lineNumber + 1)
  return $ tokens ++ rest

lineToTokens :: String -> Int -> Lex [Token]
lineToTokens ""    _         = return []
lineToTokens line lineNumber = do
  (firstToken, _, rest) <- extractFirstToken ([], State0, line)
  restTokens <- lineToTokens rest lineNumber
  case firstToken of
    "" -> return []
    _  -> return $ buildToken firstToken lineNumber : restTokens
  where
    extractFirstToken :: (String, DFAState, String) -> Lex (String, DFAState, String)
    extractFirstToken (extracted, State0,  ' ' :t) = extractFirstToken (extracted, State0, t)
    extractFirstToken (extracted, State0,  '\n':t) = extractFirstToken (extracted, State0, t)
    extractFirstToken (_        , State11, []    ) = LexicalError lineNumber
    extractFirstToken (extracted, StateF,  rest  ) = Lex (init extracted, State0, last extracted : rest)
    extractFirstToken (_        , StateE,  _     ) = LexicalError lineNumber
    extractFirstToken (extracted, _,       []    ) = Lex (extracted, State0, [])
    extractFirstToken (extracted, q,       h:t   ) = extractFirstToken (extracted ++ [h], transition q h, t)

data DFAState =
  State0  | State1  | State2  | State3  | State4  | State5  | State6  |
  State7  | State8  | State9  | State10 | State11 | State12 | StateF  | StateE
transition :: DFAState -> Char -> DFAState
transition State0   '='  = State3
transition State0   '+'  = State3
transition State0   '-'  = State3
transition State0   '*'  = State3
transition State0   '/'  = State3
transition State0   '('  = State3
transition State0   ')'  = State3
transition State0   '['  = State3
transition State0   ']'  = State3
transition State0   ';'  = State3
transition State0   ','  = State3
transition State0   '<'  = State4
transition State0   '>'  = State7
transition State0   ':'  = State7
transition State0   '.'  = State9
transition State0   '\'' = State11
transition State0   a
        | isNumber  a    = State1
        | isAlpha   a    = State2
        | otherwise      = StateE
transition State1   a
        | isNumber  a    = State1
        | otherwise      = StateF
transition State2   a
        | isAlpha   a    = State2
        | isNumber  a    = State2
        | otherwise      = StateF
transition State3   _    = StateF
transition State4   a
        | a == '>'       = State5
        | a == '='       = State6
        | otherwise      = StateF
transition State5   _    = StateF
transition State6   _    = StateF
transition State7   a
        | a == '='       = State8
        | otherwise      = StateF
transition State8   _    = StateF
transition State9   a
        | a == '.'       = State10
        | otherwise      = StateF
transition State10  _    = StateF
transition State11  a
        | a == '\''      = State12
        | a == '\n'      = StateE
        | otherwise      = State11
transition State12  _    = StateF
transition StateF   _    = StateE
transition StateE   _    = StateE

removeComments :: String -> String
removeComments code = replaceCommentsToWS code False
  where
    replaceCommentsToWS :: String -> Bool -> String
    replaceCommentsToWS ""      _     = ""
    replaceCommentsToWS ('{':t) False = " " ++ replaceCommentsToWS t True
    replaceCommentsToWS ('}':t) True  = " " ++ replaceCommentsToWS t False
    replaceCommentsToWS (h:t)   False = h   :  replaceCommentsToWS t False
    replaceCommentsToWS (h:t)   True  = ws  :  replaceCommentsToWS t True
      where
        ws = if h == '\n' then '\n' else ' '