{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Token (
  Token (..),
  buildToken,
  tkFile2Tokens
) where

import Data.Char ( isNumber )
import Data.List.Split ( splitOn )

data Token = Token {
  value      :: String,
  kind       :: String,
  id         :: String,
  lineNumber :: String
} deriving ( Show, Eq )

buildToken :: String -> String -> Token
buildToken (tokenH:tokenT) line
  | isNumber tokenH      = Token (tokenH:tokenT) "SCONSTANT" "44" line
  | tokenH == '\''       = Token (tokenH:tokenT) "SSTRING"   "45" line
buildToken token line
  | token == "and"       = Token "and"       "SAND"        "0"  line
  | token == "array"     = Token "array"     "SARRAY"      "1"  line
  | token == "begin"     = Token "begin"     "SBEGIN"      "2"  line
  | token == "boolean"   = Token "boolean"   "SBOOLEAN"    "3"  line
  | token == "char"      = Token "char"      "SCHAR"       "4"  line
  | token == "div"       = Token "div"       "SDIVD"       "5"  line
  | token == "/"         = Token "/"         "SDIVD"       "5"  line
  | token == "do"        = Token "do"        "SDO"         "6"  line
  | token == "else"      = Token "else"      "SELSE"       "7"  line
  | token == "end"       = Token "end"       "SEND"        "8"  line
  | token == "false"     = Token "false"     "SFALSE"      "9"  line
  | token == "if"        = Token "if"        "SIF"         "10" line
  | token == "integer"   = Token "integer"   "SINTEGER"    "11" line
  | token == "mod"       = Token "mod"       "SMOD"        "12" line
  | token == "not"       = Token "not"       "SNOT"        "13" line
  | token == "of"        = Token "of"        "SOF"         "14" line
  | token == "or"        = Token "or"        "SOR"         "15" line
  | token == "procedure" = Token "procedure" "SPROCEDURE"  "16" line
  | token == "program"   = Token "program"   "SPROGRAM"    "17" line
  | token == "readln"    = Token "readln"    "SREADLN"     "18" line
  | token == "then"      = Token "then"      "STHEN"       "19" line
  | token == "true"      = Token "true"      "STRUE"       "20" line
  | token == "var"       = Token "var"       "SVAR"        "21" line
  | token == "while"     = Token "while"     "SWHILE"      "22" line
  | token == "writeln"   = Token "writeln"   "SWRITELN"    "23" line
  | token == "="         = Token "="         "SEQUAL"      "24" line
  | token == "<>"        = Token "<>"        "SNOTEQUAL"   "25" line
  | token == "<"         = Token "<"         "SLESS"       "26" line
  | token == "<="        = Token "<="        "SLESSEQUAL"  "27" line
  | token == ">="        = Token ">="        "SGREATEQUAL" "28" line
  | token == ">"         = Token ">"         "SGREAT"      "29" line
  | token == "+"         = Token "+"         "SPLUS"       "30" line
  | token == "-"         = Token "-"         "SMINUS"      "31" line
  | token == "*"         = Token "*"         "SSTAR"       "32" line
  | token == "("         = Token "("         "SLPAREN"     "33" line
  | token == ")"         = Token ")"         "SRPAREN"     "34" line
  | token == "["         = Token "["         "SLBRACKET"   "35" line
  | token == "]"         = Token "]"         "SRBRACKET"   "36" line
  | token == ";"         = Token ";"         "SSEMICOLON"  "37" line
  | token == ":"         = Token ":"         "SCOLON"      "38" line
  | token == ".."        = Token ".."        "SRANGE"      "39" line
  | token == ":="        = Token ":="        "SASSIGN"     "40" line
  | token == ","         = Token ","         "SCOMMA"      "41" line
  | token == "."         = Token "."         "SDOT"        "42" line
  | otherwise            = Token token       "SIDENTIFIER" "43" line 

tkFile2Tokens :: FilePath -> IO [Token]
tkFile2Tokens tkFile = do
  tkLines <- lines <$> readFile tkFile
  return $ map tkLine2Token tkLines
  where
      tkLine2Token :: String -> Token
      tkLine2Token = (\(s:t:i:l:_) -> Token s t i l) . splitOn "\t"
