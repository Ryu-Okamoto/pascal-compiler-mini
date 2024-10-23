{-# LANGUAGE StandaloneDeriving #-}

module AST.StrictLL1 where

{-
  definition of EBNF that can express strict-LL1 grammer
  where a grammer G is strict-LL1 only if G has
    1. no body that has optional consecutive variables such as V ::= [ Va Vb ], where V, Va, Vb are variables
    2. no variable whose any body does not share first elements with others (then, G is called LL1)
  
  definition of EBNF expressing strict-LL1 by using EBNF
  ----------------------------------------------------------------------
    EBNF     ::= EBNFL "." { EBNFL "." }
    EBNFL    ::= Head "::=" Body { "|" Body }
    Head     ::= Variable
    Body     ::= Element { Element }
    Element  ::= Terminal | Variable | [ Element ] | { Element }
    Variable ::= CAPS [ String ]
    Terminal ::= \"String\"
    CAPS     ::= "A" | "B" | ... | "Z"
    String   ::= ASCII { ASCII }
  ----------------------------------------------------------------------
-}

data    EBNF     = EBNF EBNFL [EBNFL] 
data    EBNFL    = EBNFL Head Body [Body] 
newtype Head     = Head Variable 
data    Body     = Body Element [Element] 
data    Element  = AtomT Terminal | AtomV Variable | Optional Element | Repeat Element 
newtype Variable = Variable String 
newtype Terminal = Terminal String 

deriving instance Show EBNF
deriving instance Show EBNFL
deriving instance Show Head
deriving instance Show Body
deriving instance Show Element
deriving instance Eq Element
deriving instance Show Variable
deriving instance Eq Variable
deriving instance Show Terminal
deriving instance Eq Terminal