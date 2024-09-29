{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module AST where

import Token ( Token (..) )

type     AST                        =  AProgram
data     AProgram                   =  AProgram AProgramName ABlock ACompoundStatement
newtype  AProgramName               =  AProgramName AIdentifier
data     ABlock                     =  ABlock AVariableDeclaration ASubprogramDeclarations
newtype  AVariableDeclaration       =  AVariableDeclaration (Maybe AVariableDeclarations)
data     AVariableDeclarations      =  AVariableDeclarations  AVariableDeclarations' [AVariableDeclarations']
data     AVariableDeclarations'     =  AVariableDeclarations' AVariableNames AType
data     AVariableNames             =  AVariableNames AVariableName [AVariableName]
newtype  AVariableName              =  AVariableName AIdentifier
data     AType                      =  AStandard AStandardType | AArray AArrayType
newtype  AStandardType              =  AStandardType Token
data     AArrayType                 =  AArrayType AMinimumIndex AMaximumIndex AStandardType
newtype  AMinimumIndex              =  AMinimumIndex AInteger
newtype  AMaximumIndex              =  AMaximumIndex AInteger
data     AInteger                   =  AInteger (Maybe ASign) AUnsignedInteger
newtype  ASign                      =  ASign Token
newtype  ASubprogramDeclarations    =  ASubprogramDeclarations [ASubprogramDeclaration]
data     ASubprogramDeclaration     =  ASubprogramDeclaration ASubprogramHead AVariableDeclaration ACompoundStatement
data     ASubprogramHead            =  ASubprogramHead AProcedureName AParameterDeclaration
newtype  AProcedureName             =  AProcedureName AIdentifier
newtype  AParameterDeclaration      =  AParameter (Maybe AParameters)
data     AParameters                =  AParameters AParameters' [AParameters']
data     AParameters'               =  AParameters' AParameterNames AStandardType
data     AParameterNames            =  AParameterNames AParameterName [AParameterName]
newtype  AParameterName             =  AParameterName AIdentifier
newtype  ACompoundStatement         =  ACompoundStatement AStatements
data     AStatements                =  AStatements AStatement [AStatement]
data     AStatement                 =  ABasic ABasicStatemet | ABranch AIfStatement | ARepeat AWhileStatement
data     AIfStatement               =  AIfStatement AExpression ACompoundStatement (Maybe AElseStatement)
newtype  AElseStatement             =  AElseStatement ACompoundStatement
data     AWhileStatement            =  AWhileStatement AExpression ACompoundStatement
data     ABasicStatemet             =  AAssignment AAssignmentStatement | AProcedureCall AProcedureCallStatement | AIO AIOStatement | ACompound ACompoundStatement
data     AAssignmentStatement       =  AAssignmentStatement ALeftPart AExpression
newtype  ALeftPart                  =  ALeftPart AVariable
data     AVariable                  =  APure APureVariable | AIndexed AIndexedVariable
newtype  APureVariable              =  APureVariable AVariableName
data     AIndexedVariable           =  AIndexedVariable AVariableName AIndex
newtype  AIndex                     =  AIndex AExpression
data     AProcedureCallStatement    =  AProcedureCallStatement AProcedureName (Maybe AExpressions)
data     AExpressions               =  AExpressions AExpression [AExpression]
data     AExpression                =  AExpression ASimpleExpression (Maybe ARelationalOperation)
data     ARelationalOperation       =  ARelationalOperation ARelationalOperator ASimpleExpression
data     ASimpleExpression          =  ASimpleExpression (Maybe ASign) ATerm [AAdditionalOperation]
data     AAdditionalOperation       =  AAdditionalOperation AAdditionalOperator ATerm
data     ATerm                      =  ATerm AFactor [AMultiplicativeOperation]
data     AMultiplicativeOperation   =  AMultiplicativeOperation AMultiplicativeOperator AFactor
data     AFactor                    =  AVariableReference AVariable | AConstantReference AConstant | ARecursion AExpression | ANegation ANegationOperator AFactor
newtype  ANegationOperator          =  ANegationOperator Token
newtype  ARelationalOperator        =  ARelationalOperator Token
newtype  AAdditionalOperator        =  AAdditionalOperator Token
newtype  AMultiplicativeOperator    =  AMultiplicativeOperator Token
data     AIOStatement               =  AInputStatement (Maybe AVariables) | AOutputStatement (Maybe AExpressions)
data     AVariables                 =  AVariables AVariable [AVariable]
data     AConstant                  =  AIntegerLiteral AUnsignedInteger | AStringLiteral AString | ABooleanLiteral ABoolean | ACharacterLiteral ACharacter 
newtype  AUnsignedInteger           =  AUnsignedInteger Token
newtype  AString                    =  AString Token
newtype  ABoolean                   =  ABoolean Token
newtype  ACharacter                 =  ACharacter Token
newtype  AIdentifier                =  AIdentifier Token