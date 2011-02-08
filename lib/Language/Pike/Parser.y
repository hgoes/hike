{
module Language.Pike.Parser where

import Language.Pike.Syntax
import Language.Pike.Tokens
import Language.Pike.Lexer
import Language.Pike.Compiler.Error

import Control.Monad.Error
}

%name pike
%monad { Lexer }
%lexer { pikeLexer } { EOF }
%tokentype { Token }
%error { parseError }

%token
  identifier                 { Identifier $$ }
  const_string               { ConstString $$ }
  const_int                  { ConstInt $$ }
  "array"                    { Key KeyArray }
  "bool"                     { Key KeyBool }
  "break"                    { Key KeyBreak }
  "class"                    { Key KeyClass }
  "else"                     { Key KeyElse }
  "for"                      { Key KeyFor }
  "if"                       { Key KeyIf }
  "import"                   { Key KeyImport }
  "int"                      { Key KeyInt }
  "lambda"                   { Key KeyLambda }
  "public"                   { Key KeyPublic }
  "return"                   { Key KeyReturn }
  "string"                   { Key KeyString }
  "void"                     { Key KeyVoid }
  "while"                    { Key KeyWhile }
  "."                        { Dot }
  ":"                        { Colon }
  ";"                        { Semicolon }
  ","                        { Comma }
  "({"                       { Bracket ArrayDelim False }
  "})"                       { Bracket ArrayDelim True }
  "("                        { Bracket Parenthesis False }
  ")"                        { Bracket Parenthesis True }
  "{"                        { Bracket Curly False }
  "}"                        { Bracket Curly True }
  "["                        { Bracket Square False }
  "]"                        { Bracket Square True }
  "+"                        { Op OpPlus }
  "="                        { Op OpAssign }
  "=="                       { Op OpEqual }
  "->"                       { Op OpAccess }
  "<"                        { Op OpLess }

%left "="
%left "<"
%left "->"
%left "["
%left "=="
%left "+"
%left "("

%%

Definitions : Definition Definitions { $1:$2 }
            |                        { [] }

Definition : Modifiers DefinitionBody {% do { pos <- getPos ; return $ Definition $1 $2 pos } }

Modifiers : Modifier Modifiers { $1:$2 }
          |                    { [] }

Modifier : "public" { Public }

DefinitionBody : "import" ConstantIdentifier ";"                     { Import $ Left $2 }
               | Type identifier "(" Arguments ")" Block             { FunctionDef $2 $1 $4 $6 }
               | "class" identifier ArgumentsOpt "{" Definitions "}" { ClassDef $2 $3 $5 }
               | Type identifier IdentifiersN ";"                    { VariableDef $1 ($2:$3) }

ConstantIdentifier : "." identifier ConstantIdentifierN { ConstId True ($2:$3) }
                   | identifier ConstantIdentifierN     { ConstId False ($1:$2) }

ConstantIdentifierN : "." identifier ConstantIdentifierN { $2:$3 }
                    |                                    { [] }

IdentifiersN : "," identifier IdentifiersN { $2:$3 }
             |                             { [] }

Type : "string"             { TypeString } 
     | "void"               { TypeVoid }
     | "int"                { TypeInt }
     | "bool"               { TypeBool }
     | ConstantIdentifier   { TypeId $1 }
     | "array" "(" Type ")" { TypeArray $3 }

Arguments : Type identifier ArgumentsN { ($2,$1):$3 }
          |                            { [] }

ArgumentsN : "," Type identifier ArgumentsN { ($3,$2):$4 }
           |                                { [] }

ArgumentsOpt : "(" Arguments ")" { $2 }
             |                   { [] }

Block : "{" Statements "}" { $2 }

Statements : Statement Statements { $1:$2 }
           |                      { [] }

StatementOpt : Statement { Just $1 }
             |           { Nothing }

Statement : Statement1 { $1 }
          | Statement2 { $1 }
          | error      {%^ expected "statement" }
--          | error      {%^ \tok -> do { (l,c) <- getPos ; throwError $ Expected "statement" tok l c } }

Statement2 : Statement2NP {% do { pos <- getPos ; return $ Pos $1 pos } }

Statement2NP : "if" "(" Expression ")" Statement                    { StmtIf $3 $5 Nothing }
             | "if" "(" Expression ")" Statement1 "else" Statement2 { StmtIf $3 $5 (Just $7) }


Statement1 : Statement1NP {% do { pos <- getPos ; return $ Pos $1 pos } }

Statement1NP : "{" Statements "}"                                                    { StmtBlock $2 }
             | Type identifier ";"                                                   { StmtDecl $2 $1 Nothing }
             | Type identifier "=" ExpressionE ";"                                   { StmtDecl $2 $1 (Just $4) }
             | Expression ";"                                                        { StmtExpr $1 }
             | "if" "(" Expression ")" Statement1 "else" Statement1                  { StmtIf $3 $5 (Just $7) }
             | "return" ExpressionE ";"                                              { StmtReturn (Just $2) }
             | "return" ";"                                                          { StmtReturn Nothing }
             | "while" "(" Expression ")" Block                                      { StmtWhile $3 $5 }
             | "for" "(" ExprOrDecl ";" ExpressionOpt ";" ExpressionOpt ")" Block    { StmtFor $3 $5 $7 $9 }
             | "break" ";"                                                           { StmtBreak }

ExprOrDecl : Expression                       { Just $ Left $1 }
           | Type identifier "=" ExpressionE  { Just $ Right ($1,$2,$4) }
           |                                  { Nothing }

ExpressionE : Expression { $1 }
            | error      {%^ expected "expression" }

--Expression : ExpressionNP {% do { pos <- getPos ; return $ Pos $1 pos } }

Expression : ExpressionAssign { $1 }

ExpressionAssign : LValue "=" ExpressionAssign {% positional $ ExprAssign Assign $1 $3 }
                 | ExpressionOp                { $1 }

ExpressionOp : ExpressionSimple "+" ExpressionOp     {% positional $ ExprBin BinPlus $1 $3 }
             | ExpressionSimple "==" ExpressionOp    {% positional $ ExprBin BinEqual $1 $3 }
             | ExpressionSimple "<" ExpressionOp     {% positional $ ExprBin BinLess $1 $3 }
             | ExpressionSimple                      { $1 }

ExpressionSimple : ConstantIdentifier                    {% positional $ ExprId $1 }
                 | ExpressionSimple "(" ExprList ")"     {% positional $ ExprCall $1 $3 }
                 | const_string                          {% positional $ ExprString $1 }
                 | const_int                             {% positional $ ExprInt $1 }
                 | ExpressionSimple "->" identifier      {% positional $ ExprAccess $1 $3 }
                 | ExpressionSimple "[" Expression "]"   {% positional $ ExprIndex $1 $3 }
                 | "lambda" "(" Arguments ")" Statement  {% positional $ ExprLambda $3 $5 }
                 | "({" ExprList "})"                    {% positional $ ExprArray $2 }

LValue : ExpressionSimple { $1 }
--LValue : ConstantIdentifier     { LVId $1 }
--       | LValue "->" identifier { LVAccess $1 $3 }


ExpressionOpt : Expression { Just $1 }
              |            { Nothing }

ExprList : Expression ExprListN { $1:$2 }
         |                      { [] }

ExprListN : "," Expression ExprListN { $2:$3 }
          |                          { [] }

AssignType : "=" { Assign }

{
parseError xs = do
  (l,c) <- getPos
  throwError (UnknownError xs l c)

expected :: String -> Token -> Lexer a
expected tp tok = do
  (l,c) <- getPos
  throwError (Expected tp tok l c)

--positional :: a -> Lexer (Pos a
positional x = do
  p <- getPos
  return $ Pos x p
}

