{
module Language.Pike.Parser where

import Language.Pike.Syntax
import Language.Pike.Tokens
import Language.Pike.Lexer
import Language.Pike.CompileError

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
  "break"                    { Key KeyBreak }
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
  "."                        { Dot }
  ":"                        { Colon }
  ";"                        { Semicolon }
  ","                        { Comma }
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

Definition : Modifiers DefinitionBody { Definition $1 $2 }

Modifiers : Modifier Modifiers { $1:$2 }
          |                    { [] }

Modifier : "public" { Public }

DefinitionBody : "import" ConstantIdentifier ";"         { Import $ Left $2 }
               | Type identifier "(" Arguments ")" Block { FunctionDef $2 $1 $4 $6 }

ConstantIdentifier : "." identifier ConstantIdentifierN { ConstId True ($2:$3) }
                   | identifier ConstantIdentifierN     { ConstId False ($1:$2) }

ConstantIdentifierN : "." identifier ConstantIdentifierN { $2:$3 }
                    |                                    { [] }

Type : "string"             { TypeString } 
     | "void"               { TypeVoid }
     | "int"                { TypeInt }
     | ConstantIdentifier   { TypeId $1 }
     | "array" "(" Type ")" { TypeArray $3 }

Arguments : Type identifier ArgumentsN { ($2,$1):$3 }
          |                            { [] }

ArgumentsN : "," Type identifier ArgumentsN { ($3,$2):$4 }
           |                                { [] }

Block : "{" Statements "}" { $2 }

Statements : Statement Statements { $1:$2 }
           |                      { [] }

Statement : Statement1 { $1 }
          | Statement2 { $1 }
          | error      {%^ expected "statement" }
--          | error      {%^ \tok -> do { (l,c) <- getPos ; throwError $ Expected "statement" tok l c } }

Statement2 : "if" "(" Expression ")" Statement                    { StmtIf $3 $5 Nothing }
           | "if" "(" Expression ")" Statement1 "else" Statement2 { StmtIf $3 $5 (Just $7) }

Statement1 : "{" Statements "}"                                                    { StmtBlock $2 }
           | Type identifier ";"                                                   { StmtDecl $2 $1 Nothing }
           | Type identifier "=" ExpressionE ";"                                   { StmtDecl $2 $1 (Just $4) }
           | Expression ";"                                                        { StmtExpr $1 }
           | "if" "(" Expression ")" Statement1 "else" Statement1                  { StmtIf $3 $5 (Just $7) }
           | "return" ExpressionE ";"                                              { StmtReturn (Just $2) }
           | "return" ";"                                                          { StmtReturn Nothing }
           | "for" "(" ExpressionOpt ";" ExpressionOpt ";" ExpressionOpt ")" Block { StmtFor $3 $5 $7 $9 }
           | "break" ";"                                                           { StmtBreak }

ExpressionE : Expression { $1 }
            | error      {%^ expected "expression" }

Expression : ConstantIdentifier                    { ExprId $1 }
           | Expression "(" ExprList ")"           { ExprCall $1 $3 }
           | const_string                          { ExprString $1 }
           | const_int                             { ExprInt $1 }
           | Expression "+" Expression             { ExprBin BinPlus $1 $3 }
           | Expression "==" Expression            { ExprBin BinEqual $1 $3 }
           | Expression "->" Expression            { ExprBin BinAccess $1 $3 }
           | Expression "<" Expression             { ExprBin BinLess $1 $3 }
           | ConstantIdentifier "=" Expression     { ExprAssign Assign $1 $3 }
           | Expression "[" Expression "]"         { ExprIndex $1 $3 }
           | "lambda" "(" Arguments ")" Statement  { ExprLambda $3 $5 }

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
}

