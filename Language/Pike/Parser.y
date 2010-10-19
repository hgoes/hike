{
module Language.Pike.Parser where

import Language.Pike.Syntax
import Language.Pike.Tokens
}

%name pike
%tokentype { Token }
%error { parseError }

%token
  identifier                 { Identifier $$ }
  const_string               { ConstString $$ }
  const_int                  { ConstInt $$ }
  "array"                    { Key KeyArray }
  "else"                     { Key KeyElse }
  "if"                       { Key KeyIf }
  "import"                   { Key KeyImport }
  "int"                      { Key KeyInt }
  "public"                   { Key KeyPublic }
  "return"                   { Key KeyReturn }
  "string"                   { Key KeyString }
  "void"                     { Key KeyVoid }
  "."                        { Dot }
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

%left "->"
%left "["
%left "=="
%left "+"
%left "="
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

Statements : Statement  Statements { $1:$2 }
           |                       { [] }

Statement : Type identifier ";"                         { StmtDecl $2 $1 Nothing }
          | Type identifier "=" Expression ";"          { StmtDecl $2 $1 (Just $4) }
          | Expression ";"                              { StmtExpr $1 }
          | "if" "(" Expression ")" Block ElseBlock     { StmtIf $3 $5 $6 }
          | "return" Expression ";"                     { StmtReturn (Just $2) }
          | "return" ";"                                { StmtReturn Nothing }

ElseBlock : "else" Block { Just $2 }
          |              { Nothing }

Expression : ConstantIdentifier                    { ExprId $1 }
           | identifier "(" ExprList ")"           { ExprCall $1 $3 }
           | const_string                          { ExprString $1 }
           | const_int                             { ExprInt $1 }
           | Expression "+" Expression             { ExprBin BinPlus $1 $3 }
           | Expression "==" Expression            { ExprBin BinEqual $1 $3 }
           | Expression "->" Expression            { ExprBin BinAccess $1 $3 }
           | ConstantIdentifier "=" Expression     { ExprAssign Assign $1 $3 }
           | Expression "[" Expression "]"         { ExprIndex $1 $3 }

ExprList : Expression ExprListN { $1:$2 }
         |                      { [] }

ExprListN : "," Expression ExprListN { $2:$3 }
          |                          { [] }

AssignType : "=" { Assign }

{
parseError xs = error ("Parse error at "++show (take 5 xs))
}

