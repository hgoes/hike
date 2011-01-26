{
module Language.Pike.Lexer where

import Language.Pike.Tokens
}

%wrapper "basic"

tokens :-
  $white+                 ;
  array                   { key KeyArray }
  else                    { key KeyElse }
  if                      { key KeyIf }
  import                  { key KeyImport }
  int                     { key KeyInt }
  public                  { key KeyPublic }
  return                  { key KeyReturn }
  string                  { key KeyString }
  void                    { key KeyVoid }
  ";"                     { const Semicolon }
  "."                     { const Dot }
  ","                     { const Comma }
  "("                     { const (Bracket Parenthesis False) }
  ")"                     { const (Bracket Parenthesis True) }
  "["                     { const (Bracket Square False) }
  "]"                     { const (Bracket Square True) }
  "{"                     { const (Bracket Curly False) }
  "}"                     { const (Bracket Curly True) }
  "=="                    { op OpEqual }
  "="                     { op OpAssign }
  "->"                    { op OpAccess }
  "+"                     { op OpPlus }
  [a-zA-Z] [a-zA-Z0-9\_]* { \s -> Identifier s }
  [0-9]+                  { \s -> ConstInt $ read s }
  \" (. # \")* \"         { \s -> ConstString $ read s }
{
key :: Keyword -> String -> Token
key w _ = Key w

op :: Operator -> String -> Token
op w _ = Op w
}