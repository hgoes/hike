module Language.Pike.Tokens where

data Token = Key Keyword
           | Identifier String
           | Colon
           | Semicolon
           | Dot
           | Comma
           | Bracket BracketType Bool
           | ConstString String
           | ConstInt Integer
           | Op Operator
           | EOF
--           deriving Show

instance Show Token where
    show (Key w) = "keyword "++show w
    show (Identifier x) = "identifier "++show x
    show Colon = "':'"
    show Semicolon = "';'"
    show Comma = "','"
    show (Bracket Parenthesis False) = "'('"
    show (Bracket Parenthesis True) = "')'"
    show (Bracket Square False) = "'['"
    show (Bracket Square True) = "']'"
    show (Bracket Curly False) = "'{'"
    show (Bracket Curly True) = "'}'"
    show (Bracket ArrayDelim False) = "'({'"
    show (Bracket ArrayDelim True) = "'})'"
    show (ConstString str) = "string "++show str
    show (ConstInt i) = "integer "++show i
    show (Op x) = "operator "++show x
    show EOF = "end of file"
    

data Keyword = KeyArray
             | KeyBool
             | KeyBreak
             | KeyClass
             | KeyElse
             | KeyFor
             | KeyIf
             | KeyImport
             | KeyInt
             | KeyLambda
             | KeyPublic
             | KeyReturn
             | KeyString
             | KeyVoid
             | KeyWhile
             deriving Show

data BracketType
    = Parenthesis
    | Square
    | Curly
    | ArrayDelim
    deriving Show

data Operator = OpAssign
              | OpAccess
              | OpEqual
              | OpPlus
              | OpMinus
              | OpLess
              | OpLessEq

instance Show Operator where
  show OpAssign = "'='"
  show OpAccess = "'->'"
  show OpEqual  = "'=='"
  show OpPlus   = "'+'"
  show OpMinus  = "'-'"
  show OpLess   = "'<'"
  show OpLessEq = "'<='"