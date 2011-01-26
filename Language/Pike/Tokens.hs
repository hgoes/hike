module Language.Pike.Tokens where

data Token = Key Keyword
           | Identifier String
           | Semicolon
           | Dot
           | Comma
           | Bracket BracketType Bool
           | ConstString String
           | ConstInt Integer
           | Op Operator
           deriving Show

data Keyword = KeyArray
             | KeyElse
             | KeyIf
             | KeyImport
             | KeyInt
             | KeyPublic
             | KeyReturn
             | KeyString
             | KeyVoid
             deriving Show

data BracketType
    = Parenthesis
    | Square
    | Curly
    deriving Show

data Operator = OpAssign
              | OpAccess
              | OpEqual
              | OpPlus
                deriving Show