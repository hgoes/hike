module Language.Pike.CompileError where

import Language.Pike.Tokens
import Control.Monad.Error
import Language.Pike.Syntax

data CompileError
  = LexicalError Char Int Int
  | Expected String Token Int Int
  | UnknownError Token Int Int
  | GenericError String
  | TypeMismatch Expression Type Type
  | NotAFunction Expression Type
  | WrongNumberOfArguments Expression Int Int

instance Error CompileError where
  noMsg = GenericError "unknown error"
  strMsg = GenericError

instance Show CompileError where
    show (LexicalError ch l c) = "lexical error in line "++show l++", column "++show c++": unexpected "++show ch
    show (Expected tp tok l c) = "unexpected "++show tok++" in line "++show l++", column "++show c++": expected "++tp
    show (GenericError str) = "generic error: "++str
    show (UnknownError tok l c) = "unknown error in line "++show l++", column "++show c++" near "++show tok
    show (TypeMismatch expr tp exp) = "type error: expression "++show expr++" has type "++show tp++", but "++show exp++" is expected"
    show (NotAFunction expr tp) = "type error: expression "++show expr++" should be a function but has type "++show tp
    show (WrongNumberOfArguments expr got exp) = "expression "++show expr++" takes "++show exp++" arguments, but got "++show got