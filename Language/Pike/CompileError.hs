{-# LANGUAGE FlexibleInstances #-}
module Language.Pike.CompileError where

import Language.Pike.Tokens
import Control.Monad.Error
import Language.Pike.Syntax

data CompileError
  = LexicalError Char Int Int
  | Expected String Token Int Int
  | UnknownError Token Int Int
  | GenericError String
  | TypeMismatch Expression RType RType
  | NotAFunction Expression RType
  | WrongNumberOfArguments Expression Int Int
  | WrongReturnType Statement RType RType
  | LookupFailure ConstantIdentifier
  | NotAClass ConstantIdentifier
  | MisuseOfClass String String

instance Error CompileError where
  noMsg = GenericError "unknown error"
  strMsg = GenericError
  
instance Error [CompileError] where
  noMsg = [noMsg]
  strMsg str = [strMsg str]

instance Show CompileError where
    show (LexicalError ch l c) = "lexical error in line "++show l++", column "++show c++": unexpected "++show ch
    show (Expected tp tok l c) = "unexpected "++show tok++" in line "++show l++", column "++show c++": expected "++tp
    show (GenericError str) = "generic error: "++str
    show (UnknownError tok l c) = "unknown error in line "++show l++", column "++show c++" near "++show tok
    show (TypeMismatch expr tp exp) = "type error: expression "++show expr++" has type "++show tp++", but "++show exp++" is expected"
    show (NotAFunction expr tp) = "type error: expression "++show expr++" should be a function but has type "++show tp
    show (WrongNumberOfArguments expr got exp) = "expression "++show expr++" takes "++show exp++" arguments, but got "++show got
    show (WrongReturnType stmt got exp) = "statement "++show stmt++" returns type "++show got++", but "++show exp++" is expected"
    show (LookupFailure name) = "unknown identifier "++show name
    show (NotAClass name) = show name ++ " is not a class"
    show (MisuseOfClass reason name) = "a class ("++name++") cannot be used for "++reason