{-# LANGUAGE FlexibleInstances #-}
module Language.Pike.Compiler.Error where

import Language.Pike.Tokens
import Control.Monad.Error
import Language.Pike.Syntax

data CompileError p
  = LexicalError Char Int Int
  | Expected String Token Int Int
  | UnknownError Token Int Int
  | GenericError String
  | TypeMismatch (Expression p) RType RType
  | NotAFunction (Expression p) RType
  | WrongNumberOfArguments (Expression p) Int Int
  | WrongReturnType (Statement p) RType RType
  | LookupFailure ConstantIdentifier (Maybe p)
  | NotAClass ConstantIdentifier
  | MisuseOfClass String String
  | NotImplemented String
  | ImportInsideClass
  | InternalError String
  | UninitializedVariable p ConstantIdentifier
  | NothingToBreakTo p
  | NoSuchMember p String String

instance Error (CompileError p) where
  noMsg = GenericError "unknown error"
  strMsg = GenericError
  
instance Error [CompileError p] where
  noMsg = [noMsg]
  strMsg str = [strMsg str]

instance Show p => Show (CompileError p) where
    show (LexicalError ch l c) = "lexical error in line "++show l++", column "++show c++": unexpected "++show ch
    show (Expected tp tok l c) = "unexpected "++show tok++" in line "++show l++", column "++show c++": expected "++tp
    show (GenericError str) = "generic error: "++str
    show (UnknownError tok l c) = "unknown error in line "++show l++", column "++show c++" near "++show tok
    show (TypeMismatch expr tp exp) = "type error: expression "++show expr++" has type "++show tp++", but "++show exp++" is expected"
    show (NotAFunction expr tp) = "type error: expression "++show expr++" should be a function but has type "++show tp
    show (WrongNumberOfArguments expr got exp) = "expression "++show expr++" takes "++show exp++" arguments, but got "++show got
    show (WrongReturnType stmt got exp) = "statement "++show stmt++" returns type "++show got++", but "++show exp++" is expected"
    show (LookupFailure name pos) = "unknown identifier "++show name++(case pos of
                                                                          Just rpos -> " at "++show rpos
                                                                          Nothing -> "")
    show (NotAClass name) = show name ++ " is not a class"
    show (MisuseOfClass reason name) = "a class ("++name++") cannot be used for "++reason
    show (NotImplemented what) = "The following feature is yet unimplemented: "++what
    show ImportInsideClass = "Import definitions inside classes are not allowed"
    show (InternalError str) = "Internal compiler error: "++str
    show (UninitializedVariable pos var) = "Variable "++show var++" is used unintialized at "++show pos
    show (NothingToBreakTo pos) = "Nothing to break to at "++show pos
    show (NoSuchMember pos cls name) = "class "++cls++" has no member "++name++" at "++show pos