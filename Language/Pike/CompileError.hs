module Language.Pike.CompileError where

import Language.Pike.Tokens
import Control.Monad.Error

data CompileError
  = LexicalError Char Int Int
  | Expected String Token Int Int
  | UnknownError Token Int Int
  | GenericError String

instance Error CompileError where
  noMsg = GenericError "unknown error"
  strMsg = GenericError

instance Show CompileError where
    show (LexicalError ch l c) = "lexical error in line "++show l++", column "++show c++": unexpected "++show ch
    show (Expected tp tok l c) = "unexpected "++show tok++" in line "++show l++", column "++show c++": expected "++tp
    show (GenericError str) = "generic error: "++str
    show (UnknownError tok l c) = "unknown error in line "++show l++", column "++show c++" near "++show tok