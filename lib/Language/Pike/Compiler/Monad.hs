module Language.Pike.Compiler.Monad where

import Language.Pike.Compiler.Error
import Language.Pike.Compiler.Stack
import Language.Pike.Compiler.Resolver

import Data.Map as Map
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import Llvm.AbsSyn
import Llvm.Types

type Compiler a p = ErrorT [CompileError p] (StateT (Integer,Stack LlvmVar,Map String Integer) (WriterT [LlvmFunction] (Reader ClassMap))) a

newLabel :: Compiler Integer p
newLabel = do
  (x,st,strs) <- get
  put (x+1,st,strs)
  return x

runCompiler :: ClassMap -> Stack LlvmVar -> Compiler a p -> Either [CompileError p] a
runCompiler mp st c = fst $ runReader (runWriterT $ evalStateT (runErrorT c) (0,st,Map.empty)) mp