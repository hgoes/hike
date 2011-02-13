{-# LANGUAGE DoRec #-}
module Language.Pike.Compiler.Resolver where

import Language.Pike.Compiler.Error
import Language.Pike.Compiler.Stack hiding (className,classMethods,classInherits)
import Language.Pike.Syntax

import Data.Map as Map
import Data.Set as Set
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Data.List (mapAccumL)

type Resolver a p r = WriterT [CompileError p] (StateT (Integer,ClassMap) (Reader (Stack r))) a

type ClassMap = Map Integer ClassMapEntry

data ClassMapEntry = ClassMapEntry
                     { className       :: String
                     , classVariables  :: [(String,RType)]
                     , classMethods    :: [(String,(RType,[RType]))]
                     , classInners     :: Map String Integer
                     , classInherits   :: Set Integer
                     } deriving Show

resolve :: [Definition p] -> Either [CompileError p] (Stack r,ClassMap)
resolve defs = let res = GlobalContext (Map.fromList vars) (Map.fromList funcs) cls
                   (((vars,funcs,cls,inh),errs),(_,mp)) = runReader (runStateT (runWriterT (resolveBody defs)) (0,Map.empty)) res
               in case errs of
                 [] -> Right (res,mp)
                 _ -> Left errs

resolveType :: Type -> Resolver RType p r
resolveType (TypeId name) = do
  st <- ask
  let (t,res) = case stackLookup name st of
        Nothing -> ([LookupFailure name Nothing],undefined)
        Just (ref,lvl) -> case ref of
          Class n -> ([],TypeId n)
          _ -> ([NotAClass name],undefined)
  tell t
  return res
resolveType x = return $ fmap (const undefined) x -- This is a brutal hack

resolveBody :: [Definition p] -> Resolver ([(String,RType)],[(String,(RType,[RType]))],Map String Integer,Set Integer) p r
resolveBody [] = return ([],[],Map.empty,Set.empty)
resolveBody (def:defs) = do
  (m1,f1,c1,i1) <- resolveBodyDef def
  (m2,f2,c2,i2) <- resolveBody defs
  return (m1++m2,f1++f2,Map.union c1 c2,Set.union i1 i2)
    
resolveBodyDef :: Definition p -> Resolver ([(String,RType)],[(String,(RType,[RType]))],Map String Integer,Set Integer) p r
resolveBodyDef (Definition _ body _) = case body of
  VariableDef tp names -> do
    rtp <- resolveType tp
    return ([(name,rtp) | name <- names],[],Map.empty,Set.empty)
  ClassDef name args body -> do
    (num,cm) <- get
    rec { put (num+1,Map.insert num (ClassMapEntry { className = name
                                                   , classVariables = members
                                                   , classMethods = methods
                                                   , classInners = cls
                                                   , classInherits = inh
                                                   }) cm)
        ; (members,methods,cls,inh) <- local (\st -> ClassContext name num members (Map.fromList methods) cls inh st) $ resolveBody body
        }
    return ([],[],Map.singleton name num,Set.empty)
  FunctionDef name rtp args body -> do
    rtp' <- resolveType rtp
    targs <- mapM (\(_,tp) -> resolveType tp) args
    return ([],[(name,(rtp',targs))],Map.empty,Set.empty)
  Import _ -> tell [ImportInsideClass] >> return ([],[],Map.empty,Set.empty)
  Inherit name -> do
    ~(TypeId cid) <- resolveType (TypeId name)
    return ([],[],Map.empty,Set.singleton cid)
