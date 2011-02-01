{-# LANGUAGE DoRec #-}
module Language.Pike.Compiler.Resolver where

import Language.Pike.Compiler.Error
import Language.Pike.Compiler.Stack
import Language.Pike.Syntax

import Data.Map as Map
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Data.List (mapAccumL)

type Resolver a p r = WriterT [CompileError p] (StateT (Integer,ClassMap) (Reader (Stack r))) a

type ClassMap = Map Integer ClassMapEntry

data ClassMapEntry = ClassMapEntry
                     { className       :: String
                     , classVariables  :: [(String,RType)]
                     , classMethods    :: Map String (RType,[RType])
                     , classInners     :: Map String Integer
                     } deriving Show

resolve :: [Definition p] -> Either [CompileError p] (Stack r,ClassMap)
resolve defs = let res = GlobalContext (Map.fromList vars) funcs cls
                   (((vars,funcs,cls),errs),(_,mp)) = runReader (runStateT (runWriterT (resolveBody defs)) (0,Map.empty)) res
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

resolveBody :: [Definition p] -> Resolver ([(String,RType)],Map String (RType,[RType]),Map String Integer) p r
resolveBody [] = return ([],Map.empty,Map.empty)
resolveBody (def:defs) = do
  (m1,f1,c1) <- resolveBodyDef def
  (m2,f2,c2) <- resolveBody defs
  return (m1++m2,Map.union f1 f2,Map.union c1 c2)
    
resolveBodyDef :: Definition p -> Resolver ([(String,RType)],Map String (RType,[RType]),Map String Integer) p r
resolveBodyDef (Definition _ body _) = case body of
  VariableDef tp names -> do
    rtp <- resolveType tp
    return ([(name,rtp) | name <- names],Map.empty,Map.empty)
  ClassDef name args body -> do
    (num,cm) <- get
    rec { put (num+1,Map.insert num (ClassMapEntry name members methods cls) cm)
        ; (members,methods,cls) <- local (\st -> ClassContext name num members methods cls st) $ resolveBody body
        }
    return ([],Map.empty,Map.singleton name num)
  FunctionDef name rtp args body -> do
    rtp' <- resolveType rtp
    targs <- mapM (\(_,tp) -> resolveType tp) args
    return ([],Map.singleton name (rtp',targs),Map.empty)
  Import _ -> tell [ImportInsideClass] >> return ([],Map.empty,Map.empty)
