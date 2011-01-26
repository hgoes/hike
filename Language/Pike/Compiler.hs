{-# LANGUAGE TupleSections, DoRec #-}
module Language.Pike.Compiler where

import Language.Pike.Syntax
import Language.Pike.CompileError
import Llvm
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (mapMaybe,catMaybes)
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.Reader
import Data.List (find)
import Data.Map as Map hiding (map,mapMaybe)
import qualified Data.Map as Map (mapMaybe,map)
import Data.Set as Set hiding (map)
import qualified Data.Set as Set

type Compiler a = ErrorT [CompileError] (StateT ([Integer],Stack) (WriterT [LlvmFunction] (Reader ClassMap))) a

type Resolver a = WriterT [CompileError] (StateT ([Integer],ClassMap) (Reader Stack)) a

data StackReference = Pointer RType
                    | Variable RType LlvmVar
                    | Function RType [RType]
                    | Class Integer
                    deriving (Show,Eq)

type Stack = [Map String (BS.ByteString,StackReference)]

type ClassMap = Map Integer (BS.ByteString,Map String (BS.ByteString,StackReference))

resolve :: [Definition] -> Either [CompileError] (Map String (BS.ByteString,StackReference),ClassMap)
resolve defs = let ((res,errs),(_,mp)) = runReader (runStateT (runWriterT (resolveBody defs)) ([0..],Map.empty)) []
               in case errs of
                 [] -> Right (res,mp)
                 _ -> Left errs

resolveType :: Type -> Resolver RType
resolveType (TypeId name) = do
  st <- ask
  let (t,res) = case stackLookup' name st of
        Nothing -> ([LookupFailure name],undefined)
        Just (_,ref) -> case ref of
          Class n -> ([],TypeId n)
          _ -> ([NotAClass name],undefined)
  tell t
  return res
resolveType x = return $ fmap (const undefined) x -- This is a brutal hack

translateType :: Type -> Compiler RType
translateType (TypeId name) = do
  (_,ref) <- stackLookup name
  case ref of
    Class n -> return (TypeId n)
    _ -> throwError [NotAClass name]
translateType x = return $ fmap (const undefined) x
    

resolveDef :: Definition -> Resolver (Map String (BS.ByteString,StackReference))
resolveDef (Definition _ body) = case body of
  VariableDef tp names -> do
    rtp <- resolveType tp
    return $ Map.fromList $ map (\name -> (name,(BS.pack name,Pointer rtp))) names
  ClassDef name args body -> do
    rec { rbody <- local (rbody:) $ resolveBody body }
    (uniq,mp) <- get
    put (tail uniq,Map.insert (head uniq) (BS.pack name,rbody) mp)
    return $ Map.singleton name (BS.pack name,Class (head uniq))
  FunctionDef name rtp args body -> do
    rtp' <- resolveType rtp
    targs <- mapM (\(_,tp) -> resolveType tp) args
    return $ Map.singleton name (BS.pack name,Function rtp' targs)
  Import _ -> return Map.empty

resolveBody :: [Definition] -> Resolver (Map String (BS.ByteString,StackReference))
resolveBody [] = return Map.empty
resolveBody (def:defs) = do
  refs1 <- resolveDef def
  refs2 <- resolveBody defs
  return (Map.union refs1 refs2)

stackAlloc' :: String -> RType -> Stack -> Stack
stackAlloc' name tp []     = [Map.singleton name (BS.pack name,Pointer tp)]
stackAlloc' name tp (x:xs) = (Map.insert name (BS.pack name,Pointer tp) x):xs

stackAlloc :: String -> RType -> Compiler ()
stackAlloc name tp = modify $ \(uniq,st) -> (uniq,stackAlloc' name tp st)

stackLookup' :: ConstantIdentifier -> Stack -> Maybe (BS.ByteString,StackReference)
stackLookup' s [] = Nothing
stackLookup' s@(ConstId _ (name:_)) (x:xs) = case Map.lookup name x of
  Nothing -> stackLookup' s xs
  Just ref -> Just ref

stackLookup :: ConstantIdentifier -> Compiler (BS.ByteString,StackReference)
stackLookup s = do
  (_,st) <- get
  case stackLookup' s st of
    Nothing -> error $ "Couldn't lookup "++show s
    Just res -> return res

stackAdd :: Map String (BS.ByteString,StackReference) -> Compiler ()
stackAdd refs = modify $ \(uniq,st) -> (uniq,refs:st)

stackPush :: Compiler ()
stackPush = modify $ \(uniq,st) -> (uniq,Map.empty:st)

stackPop :: Compiler ()
stackPop = modify $ \(uniq,st) -> (uniq,tail st)

stackShadow :: Compiler a -> Compiler a
stackShadow comp = do
  (uniq,st) <- get
  put (uniq,[])
  res <- comp
  (nuniq,_) <- get
  put (nuniq,st)
  return res

stackPut :: String -> StackReference -> Compiler ()
stackPut name ref = do
  (uniq,st) <- get
  let nst = case st of
        [] -> [Map.singleton name (BS.pack (name++show (head uniq)),ref)]
        x:xs -> (Map.insert name (BS.pack (name++show (head uniq)),ref) x):xs
  put (tail uniq,nst)

stackDiff :: Stack -> Stack -> [Map String (StackReference,StackReference)]
stackDiff st1 st2 = reverse $ stackDiff' (reverse st1) (reverse st2)
  where
    stackDiff' :: Stack -> Stack -> [Map String (StackReference,StackReference)]
    stackDiff' (x:xs) (y:ys) = (Map.mapMaybe id $ 
                                Map.intersectionWith (\(_,l) (_,r) -> if l==r
                                                                      then Nothing
                                                                      else Just (l,r)) x y):(stackDiff' xs ys)
    stackDiff' [] ys = []
    stackDiff' xs [] = []

{-stackDiff :: Compiler a -> Compiler (a,[Map String (StackReference,StackReference)])
stackDiff comp = do
  (_,st1) <- get
  res <- comp
  (_,st2) <- get
  return (res,reverse $ stackDiff' (reverse st1) (reverse st2))-}

newLabel :: Compiler Integer
newLabel = do
  (x:xs,st) <- get
  put (xs,st)
  return x

runCompiler :: ClassMap -> Compiler a -> Either [CompileError] a
runCompiler mp c = fst $ runReader (runWriterT $ evalStateT (runErrorT c) ([0..],[])) mp

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = mapM f xs >>= return.catMaybes

typeCheck :: Expression -> Maybe RType -> RType -> Compiler ()
typeCheck expr Nothing act = return ()
typeCheck expr (Just req@(TypeFunction TypeVoid args)) act@(TypeFunction _ eargs)
  | args == eargs = return ()
  | otherwise    = throwError [TypeMismatch expr act req]
typeCheck expr (Just req) act
  | req == act = return ()
  | otherwise = throwError [TypeMismatch expr act req]

compilePike :: [Definition] -> Either [CompileError] LlvmModule
compilePike defs = case resolve defs of
  Left errs -> Left errs
  Right (st,mp) -> runCompiler mp $ do
    ((f1,aliases),f2) <- listen $ do
      alias <- generateAliases
      stackAdd st
      funcs <- mapMaybeM (\(Definition mods x) -> case x of
                             FunctionDef name ret args block -> compileFunction name ret args block >>= return.Just
                             _ -> return Nothing) defs
      stackPop
      return (funcs,alias)
    return $  LlvmModule { modComments = []
                         , modAliases = aliases
                         , modGlobals = []
                         , modFwdDecls = []
                         , modFuncs = f2++f1
                         }

generateAliases :: Compiler [LlvmAlias]
generateAliases = do
  mp <- ask
  mapM (\(_,(name,body)) -> do
           struct <- mapMaybeM (\(_,(name,ref)) -> case ref of
                                   Pointer tp -> do
                                     rtp <- toLLVMType tp
                                     return $ Just rtp
                                   _ -> return Nothing
                               ) (Map.toList body)
           return (name,LMStruct struct)
       ) (Map.toList mp)

compileFunction :: String -> Type -> [(String,Type)] -> [Statement] -> Compiler LlvmFunction
compileFunction name ret args block = do
  ret_tp <- translateType ret
  rargs <- mapM (\(name,tp) -> do
                    rtp <- translateType tp
                    ltp <- toLLVMType rtp
                    return (name,rtp,ltp)) args
  decl <- genFuncDecl (BS.pack name) ret_tp [tp | (_,tp,_) <- rargs]
  stackAdd $ Map.fromList [(argn,(BS.pack argn,Variable argtp (LMNLocalVar (BS.pack argn) ltp))) | (argn,argtp,ltp) <- rargs]
  blks <- compileBody block ret_tp
  stackPop
  return $ LlvmFunction { funcDecl = decl
                        , funcArgs = [BS.pack name | (name,tp) <- args]
                        , funcAttrs = []
                        , funcSect = Nothing
                        , funcBody = blks
                        }

genFuncDecl :: BS.ByteString -> RType -> [RType] -> Compiler LlvmFunctionDecl
genFuncDecl name ret_tp args = do
  rret_tp <- toLLVMType ret_tp
  rargs <- mapM (\tp -> toLLVMType tp >>= return.(,[])) args
  return $ LlvmFunctionDecl { decName = name
                            , funcLinkage = External
                            , funcCc = CC_Fastcc
                            , decReturnType = rret_tp
                            , decVarargs = FixedArgs
                            , decParams = rargs
                            , funcAlign = Nothing
                            }

compileBody :: [Statement] -> RType -> Compiler [LlvmBlock]
compileBody stmts rtp = do
  (blks,nrtp) <- compileStatements stmts [] Nothing (Just rtp)
  return $ readyBlocks blks

readyBlocks :: [LlvmBlock] -> [LlvmBlock]
readyBlocks = reverse.map (\blk -> blk { blockStmts = case blockStmts blk of
                                            [] -> [Unreachable]
                                            _ -> reverse (blockStmts blk) 
                                       })

appendStatements :: [LlvmStatement] -> [LlvmBlock] -> Compiler [LlvmBlock]
appendStatements [] [] = return []
appendStatements stmts [] = do
  lbl <- newLabel
  return [LlvmBlock { blockLabel = LlvmBlockId lbl
                    , blockStmts = stmts
                    }]
appendStatements stmts (x:xs) = return $ x { blockStmts = stmts ++ (blockStmts x) }:xs

compileStatements :: [Statement] -> [LlvmBlock] -> Maybe Integer -> Maybe RType -> Compiler ([LlvmBlock],Maybe RType)
compileStatements [] blks _ rtp = return (blks,rtp)
compileStatements (x:xs) blks brk rtp = do
  (stmts,nblks,nrtp) <- compileStatement x brk rtp
  nblks2 <- appendStatements stmts blks
  compileStatements xs (nblks ++ nblks2) brk nrtp

compileStatement :: Statement -> Maybe Integer -> Maybe RType -> Compiler ([LlvmStatement],[LlvmBlock],Maybe RType)
compileStatement (StmtBlock stmts) brk rtp = do
  stackPush
  (blks,nrtp) <- compileStatements stmts [] brk rtp
  stackPop
  return ([],blks,nrtp)
compileStatement (StmtDecl name tp expr) _ rtp = do
  tp2 <- translateType tp
  --stackAlloc name tp2
  rtp' <- toLLVMType tp2
  case expr of
    Nothing -> do
      var <- defaultValue tp2
      stackPut name (Variable tp2 var)
      return ([],[],rtp)
    Just rexpr -> do
      (extra,res,_) <- compileExpression rexpr (Just tp2)
      stackPut name (Variable tp2 res)
      return (extra,[],rtp)
compileStatement st@(StmtReturn expr) _ rtp = case expr of
  Nothing -> case rtp of
    Nothing -> return ([Return Nothing],[],Just TypeVoid)
    Just rrtp
      | rrtp == TypeVoid -> return ([Return Nothing],[],Just TypeVoid)
      | otherwise -> throwError [WrongReturnType st TypeVoid rrtp]
  Just rexpr -> do
    (extra,res,rrtp) <- compileExpression rexpr rtp
    return ([Return (Just res)]++extra,[],Just rrtp)
compileStatement (StmtWhile cond body) _ rtp = compileWhile cond body rtp Nothing
compileStatement (StmtFor e1 e2 e3 body) _ rtp = do
  (init_stmts,init_blks,nrtp) <- case e1 of
    Nothing -> return ([],[],rtp)
    Just r1 -> compileStatement (StmtExpr r1) Nothing rtp
  init_blks' <- appendStatements init_stmts init_blks
  let lbl_start = case init_blks' of
        [] -> Nothing
        (LlvmBlock (LlvmBlockId lbl) _:_) -> Just lbl
  (body_stmts,body_blks,nnrtp) <- compileWhile
                                  (case e2 of
                                      Nothing -> ExprInt 1
                                      Just r2 -> r2)              
                                  (body++(case e3 of
                                             Nothing -> []
                                             Just r3 -> [StmtExpr r3])) nrtp lbl_start
  return (body_stmts,body_blks++init_blks',nnrtp)
compileStatement (StmtIf expr ifTrue mel) brk rtp = do
  lblEnd <- newLabel
  (res,var,_) <- compileExpression expr (Just TypeBool)
  stackPush
  (blksTrue,nrtp1) <- do
    (stmts,blks,nrtp) <- compileStatement ifTrue brk rtp
    nblks <- appendStatements ([Branch (LMLocalVar lblEnd LMLabel)]++stmts) blks
    return (nblks,nrtp)
  stackPop
  (blksFalse,nrtp2) <- case mel of
    Nothing -> return ([],nrtp1)
    Just st -> do
      stackPush
      (blks,nrtp) <- do
        (stmts,blks,nnrtp) <- compileStatement st brk nrtp1
        nblks <- appendStatements ([Branch (LMLocalVar lblEnd LMLabel)]++stmts) blks
        return (nblks,nnrtp)
      stackPop
      return (blks,nrtp)
  let lblTrue = case blksTrue of
        [] -> lblEnd
        _  -> let LlvmBlock { blockLabel = LlvmBlockId lbl } = last blksTrue in lbl
  let lblFalse = case blksFalse of
        [] -> lblEnd
        _  -> let LlvmBlock { blockLabel = LlvmBlockId lbl } = last blksFalse in lbl
  return ([BranchIf var (LMLocalVar lblTrue LMLabel) (LMLocalVar lblFalse LMLabel)] ++ res,
          [LlvmBlock
          { blockLabel = LlvmBlockId lblEnd
          , blockStmts = []
          }]++blksFalse++blksTrue,nrtp2)
compileStatement (StmtExpr expr) _ rtp = do
  (stmts,var,_) <- compileExpression expr Nothing
  return (stmts,[],rtp)
compileStatement StmtBreak Nothing _ = error "Nothing to break to"
compileStatement StmtBreak (Just lbl) rtp = return ([Branch (LMLocalVar lbl LMLabel)],[],rtp)
compileStatement _ _ rtp = return ([Unreachable],[],rtp)

compileWhile cond body rtp mlbl_start = do
  lbl_start <- case mlbl_start of
    Just lbl -> return lbl
    Nothing -> newLabel
  lbl_test <- newLabel
  lbl_end <- newLabel
  wvars <- mapM (\(cid@(ConstId _ (wvar:_))) -> do
                    (_,Variable tp ref) <- stackLookup cid
                    lbl <- newLabel
                    rtp <- toLLVMType tp
                    stackPut wvar (Variable tp (LMLocalVar lbl rtp))
                    return (cid,tp,rtp,lbl,ref)
                ) (Set.toList (writes ((StmtExpr cond):body)))
  (_,st) <- get
  (loop,nrtp) <- compileStatements body [] (Just lbl_end) rtp
  let (LlvmBlock (LlvmBlockId lbl_loop) _):_ = loop
  nloop <- appendStatements [Branch (LMLocalVar lbl_test LMLabel)] loop
  phis <- mapM (\(cid,tp,rtp,lbl,ref) -> do
                   (_,Variable _ nref) <- stackLookup cid
                   return (Assignment
                           (LMLocalVar lbl rtp)
                           (Phi rtp [(ref,LMLocalVar lbl_start LMLabel),
                                     (nref,LMLocalVar lbl_loop LMLabel)]))) wvars
  modify (\(uniq,_) -> (uniq,st))
  (test_stmts,test_var,_) <- compileExpression cond (Just TypeBool)
  return (case mlbl_start of
             Nothing -> [Branch (LMLocalVar lbl_start LMLabel)]
             Just _ -> [],
          [LlvmBlock (LlvmBlockId lbl_end) []]++nloop++
          [LlvmBlock
           (LlvmBlockId lbl_test)
           ([BranchIf test_var (LMLocalVar lbl_loop LMLabel) (LMLocalVar lbl_end LMLabel)] ++ test_stmts ++ phis)
          ]++(case mlbl_start of
              Just _ -> []
              Nothing -> [LlvmBlock (LlvmBlockId lbl_start) [Branch (LMLocalVar lbl_test LMLabel)]])
          ,rtp)

compileExpression :: Expression -> Maybe RType -> Compiler ([LlvmStatement],LlvmVar,RType)
compileExpression (ExprInt n) tp = case tp of
  Nothing -> do
    rtp <- toLLVMType TypeInt
    return ([],LMLitVar $ LMIntLit n rtp,TypeInt)
  Just rtp -> case rtp of
    TypeInt -> return ([],LMLitVar $ LMIntLit n (LMInt 32),TypeInt)
    TypeFloat -> return ([],LMLitVar $ LMFloatLit (fromIntegral n) LMDouble,TypeFloat)
    _ -> error $ "Ints can't have type "++show rtp
compileExpression e@(ExprId name) etp = do
  (n,ref) <- stackLookup name
  case ref of
    Variable tp var -> do
      typeCheck e etp tp
      return ([],var,tp)
    Pointer tp -> do
      typeCheck e etp tp
      rtp <- toLLVMType tp
      lbl <- newLabel
      let tvar = LMLocalVar lbl rtp
      return ([Assignment tvar (Load (LMNLocalVar n (LMPointer rtp)))],tvar,tp)
    Function tp args -> do
      typeCheck e etp (TypeFunction tp args)
      fdecl <- genFuncDecl n tp args
      return ([],LMGlobalVar n (LMFunction fdecl) External Nothing Nothing False,TypeFunction tp args)
compileExpression e@(ExprAssign Assign tid expr) etp = do
  (n,ref) <- stackLookup tid
  case ref of
    Variable tp var -> do
      typeCheck e etp tp
      (extra,res,_) <- compileExpression expr (Just tp)
      llvmtp <- toLLVMType tp
      let ConstId _ (name:_) = tid
      stackPut name (Variable tp res)
      return (extra,res,tp)
    Pointer ptp -> do
      typeCheck e etp ptp
      (extra,res,_) <- compileExpression expr (Just ptp)
      llvmtp <- toLLVMType ptp
      return ([Store res (LMNLocalVar n (LMPointer llvmtp))]++extra,res,ptp)
compileExpression e@(ExprBin op lexpr rexpr) etp = do
  (lextra,lres,tpl) <- compileExpression lexpr Nothing
  (rextra,rres,tpr) <- compileExpression rexpr (Just tpl)
  res <- newLabel
  case op of
    BinLess -> do
      typeCheck e etp TypeBool
      let resvar = LMLocalVar res (LMInt 1)
      return ([Assignment resvar (Compare LM_CMP_Slt lres rres)]++rextra++lextra,resvar,TypeBool)
    BinPlus -> do
      typeCheck e etp tpl
      llvmtp <- toLLVMType tpl
      let resvar = LMLocalVar res llvmtp
      return ([Assignment resvar (LlvmOp LM_MO_Add lres rres)]++rextra++lextra,resvar,tpl)
compileExpression e@(ExprCall expr args) etp = do
  (eStmts,eVar,ftp) <- compileExpression expr Nothing
  case ftp of
    TypeFunction rtp argtp
        | (length argtp) == (length args) -> do
          rargs <- zipWithM (\arg tp -> compileExpression arg (Just tp)) args argtp
          res <- newLabel
          resvar <- toLLVMType rtp >>= return.(LMLocalVar res)
          return ([Assignment resvar (Call StdCall eVar [ v | (_,v,_) <- rargs ] [])]++(concat [stmts | (stmts,_,_) <- rargs])++eStmts,resvar,rtp)
        | otherwise -> throwError [WrongNumberOfArguments e (length  args) (length argtp)]
    _ -> throwError [NotAFunction e ftp]
compileExpression e@(ExprLambda args body) etp = do
  fid <- newLabel
  let fname = BS.pack ("lambda"++show fid)
  rargs <- mapM (\(name,tp) -> do
                    tp' <- translateType tp
                    ltp <- toLLVMType tp'
                    return (name,tp',ltp)) args
  fdecl <- genFuncDecl fname TypeInt [ tp | (_,tp,_) <- rargs]
  let rtp = case etp of
        Just (TypeFunction r _) -> Just r
        _ -> Nothing
  (blks,nrtp) <- stackShadow $ do
    stackAdd $ Map.fromList [ (name,(BS.pack name,Variable tp (LMNLocalVar (BS.pack name) ltp))) | (name,tp,ltp) <- rargs ]
    (stmts,blks,rtp2) <- compileStatement body Nothing rtp
    nblks <- appendStatements stmts blks
    return (nblks,rtp2)
  tell $ [LlvmFunction { funcDecl = fdecl,
                         funcArgs = map (BS.pack . fst) args,
                         funcAttrs = [],
                         funcSect = Nothing,
                         funcBody = readyBlocks blks
                       }]
  let ftp = TypeFunction (case nrtp of
                             Nothing -> TypeVoid
                             Just tp -> tp) [tp | (_,tp,_) <- rargs]
  typeCheck e etp ftp
  return ([],LMGlobalVar fname (LMFunction fdecl) External Nothing Nothing False,ftp)
  
    
compileExpression expr _ = error $ "Couldn't compile expression "++show expr


toLLVMType :: RType -> Compiler LlvmType
toLLVMType TypeInt = return $ LMInt 32
toLLVMType TypeBool = return $ LMInt 1
toLLVMType (TypeId n) = do
  cls <- ask
  return (LMAlias $ fst $ cls!n)

defaultValue :: RType -> Compiler LlvmVar
defaultValue TypeInt = return (LMLitVar (LMIntLit 0 (LMInt 32)))

writes :: [Statement] -> Set ConstantIdentifier
writes xs = writes' xs Set.empty
  where
    writes' :: [Statement] -> Set ConstantIdentifier -> Set ConstantIdentifier
    writes' [] s = s
    writes' (x:xs) s = writes' xs (writes'' x s)

    writes'' :: Statement -> Set ConstantIdentifier -> Set ConstantIdentifier
    writes'' (StmtBlock stmts) s = writes' stmts s
    writes'' (StmtExpr expr) s = writes''' expr s
    writes'' (StmtDecl name _ (Just expr)) s = writes''' expr (Set.insert (ConstId False [name]) s)
    writes'' (StmtIf cond ifTrue ifFalse) s = writes''' cond (writes'' ifTrue (case ifFalse of
                                                                                  Nothing -> s
                                                                                  Just e -> writes'' e s))
    writes'' (StmtReturn (Just expr)) s = writes''' expr s
    writes'' (StmtFor init cond it body) s = let s1 = case init of
                                                   Nothing -> s
                                                   Just r1 -> writes''' r1 s
                                                 s2 = case cond of
                                                   Nothing -> s1
                                                   Just r2 -> writes''' r2 s1
                                                 s3 = case it of
                                                   Nothing -> s2
                                                   Just r3 -> writes''' r3 s2
                                             in writes' body s3
    writes'' _ s = s
    
    writes''' :: Expression -> Set ConstantIdentifier -> Set ConstantIdentifier
    writes''' (ExprAssign _ lhs rhs) s = writes''' rhs (Set.insert lhs s)
    writes''' (ExprCall cmd args) s = foldl (\s' e -> writes''' e s') s (cmd:args)
    writes''' (ExprBin _ lhs rhs) s = writes''' rhs (writes''' lhs s)
    writes''' (ExprIndex lhs rhs) s = writes''' rhs (writes''' rhs s)
    writes''' (ExprLambda _ stmt) s = writes'' stmt s
    writes''' _ s = s

