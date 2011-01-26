module Language.Pike.Compiler where

import Language.Pike.Syntax
import Llvm
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (mapMaybe,catMaybes)
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Unique
import Control.Monad.State
import Control.Monad.Writer
import Data.List (find)

type Compiler a = StateT ([Unique],Stack) (Writer [LlvmFunction]) a

data StackReference = Pointer Type BS.ByteString
                    | Variable Type BS.ByteString
                    | Function Type [Type] BS.ByteString
                    deriving Show

type Stack = [[StackReference]]

initialStack :: [Definition] -> [StackReference]
initialStack [] = []
initialStack ((Definition _ body):xs)
    = case body of
        FunctionDef name tp args _ -> (Function tp [t | (_,t) <- args] (BS.pack name)):rest
        _ -> rest
    where
      rest = initialStack xs
                        

stackAlloc' :: String -> Type -> Stack -> Stack
stackAlloc' name tp []     = [[Pointer tp (BS.pack name)]]
stackAlloc' name tp (x:xs) = ((Pointer tp (BS.pack name)):x):xs

stackAlloc :: String -> Type -> Compiler ()
stackAlloc name tp = modify $ \(uniq,st) -> (uniq,stackAlloc' name tp st)

stackLookup' :: ConstantIdentifier -> Stack -> StackReference
stackLookup' s [] = error $ "Couldn't lookup "++show s
stackLookup' s@(ConstId _ (name:_)) (x:xs) = case find (\ref -> case ref of
                                                                 Pointer _ n -> n == BS.pack name
                                                                 Variable _ n -> n == BS.pack name
                                                                 Function _ _ n -> n == BS.pack name
                                                       ) x of
                                               Nothing -> stackLookup' s xs
                                               Just ref -> ref

stackLookup :: ConstantIdentifier -> Compiler StackReference
stackLookup s = do
  (_,st) <- get
  return $ stackLookup' s st

stackAdd :: [StackReference] -> Compiler ()
stackAdd refs = modify $ \(uniq,st) -> (uniq,refs:st)

stackPush :: Compiler ()
stackPush = modify $ \(uniq,st) -> (uniq,[]:st)

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

newLabel :: Compiler Unique
newLabel = do
  (x:xs,st) <- get
  put (xs,st)
  return x

uniques :: IO [Unique]
uniques = do
  x <- newUnique
  xs <- unsafeInterleaveIO uniques
  return $ x:xs

runCompiler :: Compiler a -> IO a
runCompiler c = do
  uniq <- uniques
  return $ fst $ runWriter $ evalStateT c (uniq,[])

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = mapM f xs >>= return.catMaybes

compilePike :: [Definition] -> Compiler LlvmModule
compilePike defs = do
  (f1,f2) <- listen $ do
    stackAdd (initialStack defs)
    funcs <- mapMaybeM (\(Definition mods x) -> case x of
                           FunctionDef name ret args block -> compileFunction name ret args block >>= return.Just
                           _ -> return Nothing) defs
    stackPop
    return funcs
  return $  LlvmModule
             { modComments = []
             , modAliases = []
             , modGlobals = []
             , modFwdDecls = []
             , modFuncs = f2++f1
             }

compileFunction :: String -> Type -> [(String,Type)] -> [Statement] -> Compiler LlvmFunction
compileFunction name ret args block = let decl = genFuncDecl (BS.pack name) ret (map snd args)
                                      in do
                                        stackPush
                                        stackAdd [Variable argtp (BS.pack argn) | (argn,argtp) <- args]
                                        blks <- compileBody block
                                        stackPop
                                        return $ LlvmFunction
                                             { funcDecl = decl
                                             , funcArgs = [BS.pack name | (name,tp) <- args]
                                             , funcAttrs = []
                                             , funcSect = Nothing
                                             , funcBody = blks
                                             }

genFuncDecl :: BS.ByteString -> Type -> [Type] -> LlvmFunctionDecl
genFuncDecl name ret_tp args = LlvmFunctionDecl
                               { decName = name
                               , funcLinkage = External
                               , funcCc = CC_Fastcc
                               , decReturnType = toLLVMType ret_tp
                               , decVarargs = FixedArgs
                               , decParams = map (\arg -> (toLLVMType arg,[])) args
                               , funcAlign = Nothing
                               }

compileBody :: [Statement] -> Compiler [LlvmBlock]
compileBody stmts = do
  blks <- compileStatements stmts [] Nothing
  return $ readyBlocks blks

readyBlocks :: [LlvmBlock] -> [LlvmBlock]
readyBlocks = reverse.map (\blk -> blk { blockStmts = reverse (blockStmts blk) })

appendStatements :: [LlvmStatement] -> [LlvmBlock] -> Compiler [LlvmBlock]
appendStatements stmts [] = do
  lbl <- newLabel
  return [LlvmBlock { blockLabel = LlvmBlockId lbl
                    , blockStmts = stmts
                    }]
appendStatements stmts (x:xs) = return $ x { blockStmts = stmts ++ (blockStmts x) }:xs

compileStatements :: [Statement] -> [LlvmBlock] -> Maybe Unique -> Compiler [LlvmBlock]
compileStatements [] blks _ = return blks
compileStatements (x:xs) blks brk = do
  (stmts,nblks) <- compileStatement x brk
  nblks2 <- appendStatements stmts blks
  compileStatements xs (nblks ++ nblks2) brk

compileStatement :: Statement -> Maybe Unique -> Compiler ([LlvmStatement],[LlvmBlock])
compileStatement (StmtBlock stmts) brk = do
  stackPush
  blks <- compileStatements stmts [] brk
  stackPop
  return ([],blks)
compileStatement (StmtDecl name tp expr) _ = do
  stackAlloc name tp
  let rtp = toLLVMType tp
  let tvar = LMNLocalVar (BS.pack name) (LMPointer rtp)
  let alloc = Assignment tvar (Alloca rtp 1)
  case expr of
    Nothing -> return ([alloc],[])
    Just rexpr -> do
              (extra,res) <- compileExpression rexpr
              lbl <- newLabel
              return ([Store res tvar,alloc]++extra,[])
compileStatement (StmtReturn expr) _ = case expr of
                                       Nothing -> return ([Return Nothing],[])
                                       Just rexpr -> do
                                                 (extra,res) <- compileExpression rexpr
                                                 return ([Return (Just res)]++extra,[])
compileStatement (StmtFor e1 e2 e3 body) _ = do
  begin <- case e1 of
            Nothing -> return []
            Just re1 -> do
                    (res,_) <- compileExpression re1
                    return res
  it <- case e3 of
    Nothing -> return []
    Just re3 -> do
      (res,_) <- compileExpression re3
      return res
  lbl_cont <- newLabel
  lbl_end <- newLabel
  stackPush
  body_blks' <- compileStatements body [] (Just lbl_end)
  stackPop
  body_blks <- appendStatements ((Branch (LMLocalVar lbl_cont LMLabel):it)) body_blks'
  let LlvmBlock { blockLabel = LlvmBlockId lbl_next } = last body_blks
  sw <- case e2 of
         Nothing -> return Nothing
         Just re2 -> compileExpression re2 >>= return.Just
  let sw_stmt = case sw of
        Nothing -> []
        Just (stmts,res) -> [BranchIf res (LMLocalVar lbl_next LMLabel) (LMLocalVar lbl_end LMLabel)]++stmts
  return ([Branch (LMLocalVar lbl_cont LMLabel)]++begin,[LlvmBlock (LlvmBlockId lbl_end) []]++body_blks++[LlvmBlock (LlvmBlockId lbl_cont) sw_stmt])
compileStatement (StmtIf expr ifTrue mel) brk = do
  lblEnd <- newLabel
  (res,var) <- compileExpression expr
  stackPush
  blksTrue <- compileStatement ifTrue brk
              >>= uncurry appendStatements
              >>= appendStatements [Branch (LMLocalVar lblEnd LMLabel)]
  stackPop
  blksFalse <- case mel of
    Nothing -> return []
    Just st -> do
      stackPush
      blks <- compileStatement st brk
              >>= uncurry appendStatements
              >>= appendStatements [Branch (LMLocalVar lblEnd LMLabel)]
      stackPop
      return blks
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
          }]++blksFalse++blksTrue)
compileStatement (StmtExpr expr) _ = do
  (stmts,var) <- compileExpression expr
  return (stmts,[])
compileStatement StmtBreak Nothing = error "Nothing to break to"
compileStatement StmtBreak (Just lbl) = return ([Branch (LMLocalVar lbl LMLabel)],[])
compileStatement _ _ = return ([Unreachable],[])

compileExpression :: Expression -> Compiler ([LlvmStatement],LlvmVar)
compileExpression (ExprInt n) = return ([],LMLitVar $ LMIntLit n (toLLVMType TypeInt))
compileExpression (ExprId name) = do
  ref <- stackLookup name
  case ref of
    Variable tp n -> return ([],LMNLocalVar n (toLLVMType tp))
    Pointer tp n -> do
             let rtp = toLLVMType tp
             lbl <- newLabel
             let tvar = LMLocalVar lbl rtp
             return ([Assignment tvar (Load (LMNLocalVar n (LMPointer rtp)))],tvar)
    Function tp args n -> return ([],LMGlobalVar n (LMFunction (genFuncDecl n tp args)) External Nothing Nothing False)
    --Function _ _ _ -> error $ "Can't use function "++show name++" like a variable"
compileExpression (ExprAssign Assign tid expr) = do
  (extra,res) <- compileExpression expr
  ref <- stackLookup tid
  case ref of
    Variable tp n -> error "Please don't assign to function arguments yet..."
    Pointer tp n -> return ([Store res (LMNLocalVar n (LMPointer (toLLVMType tp)))]++extra,res)
compileExpression (ExprBin op lexpr rexpr) = do
  (lextra,lres) <- compileExpression lexpr
  (rextra,rres) <- compileExpression rexpr
  res <- newLabel
  case op of
    BinLess -> do
            let resvar = LMLocalVar res (LMInt 1)
            return ([Assignment resvar (Compare LM_CMP_Slt lres rres)]++rextra++lextra,resvar)
    BinPlus -> do
            let resvar = LMLocalVar res (LMInt 32)
            return ([Assignment resvar (LlvmOp LM_MO_Add lres rres)]++rextra++lextra,resvar)
compileExpression (ExprCall expr args) = do
  (eStmts,eVar) <- compileExpression expr
  rargs <- mapM compileExpression args
  res <- newLabel
  let tp = case eVar of
        LMGlobalVar _ t _ _ _ _ -> t
        LMLocalVar _ t -> t
        LMNLocalVar _ t -> t
        _ -> error "Can't call lit"
  let rtp = case tp of
        LMFunction decl -> decReturnType decl
        _ -> error "Can't call non-function type"
  let resvar = LMLocalVar res rtp
  return ([Assignment resvar (Call StdCall eVar (map snd rargs) [])]++(concat $ map fst rargs)++eStmts,resvar)
  {-ref <- stackLookup name
  case ref of
    Function tp args rname -> do
             let resvar = LMLocalVar res (toLLVMType tp)
             let fdecl = genFuncDecl rname tp args
             let fval = LMGlobalVar rname (LMFunction fdecl) Internal Nothing Nothing False
             return ([Assignment resvar (Call StdCall fval (map snd rargs) [])]++(concat $ map fst rargs),resvar)
    _ -> error $ "trying to call non-function "++show name-}
compileExpression (ExprString str) = return ([],LMLitVar $ LMIntLit 95 (toLLVMType TypeInt))
compileExpression (ExprLambda args body) = do
  fid <- newLabel
  let fname = BS.pack ("lambda"++show (hashUnique fid))
  let fdecl = genFuncDecl fname TypeInt (map snd args)
  blks <- stackShadow $ do
    stackAdd [ Variable tp (BS.pack name) | (name,tp) <- args ]
    compileStatement body Nothing >>= uncurry appendStatements
  tell $ [LlvmFunction { funcDecl = fdecl,
                         funcArgs = map (BS.pack . fst) args,
                         funcAttrs = [],
                         funcSect = Nothing,
                         funcBody = readyBlocks blks
                       }]
  return ([],LMGlobalVar fname (LMFunction fdecl) External Nothing Nothing False)
  
    
compileExpression expr = error $ "Couldn't compile expression "++show expr

toLLVMType :: Type -> LlvmType
toLLVMType TypeInt = LMInt 32