{-# LANGUAGE TupleSections #-}
module Language.Pike.Compiler where

import Language.Pike.Syntax
import Language.Pike.Compiler.Resolver as Re
import Language.Pike.Compiler.Monad
import Language.Pike.Compiler.Stack as St
import Language.Pike.Compiler.Error
import Llvm.Types
import Llvm.AbsSyn

import qualified Data.ByteString.Char8 as BS
import Data.Map as Map
import Data.Set as Set
import Control.Monad.Writer hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Monad.Error hiding (mapM)
import Data.Traversable (mapM)
import Prelude hiding (mapM,concat,foldl,or,foldr)
import Data.Maybe (catMaybes)
import Data.Foldable
import Data.Ord
import Data.List as List (sortBy,filter)

import Debug.Trace

compilePike :: Show p => [Definition p] -> Either [CompileError p] LlvmModule
compilePike defs = case resolve defs of
  Left errs -> Left errs
  Right (st,mp) -> runCompiler mp st $ do
    ((f1,aliases,globals),f2) <- listen $ do
      alias <- generateAliases
      vtab <- generateVTables
      funcs <- mapMaybeM (\(Definition mods x _) -> case x of
                             FunctionDef name ret args block -> compileFunction name ret args block >>= return.Just.(\x -> [x])
                             ClassDef name args cdefs -> compileClass (TypeId (ConstId False [name])) name args cdefs >>= return.Just
                             _ -> return Nothing) defs
      (_,_,strs) <- get
      let global_strs = [ (LMGlobalVar (BS.pack $ "str"++show id) tp Internal Nothing Nothing True,
                           Just $ LMStaticStruc [LMStaticLit (LMIntLit (fromIntegral len) (LMInt 32))
                                                ,LMStaticStr (BS.pack $ escapeStr str) (LMArray len (LMInt 8))] tp)
                        | (str,id) <- Map.toList strs, let len = length str,let tp = LMStruct [LMInt 32,LMArray len (LMInt 8)] ]
      return (concat funcs,alias++(fmap snd vtab),(fmap fst vtab)++global_strs)
    return $  LlvmModule { modComments = []
                         , modAliases = aliases
                         , modGlobals = globals
                         , modFwdDecls = [write_decl]
                         , modFuncs = f2++f1
                         }

escapeStr :: String -> String
escapeStr str = concat $ fmap (\c -> case c of
                                  '\n' -> "\\0A"
                                  _ -> [c]) str

write_decl = LlvmFunctionDecl { decName = BS.pack "write"
                              , funcLinkage = External
                              , funcCc = CC_Ccc
                              , decReturnType = LMInt 32
                              , decVarargs = FixedArgs
                              , decParams = [(LMInt 32,[]),(LMPointer $ LMInt 8,[]),(LMInt 32,[])]
                              , funcAlign = Nothing
                              }

compileClass :: Type -> String -> [(String,Type)] -> [Definition p] -> Compiler [LlvmFunction] p
compileClass ctype cname _ cdefs = do
  let rname = ConstId False [cname]
  (ref,_) <- stackLookupM Nothing rname
  cid <- case ref of
    Class n -> return n
    _ -> throwError [NotAClass rname]
  stackPushClass cid
  res <- mapMaybeM (\(Definition mods x _) -> case x of
                      FunctionDef name ret args block -> compileMethod ctype cname name ret args block >>= return.Just.(\x -> [x])
                      _ -> return Nothing) cdefs
  stackPop
  return (concat res)

compileMethod :: Type -> String -> String -> Type -> [(String,Type)] -> [Pos Statement p] -> Compiler LlvmFunction p
compileMethod ctype cname name ret args block = do
  ctp <- translateType ctype
  llvm_ctp <- toLLVMType ctp
  ret_tp <- translateType ret
  rargs <- mapM (\(name,tp) -> do
                   rtp <- translateType tp
                   return (name,rtp)) args
  decl <- genFuncDecl (BS.pack $ cname ++ "__" ++ name) ret_tp (ctp:[tp | (_,tp) <- rargs])
  let this = LMNLocalVar (BS.pack "this") llvm_ctp
  stackPushMethod name ret_tp rargs this
  blks <- compileBody block ret_tp
  stackPop
  return $ LlvmFunction { funcDecl = decl
                        , funcArgs = (BS.pack "this"):[BS.pack name | (name,tp) <- args]
                        , funcAttrs = [GC "shadow-stack"]
                        , funcSect = Nothing
                        , funcBody = blks
                        }

compileFunction :: String -> Type -> [(String,Type)] -> [Pos Statement p] -> Compiler LlvmFunction p
compileFunction name ret args block = do
  ret_tp <- translateType ret
  rargs <- mapM (\(name,tp) -> do
                    rtp <- translateType tp
                    return (name,rtp)) args
  decl <- genFuncDecl (BS.pack name) ret_tp [tp | (_,tp) <- rargs]
  stackPushFunction name ret_tp rargs
  blks <- compileBody block ret_tp
  stackPop
  return $ LlvmFunction { funcDecl = decl
                        , funcArgs = [BS.pack name | (name,tp) <- args]
                        , funcAttrs = [GC "shadow-stack"]
                        , funcSect = Nothing
                        , funcBody = blks
                        }

compileBody :: [Pos Statement p] -> RType -> Compiler [LlvmBlock] p
compileBody stmts rtp = do
  blks <- compileStatements stmts []
  nblks <- if lastBlockTerminated blks
          then return blks
          else (case rtp of
                   TypeVoid -> appendStatements [Return Nothing] blks
                   _ -> do
                     var <- mbDefault rtp Nothing
                     appendStatements [Return $ Just var] blks)
  return $ readyBlocks nblks

readyBlocks :: [LlvmBlock] -> [LlvmBlock]
readyBlocks = reverse.fmap (\blk -> blk { blockStmts = case blockStmts blk of
                                             [] -> [Unreachable]
                                             _ -> reverse (blockStmts blk) 
                                        })

genFuncDecl :: BS.ByteString -> RType -> [RType] -> Compiler LlvmFunctionDecl p
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


mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = mapM f xs >>= return.catMaybes

generateStruct :: ClassMap -> ClassMapEntry -> [LlvmType]
generateStruct cm entr = let cur = fmap (\(_,tp) -> toLLVMType' cm tp) (classVariables entr) 
                             inh = concat $ fmap (\i -> generateStruct cm (cm!i)) (Set.toList $ Re.classInherits entr)
                         in inh++cur

generateAliases :: Compiler [LlvmAlias] p
generateAliases = do
  mp <- ask
  return $ fmap (\cls -> (BS.pack $ Re.className cls,LMStruct $ (LMPointer $ LMInt 8):(generateStruct mp cls))) (Map.elems mp)

generateVEntry :: ClassMap -> Integer -> ClassMapEntry -> Integer -> Map String (Integer,(LlvmStatic,LlvmType)) -> Compiler (Integer,Map String (Integer,(LlvmStatic,LlvmType))) p
generateVEntry cm cid entr c mp = do
  (nc,nmp) <- foldlM (\(tc,tmp) i -> generateVEntry cm i (cm!i) tc tmp) (c,mp) (Set.toList $ Re.classInherits entr)
  foldlM (\(tc,tmp) (fname,(rtp,argtp)) -> do
             let bname = BS.pack $ Re.className entr ++ "__" ++ fname
             fdecl <- genFuncDecl bname rtp (TypeId cid:argtp)
             case Map.insertLookupWithKey (\_ (_,nv) (i,ov) -> (i,nv)) fname (tc,(LMStaticPointer $ LMGlobalVar bname (LMPointer $ LMFunction fdecl) Internal Nothing Nothing True,LMPointer $ LMFunction fdecl)) tmp of
               (Just _,nmp) -> return (tc,nmp)
               (Nothing,nmp) -> return (tc+1,nmp)
         ) (nc,nmp) (Re.classMethods entr)

generateVTables :: Compiler [(LMGlobal,LlvmAlias)] p
generateVTables = do
  mp <- ask
  mapM (\(cid,cls) -> do
           (_,entrs) <- generateVEntry mp cid cls 0 Map.empty
           let elems = fmap snd (List.sortBy (comparing fst) (Map.elems entrs))
               statics = fmap fst elems
               tps = fmap snd elems
               alias_name = BS.pack $ "VTable__"++Re.className cls
           return ((LMGlobalVar (BS.pack $ "vtable__"++Re.className cls) (LMAlias alias_name) Internal Nothing Nothing True,Just $ LMStaticStruc statics (LMAlias alias_name)),
                   (alias_name,LMStruct tps)
                  )
       ) (Map.toList mp)
             

stackLookupM :: Maybe p -> ConstantIdentifier -> Compiler (StackReference LlvmVar,Integer) p
stackLookupM pos i = do
  (_,st,_) <- get
  case stackLookup i st of
    Nothing -> throwError [LookupFailure i pos]
    Just ref -> return ref

toLLVMType :: RType -> Compiler LlvmType p
toLLVMType tp = do
  cls <- ask
  return $ toLLVMType' cls tp

toLLVMType' :: ClassMap -> RType -> LlvmType
toLLVMType' _ TypeInt = LMInt 32
toLLVMType' _ TypeBool = LMInt 1
toLLVMType' _ TypeVoid = LMVoid
toLLVMType' cls (TypeArray el) = let eltp = toLLVMType' cls el
                                 in LMPointer $ LMStruct [LMInt 32,LMArray 0 eltp]
toLLVMType' cls (TypeId n) = LMPointer $ LMAlias $ BS.pack $ Re.className $ cls!n
toLLVMType' cls TypeString = LMPointer $ LMStruct [LMInt 32,LMArray 0 (LMInt 8)]

translateType :: Type -> Compiler RType p
translateType (TypeId name) = do
  (ref,_) <- stackLookupM Nothing name
  case ref of
    Class n -> return (TypeId n)
    _ -> throwError [NotAClass name]
translateType (TypeArray atp) = do
  ratp <- translateType atp
  return $ TypeArray ratp
translateType (TypeFunction rtp args) = do
  rrtp <- translateType rtp
  rargs <- mapM translateType args
  return $ TypeFunction rrtp rargs
translateType x = return $ fmap (const undefined) x

compileStatements :: [Pos Statement p] -> [LlvmBlock] -> Compiler [LlvmBlock] p
compileStatements [] blks = return blks
compileStatements ((Pos x pos):xs) blks = do
  nblks <- compileStatement pos x blks
  if terminates x
    then return nblks
    else compileStatements xs nblks

appendStatements :: [LlvmStatement] -> [LlvmBlock] -> Compiler [LlvmBlock] p
appendStatements [] [] = return []
appendStatements stmts [] = do
  lbl <- newLabel
  return [LlvmBlock { blockLabel = LlvmBlockId lbl
                    , blockStmts = stmts
                    }]
appendStatements stmts (x:xs) = return $ x { blockStmts = stmts ++ (blockStmts x) }:xs

terminates :: Statement p -> Bool
terminates (StmtReturn _) = True
terminates StmtBreak = True
terminates _ = False

terminatesLLVM :: LlvmStatement -> Bool
terminatesLLVM (Return _) = True
terminatesLLVM (Branch _) = True
terminatesLLVM (BranchIf _ _ _) = True
terminatesLLVM _ = False

lastBlockTerminated :: [LlvmBlock] -> Bool
lastBlockTerminated [] = False
lastBlockTerminated ((LlvmBlock _ stmts):_) = case stmts of
  [] -> False
  stmt:_ -> terminatesLLVM stmt

stackPop :: Compiler () p
stackPop = do
  (x,st,strs) <- get
  case stackPop' st of
    Just nst -> put (x,nst,strs)
    Nothing -> throwError [InternalError "Stack corrupt"]

firstLabel :: [LlvmBlock] -> Integer
firstLabel blks = let LlvmBlockId lbl = blockLabel (last blks) in lbl

lastLabel :: [LlvmBlock] -> Integer
lastLabel blks = let LlvmBlockId lbl = blockLabel (head blks) in lbl

compileStatement :: p -> Statement p -> [LlvmBlock] -> Compiler [LlvmBlock] p
compileStatement _ (StmtBlock stmts) cur = do
  stackPushBlock
  blks <- compileStatements stmts cur
  stackPop
  return blks
compileStatement _ (StmtDecl name tp expr) cur = do
  rtp <- translateType tp
  ltp <- toLLVMType rtp
  stackAlloc name rtp
  case expr of
    Nothing -> return cur
    Just rexpr -> do
      (extra,res,_) <- compileExpression' "initialization" rexpr (Just rtp)
      stackPut name res
      appendStatements extra cur
compileStatement _ st@(StmtReturn expr) cur = do
  rtp <- returnTypeM
  case expr of
    Nothing -> case rtp of
      TypeVoid -> appendStatements [Return Nothing] cur
      _ -> throwError [WrongReturnType st TypeVoid rtp]
    Just rexpr -> do
      (extra,res,_) <- compileExpression' "return value" rexpr (Just rtp)
      appendStatements ([Return (Just res)]++extra) cur
compileStatement _ (StmtWhile cond body) cur = compileWhile cond body cur
compileStatement pos (StmtFor init cond it body) cur = do
  let init' = case init of
        Nothing -> []
        Just (Left expr) -> [Pos (StmtExpr expr) pos]
        Just (Right (tp,name,expr)) -> [Pos (StmtDecl name tp (Just expr)) pos]
      cond' = case cond of
        Nothing -> Pos (ExprInt 1) pos
        Just expr -> expr
      it' = case it of
        Nothing -> []
        Just stmt -> [Pos (StmtExpr stmt) pos]
  compileStatement pos (StmtBlock (init'++[Pos (StmtWhile cond' (body++it')) pos])) cur
compileStatement _ (StmtExpr expr) cur = do
  (stmts,var,_) <- compileExpression' "statement expression" expr Nothing
  appendStatements stmts cur
compileStatement pos StmtBreak cur = do
  (lbl,ncur) <- case cur of
    (LlvmBlock (LlvmBlockId lbl) _):_ -> return (lbl,cur)
    [] -> do
      lbl <- newLabel
      return (lbl,[LlvmBlock (LlvmBlockId lbl) []])
  brk <- breakLabelM pos
  (c,st,strs) <- get
  nst <- case stackPopTill (\st -> case st of
                               LocalContext { localType = LoopContext {} } -> True
                               _ -> False) st >>= stackPop' of
           Nothing -> throwError [NothingToBreakTo pos]
           Just b -> return b
  let Just nst2 = addBreak lbl nst st
  put (c,nst2,strs)
  appendStatements [Branch (LMLocalVar brk LMLabel)] ncur
compileStatement _ (StmtIf expr (Pos ifTrue tpos) mel) cur = do
  lblEnd <- newLabel
  lblTrue <- newLabel
  lblFalse <- case mel of
    Just _ -> newLabel
    Nothing -> return lblEnd
  (res,var,_) <- compileExpression' "condition" expr (Just TypeBool)
  ncur1 <- appendStatements ([BranchIf var (LMLocalVar lblTrue LMLabel) (LMLocalVar lblFalse LMLabel)]++res) cur
  let ncur2 = (LlvmBlock (LlvmBlockId lblTrue) []):ncur1
  stackPushBlock
  ncur3 <- do
    nblks <- compileStatement tpos ifTrue ncur2
    if lastBlockTerminated nblks
      then return nblks
      else appendStatements [Branch (LMLocalVar lblEnd LMLabel)] nblks
  stackPop
  ncur4 <- case mel of
    Nothing -> return ncur3
    Just (Pos st stpos) -> do
      stackPushBlock
      res <- compileStatement stpos st ((LlvmBlock (LlvmBlockId lblFalse) []):ncur3)
      stackPop
      return res
  return $ (LlvmBlock (LlvmBlockId lblEnd) []):ncur4
compileStatement _ stmt _ = throwError [NotImplemented $ "Compiling "++show stmt]

compileWhile :: Pos Expression p -> [Pos Statement p] -> [LlvmBlock] -> Compiler [LlvmBlock] p
compileWhile cond body cur = do
  let ws = Set.map (\(ConstId _ (name:_)) -> name) $ writes (fmap posObj body)
  phis <- newPhiVars ws
  phis2 <- newPhiVars ws
  (lbl_start,ncur) <- case cur of
    (LlvmBlock (LlvmBlockId lbl) _):_ -> return (lbl,cur)
    [] -> do
      lbl <- newLabel
      return (lbl,[LlvmBlock (LlvmBlockId lbl) []])
  lbl_test <- newLabel
  lbl_end <- newLabel
  cls <- ask
  (_,stack_start,_) <- get
  let stack_test = stackUpdateVars (\name tp ref -> case Map.lookup name phis of
                                       Nothing -> Nothing
                                       Just i -> Just (LMLocalVar i (toLLVMType' cls tp))) stack_start
  modify (\(count,_,strs) -> (count,stack_test,strs))
  stackPushLoop lbl_start lbl_test lbl_end
  (test_stmts,test_var,_) <- compileExpression' "loop condition" cond (Just TypeBool)
  loop <- compileStatements body []
  let lbl_loop_first = firstLabel loop
      lbl_loop_last = lastLabel loop
  nloop <- if lastBlockTerminated loop
           then return loop
           else appendStatements [Branch (LMLocalVar lbl_test LMLabel)] loop
  (_,LocalContext { localType = LoopContext { loopBreakpoints = brks } },_) <- get
  stackPop
  (_,stack_end,_) <- get
  modify (\(st,_,strs) -> (st,stackUpdateVars (\name tp ref -> case Map.lookup name phis2 of
                                                  Nothing -> Nothing
                                                  Just i -> Just (LMLocalVar i (toLLVMType' cls tp))) stack_test,strs))
  
  res1 <- appendStatements [Branch (LMLocalVar lbl_test LMLabel)] ncur
  return $ [LlvmBlock (LlvmBlockId lbl_end) (createPhis cls phis2 ((lbl_test,stack_test):brks))]++nloop++
    [LlvmBlock
     (LlvmBlockId lbl_test)
     ([BranchIf test_var (LMLocalVar lbl_loop_first LMLabel) (LMLocalVar lbl_end LMLabel)] ++ test_stmts ++
      createPhis cls phis (if lastBlockTerminated loop
                           then [(lbl_start,stack_start)]
                           else [(lbl_loop_last,stack_end),(lbl_start,stack_start)]))
    ]++res1

createPhis :: ClassMap -> Map String Integer -> [(Integer,Stack LlvmVar)] -> [LlvmStatement]
createPhis cls mp [] = []
createPhis cls mp st@((_,GlobalContext {}):_) = []
createPhis cls mp st@((_,ClassContext {}):_) = createPhis cls mp (fmap (\(i,ctx) -> (i,classUpper ctx)) st)
createPhis cls mp st@((_,LocalContext {}):_)
  = let rmap = foldr (\(pt,ctx) p -> Map.intersectionWith (\(tp,ref) (trg,_,lst) -> (trg,tp,(pt,ref):lst)) (localVars ctx) p)
               (fmap (\i -> (i,undefined,[])) mp) st
    in [ Assignment (LMLocalVar trg rtp) (Phi rtp [ (mbDefault' cls tp ref,LMLocalVar lbl LMLabel)
                                                  | (lbl,ref) <- froms])
       | (name,(trg,tp,froms)) <- Map.toList rmap, let rtp = toLLVMType' cls tp
       ] ++ createPhis cls mp (fmap (\(i,ctx) -> (i,localUp ctx)) st)

returnTypeM :: Compiler RType p      
returnTypeM = do
  (_,st,_) <- get
  case returnType st of
    Nothing -> throwError [InternalError "Couldn't figure out return type"]
    Just rtp -> return rtp

breakLabelM :: p -> Compiler Integer p
breakLabelM pos = do
  (_,st,_) <- get
  case breakLabel st of
    Nothing -> throwError [NothingToBreakTo pos]
    Just brk -> return brk

thisPointerM :: Compiler LlvmVar p
thisPointerM = do
  (_,st,_) <- get
  case thisPointer st of
    Nothing -> error "Internal error: this pointer couldn't be resolved"
    Just this -> return this

compileExpression' :: String -> Pos Expression p -> Maybe RType -> Compiler ([LlvmStatement],LlvmVar,RType) p
compileExpression' reason (Pos expr pos) rt = do
  res <- compileExpression pos expr rt
  case res of
    ResultCalc stmts var ret -> return (stmts,var,ret)
    ResultClass n -> do
      classmap <- ask
      throwError [MisuseOfClass reason (Re.className $ classmap!n)]

data CompileExprResult
     = ResultCalc [LlvmStatement] LlvmVar RType
     | ResultMethod [LlvmStatement] LlvmVar LlvmVar RType [RType]
     | ResultClass Integer
     | ResultBuiltIn ConstantIdentifier
     deriving Show

compileExpression :: p -> Expression p -> Maybe RType -> Compiler CompileExprResult p
compileExpression _ (ExprInt n) tp
  = do 
    let rtp = case tp of
          Nothing -> TypeInt
          Just t -> t
    ttp <- toLLVMType rtp
    lit <- case rtp of
      TypeInt -> return $ LMIntLit n ttp
      TypeFloat -> return $ LMFloatLit (fromIntegral n) ttp
      _ -> throwError [TypeMismatch (ExprInt n) TypeInt rtp]
    return $ ResultCalc [] (LMLitVar lit) rtp
compileExpression pos e@(ExprId name) etp
  | Map.member name builtIns = return $ ResultBuiltIn name
  | otherwise = do
    (ref,_) <- stackLookupM (Just pos) name
    case ref of
      Variable tp Nothing -> throwError [UninitializedVariable pos name]
      Variable tp (Just cont) -> do
        typeCheck e etp tp
        return $ ResultCalc [] cont tp
      Function rtype args -> do
        let ConstId _ (rname:_) = name
        decl <- genFuncDecl (BS.pack rname) rtype args
        return $ ResultCalc [] (LMGlobalVar (BS.pack rname) (LMFunction decl) Internal Nothing Nothing True) (TypeFunction rtype args)
      Class i -> return $ ResultClass i
      ClassMember this tp idx -> do
        typeCheck e etp tp
        tmp <- newLabel
        tmptp <- toLLVMType tp
        rlbl <- newLabel
        let tmpvar = LMLocalVar tmp (LMPointer tmptp)
            rvar = LMLocalVar rlbl tmptp
        return $ ResultCalc [Assignment rvar (Load tmpvar)
                            ,Assignment tmpvar
                             (GetElemPtr True this [ LMLitVar (LMIntLit i (LMInt 32)) | i <- [0,idx+1]])
                            ] rvar tp
      ClassMethod cls rtype argtps -> do
        this <- thisPointerM
        cm <- ask
        let ConstId _ (rname:_) = name
            entr = cm!cls
            fname = (BS.pack $ (Re.className entr) ++ "__" ++ rname)
        decl <- genFuncDecl fname rtype ((TypeId cls):argtps)
        return $ ResultMethod [] (LMGlobalVar fname (LMFunction decl) Internal Nothing Nothing False) this rtype argtps
        
      _ -> throwError [NotImplemented $ "Expression result "++show ref]
compileExpression _ e@(ExprBin op lexpr rexpr) etp = do
  (lextra,lres,tpl) <- compileExpression' "binary expressions" lexpr Nothing
  (rextra,rres,tpr) <- compileExpression' "binary expressions" rexpr (Just tpl)
  res <- newLabel
  case op of
    BinLess -> do
      typeCheck e etp TypeBool
      let resvar = LMLocalVar res (LMInt 1)
      return $ ResultCalc ([Assignment resvar (Compare LM_CMP_Slt lres rres)]++rextra++lextra) resvar TypeBool
    BinPlus -> do
      typeCheck e etp tpl
      llvmtp <- toLLVMType tpl
      let resvar = LMLocalVar res llvmtp
      return $ ResultCalc ([Assignment resvar (LlvmOp LM_MO_Add lres rres)]++rextra++lextra) resvar tpl
compileExpression pos e@(ExprAssign Assign lhs expr) etp = do
  (res,tp) <- compileAssign (position lhs) (posObj lhs)
  (extra,rvar,_) <- compileExpression' "assignment" expr (Just tp)
  case res of
    Left (name,_) -> do
      stackPut name rvar
      return $ ResultCalc extra rvar tp
    Right (tmpvar,stmts) -> do
      return $ ResultCalc ([Store rvar tmpvar]++stmts++extra) rvar tp
compileExpression _ e@(ExprCall (Pos expr rpos) args) etp = do
  res <- compileExpression rpos expr Nothing
  case res of
    ResultCalc eStmts eVar ftp -> case ftp of
      TypeFunction rtp argtp
          | (length argtp) == (length args) -> do
            typeCheck e etp rtp
            rargs <- zipWithM (\arg tp -> compileExpression' "function argument" arg (Just tp)) args argtp
            let call = Call StdCall eVar [ v | (_,v,_) <- rargs ] []
                rest = (concat [stmts | (stmts,_,_) <- rargs])++eStmts
            case rtp of
              TypeVoid -> return $ ResultCalc ([Expr call]++rest) (LMLitVar (LMUndefLit LMVoid)) rtp
              _ -> do
                res <- newLabel
                resvar <- toLLVMType rtp >>= return.(LMLocalVar res)
                return $ ResultCalc ([Assignment resvar call]++rest) resvar rtp
          | otherwise -> throwError [WrongNumberOfArguments e (length  args) (length argtp)]
      _ -> throwError [NotAFunction e ftp]
    ResultClass n -> do
      typeCheck e etp (TypeId n)
      classmap <- ask
      --let (name,int_name,funcs) = classmap!n
      let entr = classmap!n
          int_name = BS.pack $ Re.className entr
      lbl <- newLabel
      lbl_tmp1 <- newLabel
      lbl_tmp2 <- newLabel
      let resvar = LMLocalVar lbl (LMPointer (LMAlias int_name))
          vname = BS.pack $ "VTable__"++Re.className entr
          tmpvar1 = LMLocalVar lbl_tmp1 (LMPointer $ LMPointer $ LMInt 8)
          tmpvar2 = LMLocalVar lbl_tmp2 (LMPointer $ LMPointer $ LMAlias $ vname)
          stmts = [Store (LMGlobalVar (BS.pack $ "vtable__"++Re.className entr) (LMPointer $ LMAlias vname) Internal Nothing Nothing True) tmpvar2
                  ,Assignment tmpvar2 (Cast LM_Bitcast tmpvar1 (LMPointer $ LMPointer $ LMAlias vname))
                  ,Assignment tmpvar1 (GetElemPtr True resvar [ LMLitVar (LMIntLit 0 (LMInt 32))
                                                              , LMLitVar (LMIntLit 0 (LMInt 32))
                                                              ])
                  ,Assignment resvar (Malloc (LMAlias int_name) (LMLitVar $ LMIntLit 1 (LMInt 32)))]
      case etp of
        Nothing -> return $ ResultCalc stmts resvar (TypeId n)
        Just (TypeId n') -> if n == n'
                            then return $ ResultCalc stmts resvar (TypeId n)
                            else (do
                                     cast_lbl <- newLabel
                                     let entr2 = classmap!n'
                                         ttp = LMPointer (LMAlias $ BS.pack (Re.className entr2))
                                         cast_var = LMLocalVar cast_lbl ttp
                                         extra = Assignment cast_var (Cast LM_Bitcast resvar ttp)
                                     return $ ResultCalc (extra:stmts) cast_var (TypeId n'))
    ResultMethod eStmts fvar this rtp argtp
      | (length argtp) == (length args) -> do
        typeCheck e etp rtp
        rargs <- zipWithM (\arg tp -> compileExpression' "method argument" arg (Just tp)) args argtp
        let call = Call StdCall fvar (this:[ v | (_,v,_) <- rargs ]) []
            rest = (concat [stmts | (stmts,_,_) <- rargs])++eStmts
        case rtp of
          TypeVoid -> return $ ResultCalc ([Expr call]++rest) (LMLitVar (LMUndefLit LMVoid)) rtp
          _ -> do
            res <- newLabel
            resvar <- toLLVMType rtp >>= return.(LMLocalVar res)
            return $ ResultCalc ([Assignment resvar call]++rest) resvar rtp
      | otherwise -> throwError [WrongNumberOfArguments e (length  args) (length argtp)]
    ResultBuiltIn b -> do
      rargs <- mapM (\arg -> compileExpression' "builtin argument" arg Nothing) args
      (var,stmts,rtp) <- (builtIns!b) [ (var,tp) | (stmt,var,tp) <- rargs ]
      return $ ResultCalc (stmts++concat [ stmt | (stmt,_,_) <- rargs ]) var rtp
compileExpression pos e@(ExprAccess expr name) etp = do
  (extra,rvar,tp) <- compileExpression' "accessor" expr Nothing
  case tp of
    TypeId cid -> do
      cm <- ask
      let entr = cm!cid
      res <- lookupMember cid name
      case res of
        Left _ -> throwError [NoSuchMember pos (Re.className entr) name]
        Right (Left (rtp,idx)) -> do
          typeCheck e etp rtp
          tmp <- newLabel
          tmptp <- toLLVMType rtp
          rlbl <- newLabel
          let tmpvar = LMLocalVar tmp (LMPointer tmptp)
              resvar = LMLocalVar rlbl tmptp
          return $ ResultCalc ([Assignment resvar (Load tmpvar)
                               ,Assignment tmpvar
                                (GetElemPtr True rvar [ LMLitVar (LMIntLit i (LMInt 32)) | i <- [0,idx+1]])
                               ]++extra) resvar rtp
        Right (Right (ocid,rtp,args,idx)) -> do
          decl <- genFuncDecl BS.empty rtp (TypeId ocid:args)
          vtable_lbl <- newLabel
          rvtable_lbl <- newLabel
          table_lbl <- newLabel
          func_lbl <- newLabel
          func2_lbl <- newLabel
          let vtable_var = LMLocalVar vtable_lbl (LMPointer $ LMPointer (LMInt 8))
              table_tp = LMPointer (LMAlias $ BS.pack $ "VTable__"++Re.className entr)
              rvtable_var = LMLocalVar rvtable_lbl (LMPointer table_tp)
              table_var = LMLocalVar table_lbl table_tp
              func_var = LMLocalVar func_lbl (LMPointer $ LMPointer $ LMFunction decl)
              func2_var = LMLocalVar func2_lbl (LMPointer $ LMFunction decl)
              access = [Assignment func2_var (Load func_var)
                       ,Assignment func_var (GetElemPtr True table_var [ LMLitVar (LMIntLit i (LMInt 32)) | i <- [0,idx] ])
                       ,Assignment table_var (Load rvtable_var)
                       ,Assignment rvtable_var (Cast LM_Bitcast vtable_var (LMPointer table_tp))
                       ,Assignment vtable_var (GetElemPtr True rvar [ LMLitVar (LMIntLit i (LMInt 32)) | i <- [0,0] ])
                       ]
          if cid==ocid
            then return $ ResultMethod (access++extra) func2_var rvar rtp args
            else (do
                     cast_lbl <- newLabel
                     cast_tp <- toLLVMType (TypeId ocid)
                     let cast_var = LMLocalVar cast_lbl cast_tp
                         cast = Assignment cast_var (Cast LM_Bitcast rvar cast_tp)
                     return $ ResultMethod (cast:access++extra) func2_var cast_var rtp args)
compileExpression pos e@(ExprArray elems) etp = do
  let el_tp = case etp of
        Nothing -> Nothing
        Just tp -> case tp of
          TypeArray eel_tp -> Just $ Just eel_tp
          _ -> Just Nothing
  res1 <- case elems of
    [] -> return Nothing
    el:_ -> fmap Just $ compileExpression' "array element" el (case el_tp of
                                                                 Nothing -> Nothing
                                                                 Just Nothing -> Nothing
                                                                 Just tp -> tp)
  res1_tp <- case res1 of
    Nothing -> return Nothing
    Just (_,_,tp) -> return $ Just tp
  rel_tp <- case el_tp of
    Nothing -> case res1_tp of
      Nothing -> return TypeVoid
      Just tp -> return tp
    Just Nothing -> case res1_tp of
      Just tp -> throwError [TypeMismatch e (TypeArray tp) (let Just etp' = etp in etp')]
      Nothing -> throwError [TypeMismatch e (TypeArray TypeVoid) (let Just etp' = etp in etp')]
    Just (Just tp) -> return tp
  res_tp <- toLLVMType rel_tp
  {-res_lbl <- newLabel
  let rvar = LMLocalVar res_lbl (LMPointer res_tp)-}
  (rvar,alloc) <- sizedArrayMalloc (LMInt 32) res_tp (LMLitVar $ LMIntLit (fromIntegral $ length elems) (LMInt 32))
  args <- mapM (\(el,i) -> do
                  (stmts,resvar,_) <- compileExpression' "array element" el (Just rel_tp)
                  tmp_lbl <- newLabel
                  let tmp_var = LMLocalVar tmp_lbl (LMPointer res_tp)
                  return $ [Store resvar tmp_var
                           ,Assignment tmp_var (GetElemPtr True rvar [LMLitVar (LMIntLit idx (LMInt 32)) | idx <- [0,1,i]])
                           ]++stmts
              ) (zip elems [0..])
  return $ ResultCalc ((concat args) ++ alloc) rvar (TypeArray rel_tp)
compileExpression pos e@(ExprIndex expr idx) etp = do  
  (stmts_expr,resvar_expr,tp_expr) <- compileExpression' "indexed expression" expr Nothing
  (stmts_idx,resvar_idx,_) <- compileExpression' "index expression" idx (Just TypeInt)
  case tp_expr of
    TypeArray rtp -> do
      typeCheck e etp rtp
      tmp_lbl <- newLabel
      res_lbl <- newLabel
      rtp_llvm <- toLLVMType rtp
      let tmp_var = LMLocalVar tmp_lbl (LMPointer rtp_llvm)
          res_var = LMLocalVar res_lbl rtp_llvm
      return $ ResultCalc ([Assignment res_var (Load tmp_var)
                           ,Assignment tmp_var (GetElemPtr False resvar_expr [LMLitVar (LMIntLit 0 (LMInt 32))
                                                                             ,LMLitVar (LMIntLit 1 (LMInt 32))
                                                                             ,resvar_idx])
                           ]++stmts_idx++stmts_expr) res_var rtp
compileExpression pos e@(ExprString str) etp = do
  typeCheck e etp TypeString
  (n,st,strs) <- get
  strid <- case Map.lookup str strs of
    Nothing -> do
      let sz = fromIntegral $ Map.size strs
      put (n,st,Map.insert str sz strs)
      return sz
    Just i -> return i
  cast_lbl <- newLabel
  let orig_var = LMGlobalVar (BS.pack $ "str"++show strid) (LMPointer $ LMStruct [LMInt 32,LMArray (length str) (LMInt 8)]) Internal Nothing Nothing True
      cast_tp = LMPointer $ LMStruct [LMInt 32,LMArray 0 (LMInt 8)]
      cast_var = LMLocalVar cast_lbl cast_tp
  return $ ResultCalc [Assignment cast_var (Cast LM_Bitcast orig_var cast_tp)] cast_var TypeString
compileExpression _ expr _ = error $ "Couldn't compile expression "++show expr
  

compileAssign :: p -> Expression p -> Compiler (Either (String,Maybe LlvmVar) (LlvmVar,[LlvmStatement]),RType) p
compileAssign pos (ExprId cid) = do
  (ref,_) <- stackLookupM (Just pos) cid
  case ref of
    Variable tp var -> do
      let ConstId _ (name:_) = cid
      return (Left (name,var),tp)
    ClassMember var tp idx -> do
      tmp_lbl <- newLabel
      res_lbl <- newLabel
      rtp <- toLLVMType tp
      let tmp_var = LMLocalVar tmp_lbl (LMPointer rtp)
      return (Right (tmp_var,[Assignment tmp_var (GetElemPtr True var [LMLitVar (LMIntLit 0 (LMInt 32))
                                                                      ,LMLitVar (LMIntLit (idx+1) (LMInt 32))
                                                                      ])
                             ]),tp)
    _ -> throwError [NotImplemented $ "Assigning to "++show ref]
compileAssign pos (ExprAccess expr name) = do
  (res,rtp) <- compileAssign (position expr) (posObj expr)
  (cvar,stmts) <- case res of
    Left (name,var) -> case var of
      Just rvar -> return (rvar,[])
      Nothing -> throwError [UninitializedVariable pos (ConstId False [name])]
    Right r -> return r
  case rtp of
    TypeId cid -> do
      classmap <- ask
      let entr = classmap!cid
      res <- lookupMember cid name
      case res of
        Left _ -> throwError [NoSuchMember pos (Re.className entr) name]
        Right (Right _) -> throwError [NoSuchMember pos (Re.className entr) name]
        Right (Left (ntp,idx)) -> do
          tmp <- newLabel
          tmptp <- toLLVMType ntp
          let tmpvar = LMLocalVar tmp (LMPointer tmptp)
          return (Right (tmpvar,[Assignment tmpvar
                                 (GetElemPtr True cvar [ LMLitVar (LMIntLit i (LMInt 32)) | i <- [0,idx+1]])
                                ]++stmts),ntp)
compileAssign pos (ExprIndex expr idx) = do
  (res,rtp) <- compileAssign (position expr) (posObj expr)
  (cvar,stmts) <- case res of
    Left (name,var) -> case var of
      Just rvar -> return (rvar,[])
      Nothing -> throwError [UninitializedVariable pos (ConstId False [name])]
    Right r -> return r
  (idxstmts,idxvar,_) <- compileExpression' "index" idx (Just TypeInt)
  case rtp of
    TypeArray eltp -> do
      tmp_lbl <- newLabel
      tmp_tp <- toLLVMType eltp
      let tmp_var = LMLocalVar tmp_lbl (LMPointer tmp_tp)
      return (Right (tmp_var,[Assignment tmp_var
                              (GetElemPtr False cvar [ LMLitVar (LMIntLit 0 (LMInt 32))
                                                     , LMLitVar (LMIntLit 1 (LMInt 32))
                                                     , idxvar ])
                             ]++stmts++idxstmts),eltp)

sizedArrayMalloc :: LlvmType -> LlvmType -> LlvmVar -> Compiler (LlvmVar,[LlvmStatement]) p
sizedArrayMalloc idx_tp el_tp len_var = do
  let tp = LMStruct [idx_tp,LMArray 0 el_tp]
  size_lbl <- newLabel
  sizeu_lbl <- newLabel
  ptr_lbl <- newLabel
  res_lbl <- newLabel
  arrlen_lbl <- newLabel
  let size_var = LMLocalVar size_lbl (LMPointer el_tp)
      sizeu_var = LMLocalVar sizeu_lbl (LMInt 32)
      ptr_var = LMLocalVar ptr_lbl (LMPointer (LMInt 8))
      res_var = LMLocalVar res_lbl (LMPointer tp)
      arrlen_var = LMLocalVar arrlen_lbl (LMPointer idx_tp)
  return (res_var,[Store len_var arrlen_var 
                  ,Assignment arrlen_var (GetElemPtr True res_var [ LMLitVar (LMIntLit 0 (LMInt 32))
                                                                  , LMLitVar (LMIntLit 0 (LMInt 32))])
                  ,Assignment res_var (Cast LM_Bitcast ptr_var (LMPointer tp))
                  ,Assignment ptr_var (Malloc (LMInt 8) sizeu_var)
                  ,Assignment sizeu_var (Cast LM_Ptrtoint size_var (LMInt 32))
                  ,Assignment size_var (GetElemPtr False (LMLitVar $ LMNullLit (LMPointer tp)) [ LMLitVar (LMIntLit 0 (LMInt 32))
                                                                                               , LMLitVar (LMIntLit 1 (LMInt 32))
                                                                                               , len_var
                                                                                               ])
                  ])

checkSubtyping :: Integer -> Integer -> Compiler Bool p
checkSubtyping par ch
  | par == ch  = return True
  | otherwise = do
    cm <- ask
    let entr = cm!ch
    res <- mapM (\i -> checkSubtyping par i) (Set.toList $ Re.classInherits entr)
    return $ or res

typeCheck :: Expression p -> Maybe RType -> RType -> Compiler () p
typeCheck expr Nothing act = return ()
typeCheck expr (Just req@(TypeFunction TypeVoid args)) act@(TypeFunction _ eargs)
  | args == eargs = return ()
  | otherwise    = throwError [TypeMismatch expr act req]
typeCheck expr (Just (TypeId c1)) (TypeId c2) = do
  res <- checkSubtyping c1 c2
  if res then return () else throwError [TypeMismatch expr (TypeId c1) (TypeId c2)]
  
typeCheck expr (Just req) act
  | req == act = return ()
  | otherwise = throwError [TypeMismatch expr act req]
              
stackPushBlock :: Compiler () p
stackPushBlock = modify (\(n,st,strs) -> (n,LocalContext st Map.empty BlockContext,strs))

stackPushLoop :: Integer -> Integer -> Integer -> Compiler () p
stackPushLoop test start end = modify $ \(n,st,strs) -> (n,LocalContext st Map.empty (LoopContext test start end []),strs)

stackPushFunction :: String -> RType -> [(String,RType)] -> Compiler () p
stackPushFunction name rtype args = do
  vars <- mapM (\(n,tp) -> do
                   rtp <- toLLVMType tp
                   return (n,tp,LMNLocalVar (BS.pack n) rtp)
               ) args
  modify $ \(n,st,strs) -> (n,LocalContext st Map.empty (FunctionContext name rtype vars),strs)

stackPushMethod :: String -> RType -> [(String,RType)] -> LlvmVar -> Compiler () p
stackPushMethod name rtype args this = do
  vars <- mapM (\(n,tp) -> do
                   rtp <- toLLVMType tp
                   return (n,tp,LMNLocalVar (BS.pack n) rtp)
               ) args
  modify $ \(n,st,strs) -> (n,LocalContext st Map.empty (MethodContext name this rtype vars),strs)

stackPushClass :: Integer -> Compiler () p
stackPushClass cid = do
  classmap <- ask
  let entr = classmap!cid
  memb <- getInheritedMembers entr
  modify $ \(n,st,strs) -> (n,ClassContext (Re.className entr) cid memb (Map.fromList $ Re.classMethods entr) (Re.classInners entr) (Re.classInherits entr) st,strs)

getInheritedMembers :: ClassMapEntry -> Compiler [(String,RType)] p
getInheritedMembers entr = do
  cm <- ask
  inh <- mapM (\i -> getInheritedMembers (cm!i)) (Set.toList $ Re.classInherits entr)
  return $ concat inh ++ (Re.classVariables entr)

stackAlloc :: String -> RType -> Compiler () p
stackAlloc name tp = modify $ \(n,st,strs) -> (n,stackAllocVar name tp st,strs)

stackPut :: String -> LlvmVar -> Compiler () p
stackPut name cont = do
  (n,st,strs) <- get
  case stackUpdateVar name cont st of
    Nothing -> throwError [InternalError $ "Variable "++name++" couldn't be updated"]
    Just nst -> put (n,nst,strs)

mbDefault :: RType -> Maybe LlvmVar -> Compiler LlvmVar p
mbDefault _ (Just var) = return var
mbDefault TypeInt Nothing = do
  rtp <- toLLVMType TypeInt
  return $ LMLitVar (LMIntLit 0 rtp)

mbDefault' :: ClassMap -> RType -> Maybe LlvmVar -> LlvmVar
mbDefault' _ _ (Just var) = var
mbDefault' cm TypeInt Nothing = LMLitVar (LMIntLit 0 (toLLVMType' cm TypeInt))

writes :: [Statement p] -> Set ConstantIdentifier
writes xs = writes' xs Set.empty
  where
    writes' :: [Statement p] -> Set ConstantIdentifier -> Set ConstantIdentifier
    writes' [] s = s
    writes' (x:xs) s = writes' xs (writes'' x s)

    writes'' :: Statement p -> Set ConstantIdentifier -> Set ConstantIdentifier
    writes'' (StmtBlock stmts) s = writes' (fmap posObj stmts) s
    writes'' (StmtExpr expr) s = writes''' (posObj expr) s
    writes'' (StmtDecl name _ (Just expr)) s = writes''' (posObj expr) (Set.insert (ConstId False [name]) s)
    writes'' (StmtIf cond (Pos ifTrue _) ifFalse) s = writes''' (posObj cond) (writes'' ifTrue (case ifFalse of
                                                                                                   Nothing -> s
                                                                                                   Just (Pos e _) -> writes'' e s))
    writes'' (StmtReturn (Just expr)) s = writes''' (posObj expr) s
    writes'' (StmtFor init cond it body) s = let s1 = case init of
                                                   Nothing -> s
                                                   Just (Left (Pos r1 _)) -> writes''' r1 s
                                                   Just (Right (_,_,Pos r1 _)) ->  writes''' r1 s
                                                 s2 = case cond of
                                                   Nothing -> s1
                                                   Just (Pos r2 _) -> writes''' r2 s1
                                                 s3 = case it of
                                                   Nothing -> s2
                                                   Just (Pos r3 _) -> writes''' r3 s2
                                             in writes' (fmap posObj body) s3
    writes'' _ s = s
    
    writes''' :: Expression p -> Set ConstantIdentifier -> Set ConstantIdentifier
    writes''' (ExprAssign _ lhs (Pos rhs _)) s = case posObj lhs of
                                                      ExprId tid -> writes''' rhs (Set.insert tid s)
                                                      _ -> writes''' rhs s
    writes''' (ExprCall cmd args) s = foldl (\s' e -> writes''' e s') s (fmap posObj (cmd:args))
    writes''' (ExprBin _ (Pos lhs _) (Pos rhs _)) s = writes''' rhs (writes''' lhs s)
    writes''' (ExprIndex (Pos lhs _) (Pos rhs _)) s = writes''' rhs (writes''' rhs s)
    writes''' (ExprLambda _ (Pos stmt _)) s = writes'' stmt s
    writes''' _ s = s

newPhiVars :: Ord a => Set a -> Compiler (Map a Integer) p
newPhiVars vars = do
  res <- mapM (\var -> do
                  lbl <- newLabel
                  return (var,lbl)) (Set.toAscList vars)
  return $ Map.fromAscList res

type BuiltIn p = [(LlvmVar,RType)] -> Compiler (LlvmVar,[LlvmStatement],RType) p

builtIns :: Map ConstantIdentifier (BuiltIn p)
builtIns = Map.fromList [(ConstId False ["sizeof"],sizeOf),(ConstId False ["write"],write)]

sizeOf :: BuiltIn p
sizeOf [(arg,tp)] = case tp of
  TypeArray el_tp -> do
    rel_tp <- toLLVMType el_tp
    res_lbl <- newLabel
    tmp_lbl <- newLabel
    let res_var = LMLocalVar res_lbl (LMInt 32)
        tmp_var = LMLocalVar tmp_lbl (LMPointer $ LMInt 32)
    return (res_var,[Assignment res_var (Load tmp_var)
                    ,Assignment tmp_var (GetElemPtr True arg [ LMLitVar $ LMIntLit 0 (LMInt 32)
                                                             , LMLitVar $ LMIntLit 0 (LMInt 32)
                                                             ])
                    ],TypeInt)

write :: BuiltIn p
write [(arg,tp)] = case tp of
  TypeString -> do
    sizeptr_lbl <- newLabel
    size_lbl <- newLabel
    ptr_lbl <- newLabel
    let sizeptr_var = LMLocalVar sizeptr_lbl (LMPointer $ LMInt 32)
        size_var = LMLocalVar size_lbl (LMInt 32)
        ptr_var = LMLocalVar ptr_lbl (LMPointer $ LMInt 8)
    return (LMLitVar (LMUndefLit LMVoid),
            [Expr $ Call StdCall (LMGlobalVar (BS.pack "write") (LMFunction write_decl) External Nothing Nothing True) [LMLitVar (LMIntLit 1 (LMInt 32))
                                                                                                                       ,ptr_var
                                                                                                                       ,size_var] []
            ,Assignment ptr_var (GetElemPtr True arg [ LMLitVar $ LMIntLit 0 (LMInt 32)
                                                     , LMLitVar $ LMIntLit 1 (LMInt 32)
                                                     , LMLitVar $ LMIntLit 0 (LMInt 32)
                                                     ])
            ,Assignment size_var (Load sizeptr_var)
            ,Assignment sizeptr_var (GetElemPtr True arg [ LMLitVar $ LMIntLit 0 (LMInt 32)
                                                         , LMLitVar $ LMIntLit 0 (LMInt 32)
                                                         ])
            ],TypeVoid)
     
        
    

type RecLookupResult = Either (Integer,Integer,Set String) (Either (RType,Integer) (Integer,RType,[RType],Integer))

lookupMember' :: [Integer] -> String -> Compiler RecLookupResult p
lookupMember' [] _ = return $ Left (0,0,Set.empty)
lookupMember' (x:xs) name = do
  res <- lookupMember x name
  case res of
    Right rr -> return $ Right rr
    Left (off,off2,methods) -> do
      nres <- lookupMember' xs name
      case nres of
        Left (noff,noff2,methods2) -> return $ Left (off+noff,off2+noff2,Set.union methods methods2)
        Right rr -> case rr of
          Left (tp,noff) -> return $ Right $ Left (tp,off+noff)
          Right (cid,rtp,argtp,noff) -> return $ Right $ Right (cid,rtp,argtp,off2+noff)

lookupMember :: Integer -> String -> Compiler RecLookupResult p
lookupMember cid name = do
  cm <- ask
  let entr = cm!cid
  inh <- lookupMember' (Set.toList $ Re.classInherits entr) name
  case inh of
    Right res -> return $ Right res
    Left (off,off2,methods) -> case lookupWithIndex name (classVariables entr) of
      Just (rtp,idx) -> return $ Right $ Left (rtp,idx+off)
      Nothing -> let nmethods = List.filter (\(fname,_) -> not $ Set.member fname methods) $ Re.classMethods entr
                 in case lookupWithIndex name nmethods of
                   Nothing -> return $ Left (fromIntegral $ length (classVariables entr),fromIntegral $ length nmethods,Set.fromList (fmap fst nmethods))
                   Just ((rtp,argtp),idx) -> return $ Right $ Right (cid,rtp,argtp,idx)