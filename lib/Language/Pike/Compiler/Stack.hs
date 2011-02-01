module Language.Pike.Compiler.Stack where

import Language.Pike.Syntax
import Data.Map as Map
import Data.List (find)

data StackReference ref = Variable RType (Maybe ref)
                        | GlobalVariable RType String
                        | Class Integer
                        | ClassMember Integer RType Integer
                        | Function RType [RType]
                        | ClassMethod Integer RType [RType]
                        deriving Show

data Stack ref = GlobalContext
                 { globalVars      :: Map String RType
                 , globalFunctions :: Map String (RType,[RType])
                 , globalClasses   :: Map String Integer
                 }
               | ClassContext
                 { className    :: String
                 , classId      :: Integer
                 , classMembers :: [(String,RType)]
                 , classMethods :: Map String (RType,[RType])
                 , classClasses :: Map String Integer
                 , classUpper   :: Stack ref
                 }
               | LocalContext
                 { localUp   :: Stack ref
                 , localVars :: Map String (RType,Maybe ref)
                 , localType :: LocalType ref
                 }
               deriving Show

data LocalType ref = FunctionContext
                     { functionName       :: String
                     , functionReturnType :: RType
                     , functionArgs       :: [(String,RType,ref)]
                     }
                   | LoopContext
                     { loopTest        :: Integer
                     , loopStart       :: Integer
                     , loopBreak       :: Integer
                     , loopBreakpoints :: [(Integer,Stack ref)]
                     }
                   | BlockContext
                   deriving Show

stackLookup :: ConstantIdentifier -> Stack r -> Maybe (StackReference r,Integer)
stackLookup (ConstId _ (name:_)) = stackLookup' name

stackAllocVar :: String -> RType -> Stack r -> Stack r
stackAllocVar name tp ctx = ctx { localVars = Map.insert name (tp,Nothing) (localVars ctx) }

stackUpdateVar :: String -> r -> Stack r -> Maybe (Stack r)
stackUpdateVar name cont ctx@(LocalContext { localUp = up
                                           , localVars = vars
                                           , localType = tp
                                           }) = case tp of
  FunctionContext { functionArgs = args } -> case updateArgs name cont args of
    Just nargs -> Just $ ctx { localType = tp { functionArgs = nargs } }
    Nothing -> rest
  _ -> rest
  where
    rest = case Map.updateLookupWithKey (\_ (tp,_) -> Just (tp,Just cont)) name vars of
      (Just _,nvars) -> Just $ ctx { localVars = nvars }
      (Nothing,_) -> case stackUpdateVar name cont up of
        Nothing -> Nothing
        Just nup -> Just $ ctx { localUp = nup }
stackUpdateVar name cont ctx@(ClassContext { classUpper = up }) = case stackUpdateVar name cont up of
  Nothing -> Nothing
  Just nup -> Just $ ctx { classUpper = nup }
stackUpdateVar name cont (GlobalContext {}) = Nothing

stackUpdateVars :: (String -> RType -> Maybe r -> Maybe r) -> Stack r -> Stack r
stackUpdateVars f ctx@(GlobalContext {}) = ctx
stackUpdateVars f ctx@(ClassContext { classUpper = up }) = ctx { classUpper = stackUpdateVars f up }
stackUpdateVars f ctx@(LocalContext { localUp = up
                                    , localVars = vars
                                    , localType = tp
                                    }) = ctx { localUp = stackUpdateVars f up
                                             , localVars = Map.mapWithKey (\name (tp,ref) -> (tp,case f name tp ref of
                                                                                                 Nothing -> ref
                                                                                                 Just nref -> Just nref)) (localVars ctx)
                                             , localType = case tp of
                                               FunctionContext {} -> tp { functionArgs = fmap (\(name,tp,ref) -> (name,tp,case f name tp (Just ref) of
                                                                                                                     Nothing -> ref
                                                                                                                     Just nref -> nref)
                                                                                              ) (functionArgs tp) }
                                               _ -> tp
                                             }

stackLookup' :: String -> Stack r -> Maybe (StackReference r,Integer)
stackLookup' name (GlobalContext { globalVars = vars
                                 , globalFunctions = funcs
                                 , globalClasses = cls
                                 }) = case Map.lookup name vars of
  Just tp -> Just (GlobalVariable tp name,0)
  Nothing -> case Map.lookup name funcs of
    Just (ret,args) -> Just (Function ret args,0)
    Nothing -> case Map.lookup name cls of
      Just i -> Just (Class i,0)
      Nothing -> Nothing
stackLookup' name (ClassContext { classId = i
                                , classMembers = vars
                                , classMethods = funcs
                                , classClasses = cls
                                , classUpper = up
                                }) = case findWithIndex (\(n,_) -> n == name) vars of
  Just ((_,tp),idx) -> Just (ClassMember i tp idx,0)
  Nothing -> case Map.lookup name funcs of
    Just (ret,args) -> Just (ClassMethod i ret args,0)
    Nothing -> case Map.lookup name cls of
      Just cid -> Just (Class cid,0)
      Nothing -> stackLookup' name up
stackLookup' name (LocalContext { localUp = up
                                , localVars = vars
                                , localType = tp
                                }) = case tp of
  FunctionContext { functionArgs = args } -> case find (\(n,_,_) -> n == name) args of
    Just (_,tp,v) -> Just (Variable tp (Just v),0)
    Nothing -> rest
  _ -> rest
  where
    rest = case Map.lookup name vars of
      Just (tp,v) -> Just (Variable tp v,0)
      Nothing -> case stackLookup' name up of
        Just (ref,n) -> Just (ref,n+1)
        Nothing -> Nothing

findWithIndex :: (a -> Bool) -> [a] -> Maybe (a,Integer)
findWithIndex f = findWithIndex' 0
  where
    findWithIndex' _ [] = Nothing
    findWithIndex' n (x:xs) = if f x
                              then Just (x,n)
                              else findWithIndex' (n+1) xs

updateArgs :: String -> ref -> [(String,RType,ref)] -> Maybe [(String,RType,ref)]
updateArgs str ref [] = Nothing
updateArgs str ref (x@(name,tp,_):xs) = if str == name
                                        then Just ((name,tp,ref):xs)
                                        else (case updateArgs str ref xs of
                                                 Nothing -> Nothing
                                                 Just up -> Just $ x:up)

stackPop' :: Stack r -> Maybe (Stack r)
stackPop' (GlobalContext {}) = Nothing
stackPop' (ClassContext { classUpper = up}) = Just up
stackPop' (LocalContext { localUp = up}) = Just up

stackPopTill :: (Stack r -> Bool) -> Stack r -> Maybe (Stack r)
stackPopTill f st = if f st
                    then Just st
                    else (case st of
                             GlobalContext {} -> Nothing
                             ClassContext { classUpper = up } -> stackPopTill f up
                             LocalContext { localUp = up } -> stackPopTill f up)

addBreak :: Integer -> Stack r -> Stack r -> Maybe (Stack r)
addBreak lbl state (GlobalContext {}) = Nothing
addBreak lbl state ctx@(ClassContext {}) = case addBreak lbl state (classUpper ctx) of
  Nothing -> Nothing
  Just nst -> Just $ ctx { classUpper = nst }
addBreak lbl state ctx@(LocalContext {}) = case localType ctx of
  LoopContext {} -> Just $ ctx { localType = (localType ctx) { loopBreakpoints = (lbl,state):(loopBreakpoints (localType ctx)) } }
  _ -> case addBreak lbl state (localUp ctx) of
    Nothing -> Nothing
    Just nst -> Just $ ctx { localUp = nst }

returnType :: Stack r -> Maybe RType
returnType (GlobalContext {}) = Nothing
returnType (ClassContext { classUpper = up }) = returnType up
returnType (LocalContext { localUp = up
                         , localType = tp
                         }) = case tp of
  FunctionContext { functionReturnType = rtp } -> Just rtp
  _ -> returnType up

breakLabel :: Stack r -> Maybe Integer
breakLabel (GlobalContext {}) = Nothing
breakLabel (ClassContext { classUpper = up }) = breakLabel up
breakLabel (LocalContext { localUp = up
                         , localType = tp
                         }) = case tp of
  LoopContext { loopBreak = lbl } -> Just lbl
  _ -> breakLabel up
