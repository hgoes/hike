{-# LANGUAGE DeriveFunctor,FlexibleContexts #-}
module Language.Pike.Syntax where

data Pos a p = Pos
               { posObj :: (a p)
               , position :: p
               }

instance Show (a p) => Show (Pos a p) where
  show (Pos x _) = show x

data Definition p = Definition [Modifier] (DefinitionBody p) p
                  deriving Show

data DefinitionBody p
    = Import (Either ConstantIdentifier String)
    | FunctionDef String Type [(String,Type)] [Pos Statement p]
    | ClassDef String [(String,Type)] [Definition p]
    | VariableDef Type [String]
    deriving Show

data Modifier
    = Extern
    | Final
    | Inline
    | Local
    | NoMask
    | Optional
    | Private
    | Protected
    | Public
    | Static
    | Variant
    deriving Show

type Type = PType ConstantIdentifier

type RType = PType Integer

data PType a = TypeInt
             | TypeString
             | TypeFloat
             | TypeProgram
             | TypeVoid
             | TypeId a
             | TypeArray (PType a)
             | TypeBool
             | TypeFunction (PType a) [PType a]
             deriving (Show,Eq,Functor)

data ConstantIdentifier = ConstId Bool [String]
                        deriving (Show,Eq,Ord)

data Statement p
    = StmtBlock [Pos Statement p]
    | StmtExpr (Pos Expression p)
    | StmtDecl String Type (Maybe (Pos Expression p))
    | StmtIf (Pos Expression p) (Pos Statement p) (Maybe (Pos Statement p))
    | StmtReturn (Maybe (Pos Expression p))
    | StmtWhile (Pos Expression p) [Pos Statement p]
    | StmtFor
      (Maybe (Either (Pos Expression p) (Type,String,Pos Expression p)))
      (Maybe (Pos Expression p))
      (Maybe (Pos Expression p)) [Pos Statement p]
    | StmtBreak
    deriving Show

data Expression p
    = ExprId ConstantIdentifier
    | ExprAssign AssignType (Pos Expression p) (Pos Expression p)
    | ExprCall (Pos Expression p) [Pos Expression p]
    | ExprString String
    | ExprInt Integer
    | ExprBin BinOp (Pos Expression p) (Pos Expression p)
    | ExprAccess (Pos Expression p) String
    | ExprIndex (Pos Expression p) (Pos Expression p)
    | ExprLambda [(String,Type)] (Pos Statement p)
    | ExprArray [Pos Expression p]
    deriving Show

data LValue
  = LVId ConstantIdentifier
  | LVAccess LValue String
  deriving Show

data AssignType
    = Assign
    | AssignPlus
    deriving Show

data BinOp
    = BinPlus
    | BinEqual
    | BinLess
    deriving Show