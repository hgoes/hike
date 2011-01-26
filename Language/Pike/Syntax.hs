{-# LANGUAGE DeriveFunctor #-}
module Language.Pike.Syntax where

data Definition = Definition [Modifier] DefinitionBody
                deriving Show

data DefinitionBody
    = Import (Either ConstantIdentifier String)
    | FunctionDef String Type [(String,Type)] [Statement]
    | ClassDef String [(String,Type)] [Definition]
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

data Statement
    = StmtBlock [Statement]
    | StmtExpr Expression
    | StmtDecl String Type (Maybe Expression)
    | StmtIf Expression Statement (Maybe Statement)
    | StmtReturn (Maybe Expression)
    | StmtWhile Expression [Statement]
    | StmtFor (Maybe Expression) (Maybe Expression) (Maybe Expression) [Statement]
    | StmtBreak
    deriving Show

data Expression
    = ExprId ConstantIdentifier
    | ExprAssign AssignType ConstantIdentifier Expression
    | ExprCall Expression [Expression]
    | ExprString String
    | ExprInt Integer
    | ExprBin BinOp Expression Expression
    | ExprIndex Expression Expression
    | ExprLambda [(String,Type)] Statement
    deriving Show

data AssignType
    = Assign
    | AssignPlus
    deriving Show

data BinOp
    = BinPlus
    | BinEqual
    | BinAccess
    | BinLess
    deriving Show