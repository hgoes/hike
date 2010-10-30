module Language.Pike.Syntax where

data Definition = Definition [Modifier] DefinitionBody
                deriving Show

data Signature = Sig Type [(String,Type)] deriving Show

data DefinitionBody
    = Import (Either ConstantIdentifier String)
    | FunctionDef String Type [(String,Type)] [Statement]
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

data Type = TypeInt
          | TypeString
          | TypeFloat
          | TypeProgram
          | TypeVoid
          | TypeId ConstantIdentifier
          | TypeArray Type
          | TypeBool
          | TypeFunction Type [Type]
          deriving (Show,Eq)

data ConstantIdentifier = ConstId Bool [String]
                        deriving (Show,Eq)

data Statement
    = StmtBlock [Statement]
    | StmtExpr Expression
    | StmtDecl String Type (Maybe Expression)
    | StmtIf Expression Statement (Maybe Statement)
    | StmtReturn (Maybe Expression)
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