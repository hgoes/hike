module Language.Pike.Syntax where

data Definition = Definition [Modifier] DefinitionBody
                deriving Show

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
          deriving Show

data ConstantIdentifier = ConstId Bool [String]
                        deriving Show

data Statement
    = StmtExpr Expression
    | StmtDecl String Type (Maybe Expression)
    | StmtIf Expression [Statement] (Maybe [Statement])
    | StmtReturn (Maybe Expression)
    deriving Show

data Expression
    = ExprId ConstantIdentifier
    | ExprAssign AssignType ConstantIdentifier Expression
    | ExprCall String [Expression]
    | ExprString String
    | ExprInt Integer
    | ExprBin BinOp Expression Expression
    | ExprIndex Expression Expression
    deriving Show

data AssignType
    = Assign
    | AssignPlus
    deriving Show

data BinOp
    = BinPlus
    | BinEqual
    | BinAccess
    deriving Show