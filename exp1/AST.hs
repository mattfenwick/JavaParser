module AST (

    Modifier(..)
  , BasicType(..)
  , Type(..)
  , ReturnType(..)
  , Identifier
  , FormalParameter(..)
  , Statement(..)
  
) where


data Modifier
    = Mstrictfp
    | MPublic
    | Mprotected
    | Mprivate
    | Mstatic
    | Mabstract
    | Mfinal 
    | Mnative
    | Msynchronized
    | Mtransient
    | Mvolatile
  deriving (Show, Eq)
  
data BasicType
    = Tbyte
    | Tshort
    | Tchar
    | Tint
    | Tlong
    | Tfloat
    | Tdouble
    | Tboolean
    | RefType [(String, [Type])]
  deriving (Show, Eq)
  
data Type
    = Type BasicType Int
  deriving (Show, Eq)
  
data ReturnType
    = RTType Type
    | RTVoid
  deriving (Show, Eq)

type Identifier = String

data FormalParameter
    = FormalParameter {
        getModifiers :: [Modifier],
        getType      :: Type,
        getName      :: Identifier
    }
  deriving (Show, Eq)

data Statement
    = SReturn (Maybe Identifier)
  deriving (Show, Eq)
