module AST (

    Modifier(..)
  , BasicType(..)
  , Type(..)
  , Identifier
  , FormalParameter(..)
  , Statement(..)
  , Method(..)
  
) where


data Modifier
    = Mstrictfp
    | Mpublic
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

data Method
    = Method {
        modifiers        :: [Modifier],
        typeParameters   :: [String],
        returnType       :: Maybe Type,
        name             :: String,
        formalParameters :: [FormalParameter],
        body             :: Statement
    }
  deriving (Show, Eq)
