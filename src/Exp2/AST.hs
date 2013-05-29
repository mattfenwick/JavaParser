module Exp2.AST (

    Modifier(..)
  , BasicType(..)
  , Type(..)
  , Identifier
  , FormalParameter(..)
  , Statement(..)
  , Method(..)
  , TypeArgument(..)
  , TypeParameter(..)
  
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

data TypeArgument
    = Wild
    | WildSuper Type
    | WildExtends Type
    | TASimple Type
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
    | RefType [(String, [TypeArgument])]
  deriving (Show, Eq)
  
data Type
    = Type BasicType Int
  deriving (Show, Eq)
  
data TypeParameter
    = TypeParameter {
      getIdentifier :: Identifier,
      getBounds     :: [Type]
    }
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
        typeParameters   :: [TypeParameter],
        returnType       :: Maybe Type,
        name             :: String,
        formalParameters :: [FormalParameter],
        body             :: Statement
    }
  deriving (Show, Eq)
