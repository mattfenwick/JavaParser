
-- Type in the sense of A<B,C>.D, not in the sense of a TypeDeclaration

data PrimitiveType
    = Byte
    | Short
    | Char
    | Int
    | Long
    | Float
    | Double
    | Boolean
    deriving (Show, Eq)

data ReferenceType
    = RPrim PrimitiveType Int
    | RRef [(String, [TypeArgument])]  -- outer list must have >= 1 elem, inner list can have any number
    deriving (Show, Eq)
    
data TypeArgument
    = TARef ReferenceType
    | TAWild
    | TAExtends ReferenceType
    | TASuper ReferenceType
    deriving (Show, Eq)
    
data Type 
    = TPrim PrimitiveType
    | TRef ReferenceType
    deriving (Show, Eq)


-- under construction !!!

{-

data CompilationUnit
    = CU {
        annotations :: [Annotation],
        packageName :: Maybe QIdent,
        imports     :: [Import],
        types       :: [([Modifier], Type)]
    } deriving (Show, Eq)

data Import
    = Import {
        isStatic :: Bool,
        name :: QIdent,
        isWildCard :: Bool
    } deriving (Show, Eq)

data TypeDeclaration
    = TClass Class
    | TEnum Enum
    | TInterface Interface 
    | TAnnotation Annotation
  deriving (Show, Eq)

data Class
    = Class {
        name    :: String, 
        typeParams :: [TypeParameter],
        extends :: Type,
        implements :: [Type],
        fields  :: [Field],
        methods :: [Method],
        constructors :: [Method],
        blocks  :: [Block],
        types   :: [TypeDeclaration]
    } deriving (Show, Eq)
    
data Enum
    = Enum {
        name       :: String,
        implements :: [Type],
        constants  :: [EnumConstants],
        fields     :: [Field],
        methods    :: [Method],
        constructors :: [Method],
        blocks     :: [Block],
        types      :: [TypeDeclaration]
    } deriving (Show, Eq)
    
data Interface
    = Interface {
        name       :: String,
        typeParams :: [TypeParameter],
        extends    :: [Type],
        constants  :: [IField],
        methods    :: [IMethod],
        types      :: [TypeDeclaration]
    } deriving (Show, Eq)
    
data Annotation
    = Annotation {
        name     :: String,
        methods  :: [AMethod],
        constants :: [IField],
        types    :: [TypeDeclaration]
    } deriving (Show, Eq)

-}        