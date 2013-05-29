
-- Type in the sense of A<B,C>.D, not in the sense of a TypeDeclaration

data BaseType
    = Byte
    | Short
    | Char
    | Int
    | Long
    | Float
    | Double
    | Boolean
    = RefType [(String, [TypeArgument])]  -- outer list must have >= 1 elem, each inner list can have any number
    deriving (Show, Eq)

data TypeArgument
    = TASimple Type
    | TAWild
    | TAExtends [Type]
    | TASuper Type
    deriving (Show, Eq)
    
data Type 
    = Type {
        getBaseType :: BaseType,
        getArrayLevels :: Int 
    } deriving (Show, Eq)
