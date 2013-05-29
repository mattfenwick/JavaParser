module Tokens (
    
    Keyword(..)
  , Literal(..)
  , LInteger(..)
  , LFloating(..)
  , Separator(..)
  , Operator(..)
  , Token(..)
  , InputElement(..)
  
  , stringToKeyword

) where

import qualified Data.Map


data Keyword
  = Kdouble
  | Kthrows
  | Kfinally
  | Kinterface
  | Kabstract
  | Kcontinue
  | Kfor
  | Knew
  | Kswitch
  | Kassert
  | Kdefault
  | Kif
  | Kpackage
  | Ksynchronized
  | Kbreak
  | Kimplements
  | Kprotected
  | Kthrow
  | Kboolean
  | Kdo
  | Kgoto
  | Kprivate
  | Kthis
  | Kbyte
  | Kelse
  | Kimport
  | Kpublic
  | Kcase
  | Kenum
  | Kinstanceof
  | Kreturn
  | Ktransient
  | Kcatch
  | Kextends
  | Kint
  | Kshort
  | Ktry
  | Kclass
  | Klong
  | Kstrictfp
  | Kvolatile
  | Kchar
  | Kfinal
  | Kstatic
  | Kvoid
  | Kconst
  | Kfloat
  | Knative
  | Ksuper
  | Kwhile
  deriving (Show, Eq, Bounded, Enum, Read)


stringToKeyword :: String -> Maybe Keyword
stringToKeyword str = Data.Map.lookup str table
  where table = Data.Map.fromList (zip (map (tail . show) kWords) kWords)
        kWords = [minBound .. maxBound] :: [Keyword]


data Literal 
  = LNull
  | LBool Bool
  | LChar Char
  | LString String
  | LInteger LInteger String String -- literal format, value, type
  | LFloating LFloating String String -- literal format, value, type
  deriving (Show, Eq)  
  
data LInteger
    = LIBinary
    | LIOctal
    | LIDecimal
    | LIHex
  deriving (Show, Eq)

data LFloating
    = LFPDecimal
    | LFPHex
  deriving (Show, Eq)


data Separator
  = OpenParen
  | CloseParen
  | OpenCurly
  | CloseCurly
  | OpenSquare
  | CloseSquare
  | Comma
  | Semicolon
  | Period
  deriving (Show, Eq)
  
  
data Operator
  = Equals
  | GreaterThan
  | LessThan
  | ExclamationPoint
  | Tilda
  | QuestionMark
  | Colon
  | DoubleEquals
  | LessThanOrEquals
  | GreaterThanOrEquals
  | NotEquals
  | AndAnd
  | OrOr
  | PlusPlus
  | MinusMinus
  | Plus
  | Minus
  | Times
  | DivideBy
  | And
  | Or
  | ToThe
  | Percentage
  | DoubleLessThan
  | DoubleGreaterThan
  | TripleGreaterThan
  | PlusEquals
  | MinusEquals
  | TimesEquals
  | DivideByEquals
  | AndEquals
  | OrEquals
  | ToTheEquals
  | PercentageEquals
  | DoubleLessThanEquals
  | DoubleGreaterThanEquals
  | TripleGreaterThanEquals
  deriving (Show, Eq)


data Token
  = Identifier String
  | Keyword Keyword
  | Literal Literal
  | Separator Separator
  | Operator Operator
  | AtSign
  | Ellipsis
  deriving (Show, Eq)


data InputElement
  = Whitespace String
  | Comment String
  | Token Token
  deriving (Show, Eq)
