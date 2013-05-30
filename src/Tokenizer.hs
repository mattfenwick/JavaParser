{-# LANGUAGE NoMonomorphismRestriction #-}
module Tokenizer (

    input

) where

import Combinaparse
import Tokens                    
import Data.Traversable          (Traversable(..))
import Control.Monad             (replicateM)
import Hex                       (hexStringToInteger, octalStringToInteger)
import Data.Char                 (chr)
import Prelude        hiding     (exp)


rawHexDigit :: Parser e Char Char
rawHexDigit = satisfy (flip elem (['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']))

unicodeEscape :: Parser String Char Char
unicodeEscape = literal '\\' *> many1 (literal 'u') *> fmap (chr . fromInteger . hexStringToInteger) hexDigits
  where hexDigits = commit "unicode escape error" (replicateM 4 rawHexDigit)

unicodeInputCharacter = unicodeEscape <|> item

-- enhanced combinators that are unicode-escape aware

jItem      = unicodeInputCharacter
jSatisfy   = flip check jItem
jNot1 p    = switch p *> jItem
jLiteral c = jSatisfy (== c)
jStr       = traverse jLiteral

oneOf :: [Char] -> Parser String Char Char
oneOf = jSatisfy . flip elem

-- which means everything after this should use the `j...` versions


lineTerminator = jLiteral '\n' <|> jLiteral '\r' <|> (pure '\n' <* jStr "\r\n") -- bad solution to type problem !!!

inputCharacter = jNot1 lineTerminator

sub = end -- may be wrong.  should optionally match ASCII 26 

whiteSpace :: Parser String Char Char
whiteSpace = jLiteral ' ' <|> jLiteral '\t' <|> jLiteral '\f' <|> lineTerminator

comment :: Parser String Char String
comment = c1 <|> c2
  where c1 = jStr "/*" *> many0 (jNot1 $ jStr "*/") <* jStr "*/"
        c2 = jStr "//" *> many0 inputCharacter


classify :: String -> Token
classify "null" = Literal LNull
classify "true" = Literal (LBool True)
classify "false" = Literal (LBool False)
classify str = case stringToKeyword str of (Just k) -> Keyword k
                                           Nothing  -> Identifier str


identifierOrKeywordOrBooleanOrNull = fmap (\f rs -> classify (f:rs)) first <*> many0 rest
  where first = oneOf (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_$")
        rest = first <|> oneOf ['0' .. '9']

floatingPointLiteral :: Parser String Char Literal
floatingPointLiteral = decimalFP -- <|> hexFP

floatType :: Parser String Char String
floatType = (oneOf "fF" *> pure "Float") <|> (oneOf "dD" *> pure "Double")

digits :: Parser String Char String
digits = 
    digit                    >>= \d ->
    many0 digitOrUnderscore  >>= \ds ->
    case safeTail ds of (Just '_') -> throwError "bad _ in decimal floating point literal";
                        _          -> return (d : filter (/= '_') ds)

exp :: Parser String Char String
exp = pure (\c e ds -> c : e : ds) <*> oneOf "eE" <*> optional '+' (oneOf "+-") <*> digits

binaryExp :: Parser String Char String
binaryExp = pure  (\c e ds -> c : e : ds) <*> oneOf "pP" <*> optional '+' (oneOf "+-") <*> digits

decimalFP :: Parser String Char Literal
decimalFP = fmap (\(d1, d, d2, e, t) -> LFloating LFPDecimal (d1 ++ (d : d2) ++ e) t) (a1 <|> a2 <|> a3 <|> a4)
  where
    a1 = pure (,,,,) <*> digits  <*> jLiteral '.' <*> optional "" digits <*> optional "" exp <*> optional "Double" floatType
    a2 = pure (,,,,) <*> pure "" <*> jLiteral '.' <*> digits             <*> optional "" exp <*> optional "Double" floatType
    a3 = pure (,,,,) <*> digits  <*> pure '.'     <*> pure ""            <*> exp             <*> optional "Double" floatType
    a4 = pure (,,,,) <*> digits  <*> pure '.'     <*> pure ""            <*> pure ""         <*> floatType

{-
HexFP:
    HexSignificand  BinaryExponent  FloatTypeSuffix(?)

HexSignificand:
    HexNumeral
    HexNumeral  '.'
    '0'  [xX]  HexDigits(?)  '.'  HexDigits
-}
-- hexFP = undefined

digit = oneOf ['0' .. '9']
digitOrUnderscore = digit <|> jLiteral '_'

safeTail [] = Nothing
safeTail (x:[]) = Just x
safeTail (x:xs) = safeTail xs

integerLiteral :: Parser String Char Literal
integerLiteral = binaryInteger <|> hexInteger <|> octalInteger <|> decimalInteger

integerTypeSuffix = optional "int" (oneOf "lL" *> pure "long")

binaryInteger :: Parser String Char Literal
binaryInteger = pure (LInteger LIBinary) <*> body <*> integerTypeSuffix
  where body = jLiteral '0'           *> 
               oneOf "bB"             *>  -- once we see the b, we need to commit
               oneOf "01"            >>= \o -> 
               many0 (oneOf "01_")   >>= \os ->
               case safeTail os of (Just '_') -> throwError "bad _ in binary integer literal";
                                   _          -> return (o : filter (/= '_') os);

hexInteger :: Parser String Char Literal
hexInteger = pure (LInteger LIHex) <*> body <*> integerTypeSuffix
  where body = jLiteral '0'                        *>
               oneOf "xX"                          *>  -- once we see the x, we need to commit
               hexDigit                           >>= \o ->
               many0 (hexDigit <|> jLiteral '_')  >>= \os -> 
               case safeTail os of (Just '_') -> throwError "bad _ in hex integer literal";
                                   _          -> return (o : filter (/= '_') os);
        hexDigit = oneOf (['0' .. '9'] ++ "abcdefABCDEF")

octalInteger :: Parser String Char Literal
octalInteger = pure (LInteger LIOctal) <*> body <*> integerTypeSuffix
  where body = jLiteral '0'                *>  -- do not need to commit after seeing 0, 0 is a valid decimal integer
               many1 (oneOf "01234567_")  >>= \os -> 
               case safeTail os of (Just '_') -> throwError "bad _ in octal integer literal";
                                   _          -> return (filter (/= '_') os);

decimalInteger :: Parser String Char Literal
decimalInteger = pure (LInteger LIDecimal) <*> body <*> integerTypeSuffix
  where body = i1 <|> fmap (:[]) digit
        i1 = oneOf ['1' .. '9']                >>= \o ->
             many0 digitOrUnderscore           >>= \os ->
             case safeTail os of (Just '_') -> throwError "bad _ in decimal integer literal";
                                 _          -> return (o : filter (/= '_') os); -- Nothing or Just a digit:  good to go

{-
  - \8 -> error
  - \78 -> not error, \7 is the octal escape
  - \1234 -> not error, \123 is the o.e.
  - \1a -> not error, \1 is o.e.
-}
octalEscape :: Parser String Char Char
octalEscape = fmap (chr . fromInteger . octalStringToInteger) (o1 <|> o2 <|> o3)
  where o1 = sequenceA [oneOf "0123", odigit, odigit]
        o2 = sequenceA [odigit, odigit]
        o3 = fmap (:[]) odigit
        odigit = oneOf "01234567"

-- need to commit after seeing the \
escapeSequence = jLiteral '\\' *> foldr (<|>) empty parsers
  where parsers = octalEscape : map (\(i, o) -> jLiteral i *> pure o) escapes
        escapes = [('b',  '\b'),
                   ('t',  '\t'),
                   ('n',  '\n'),
                   ('f',  '\f'),
                   ('r',  '\r'),
                   ('"',  '"' ),
                   ('\'', '\''),
                   ('\\', '\\')]
                   
characterLiteral = sq *> commit "invalid character literal or missing single quote" rest
  where sq = jLiteral '\''
        single = switch (jLiteral '\'' <|> jLiteral '\\') *>  inputCharacter
        rest = fmap LChar (single <|> escapeSequence) <* sq

stringCharacter = escapeSequence <|> (switch (jLiteral '"' <|> jLiteral '\\') *> inputCharacter)

stringLiteral = dq *> commit "invalid string literal or missing double quote" rest
  where dq = jLiteral '"'
        rest = fmap LString (many0 stringCharacter) <* dq

javaLiteral :: Parser String Char Token
javaLiteral = fmap Literal (floatingPointLiteral <|> integerLiteral <|> characterLiteral <|> stringLiteral)


seps :: [(Char, Separator)]
seps = [('(', OpenParen),
        (')', CloseParen),
        ('{', OpenCurly),
        ('}', CloseCurly),
        ('[', OpenSquare),
        (']', CloseSquare),
        (',', Comma),
        (';', Semicolon),
        ('.', Period)]

separator :: Parser String Char Separator
separator = foldr (<|>) empty $ map p seps
  where 
    p (c, sep) = fmap (const sep) $ jLiteral c


ops :: [(String, Operator)]
ops = [("==",   DoubleEquals),
       ("=",    Equals),
       (">>>=", TripleGreaterThanEquals),
       (">>>",  TripleGreaterThan),
       (">>=",  DoubleGreaterThanEquals),
       (">>",   DoubleGreaterThan),
       (">=",   GreaterThanOrEquals),
       (">",    GreaterThan),
       ("<<=",  DoubleLessThanEquals),
       ("<=",   LessThanOrEquals),
       ("<<",   DoubleLessThan),
       ("<",    LessThan),
       ("!=",   NotEquals),
       ("!",    ExclamationPoint),
       ("&&",   AndAnd),
       ("&=",   AndEquals),
       ("&",    And),
       ("||",   OrOr),
       ("|=",   OrEquals),
       ("|",    Or),
       ("+=",   PlusEquals),
       ("++",   PlusPlus),
       ("+",    Plus),
       ("-=",   MinusEquals),
       ("--",   MinusMinus),
       ("-",    Minus),
       ("*=",   TimesEquals),
       ("*",    Times),
       ("/=",   DivideByEquals),
       ("/",    DivideBy),
       ("^=",   ToTheEquals),
       ("^",    ToThe),
       ("%=",   PercentageEquals),
       ("%",    Percentage),
       ("~",    Tilda),
       ("?",    QuestionMark),
       (":",    Colon)]

operator :: Parser String Char Operator
operator = foldr (<|>) empty $ map p ops
  where p (s, op) = fmap (const op) $ jStr s


token :: Parser String Char Token
token = identifierOrKeywordOrBooleanOrNull  <|> 
        javaLiteral                         <|> 
        (jLiteral '@' *> pure AtSign)       <|>
        (jStr "..." *> pure Ellipsis)       <|>
        fmap Separator separator            <|> 
        fmap Operator operator

inputElement :: Parser String Char InputElement
inputElement = fmap Whitespace (many1 whiteSpace) <|> fmap Comment comment <|> fmap Token token

input :: Parser String Char [InputElement]
input = many0 inputElement <* optionalM sub
