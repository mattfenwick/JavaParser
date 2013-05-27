{-# LANGUAGE NoMonomorphismRestriction #-}
module Tokenizer (

) where

import Combinaparse
import Tokens                    (Keyword(..), Literal(..), Operator(..), Separator(..), Token(..), stringToKeyword)
import Data.Traversable          (Traversable(..))
import Control.Monad             (replicateM)
import Hex                       (hexStringToInteger)
import Data.Char                 (chr)


-- more combinators

str = traverse literal


--


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

-- which means everything after this should use the `j...` versions

lineTerminator = jLiteral '\n' <|> jLiteral '\r' <|> (pure '\n' <* jStr "\r\n") -- bad solution to type problem !!!

inputCharacter = jNot1 lineTerminator

sub = end

whiteSpace = jLiteral ' ' <|> jLiteral '\t' <|> jLiteral '\f' <|> lineTerminator

comment = c1 <|> c2
  where c1 = jStr "/*" *> many0 (jNot1 $ jStr "*/") <* jStr "*/"
        c2 = jStr "//" *> many0 inputCharacter


classify :: String -> Token
classify "null" = Literal LNull
classify "true" = Literal (LBool True)
classify "false" = Literal (LBool False)
classify str = case stringToKeyword str of (Just k) -> Keyword k
                                           Nothing  -> Identifier str


identifierOrKeyword = fmap (\f rs -> classify (f:rs)) first <*> many0 rest
  where first = jSatisfy (flip elem (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_$"))
        rest = first <|> jSatisfy (flip elem ['0' .. '9'])


digit = jSatisfy (flip elem ['0' .. '9'])
{-
digitOrUnderscore = fmap (:[]) digit <|> (underscore *> pure [])
  where underscore = jLiteral '_' *> get >>= f
        f (x:xs) 
          | elem x ['0' .. '9'] = pure ()
          | x == '_'            = pure ()
          | otherwise = throwError "bad _ in integer literal"
        f [] = throwError "bad _ in integer literal"
-}
digitOrUnderscore = digit <|> jLiteral '_'

safeTail [] = Nothing
safeTail (x:[]) = Just x
safeTail (x:xs) = safeTail xs

integerLiteral = pure LInteger <*> body <*> typeSuffix
  where body = i1 <|> fmap (:[]) digit
        i1 = jSatisfy (flip elem ['1' .. '9']) >>= \o ->
             many0 digitOrUnderscore           >>= \os ->
             case safeTail os of (Just '_') -> throwError "bad _ in integer literal";
                                 _          -> return (o : filter (/= '_') os); -- Nothing or Just a digit:  good to go
        typeSuffix = optional "int" ((jLiteral 'l' <|> jLiteral 'L') *> pure "long")


escapeSequence = jLiteral '\\' *> foldr (<|>) empty parsers
  where parsers = map (\(i, o) -> jLiteral i *> pure o) escapes
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
javaLiteral = fmap Literal (integerLiteral <|> characterLiteral <|> stringLiteral)


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


-- separator :: Parser Char Separator
separator = foldr (<|>) empty $ map p seps
  where 
    p (c, sep) = fmap (const sep) $ jLiteral c


ops :: [(String, Operator)]
ops = [("=",    Equals),
       (">",    GreaterThan),
       ("<",    LessThan),
       ("!",    ExclamationPoint),
       ("~",    Tilda),
       ("?",    QuestionMark),
       (":",    Colon),
       ("==",   DoubleEquals),
       ("<=",   LessThanOrEquals),
       (">=",   GreaterThanOrEquals),
       ("!=",   NotEquals),
       ("&&",   AndAnd),
       ("||",   OrOr),
       ("++",   PlusPlus),
       ("--",   MinusMinus),
       ("+",    Plus),
       ("-",    Minus),
       ("*",    Times),
       ("/",    DivideBy),
       ("&",    And),
       ("|",    Or),
       ("^",    ToThe),
       ("%",    Percentage),
       ("<<",   DoubleLessThan),
       (">>",   DoubleGreaterThan),
       (">>>",  TripleGreaterThan),
       ("+=",   PlusEquals),
       ("-=",   MinusEquals),
       ("*=",   TimesEquals),
       ("/=",   DivideByEquals),
       ("&=",   AndEquals),
       ("|=",   OrEquals),
       ("^=",   ToTheEquals),
       ("%=",   PercentageEquals),
       ("<<=",  DoubleLessThanEquals),
       (">>=",  DoubleGreaterThanEquals),
       (">>>=", TripleGreaterThanEquals)]

-- operator :: Parser Char Operator
operator = foldr (<|>) empty $ map p ops
  where p (s, op) = fmap (const op) $ jStr s
{-
Separator:
    '('  |  ')'  |  '{'  |  '}'  |  '['  |
    ']'  |  ';'  |  ','  |  '.'
    
Operator:
    '='    |  '>'    |  '<'   |  '!'   |  '~'    |  '?'   |  ':'    |
    '=='   |  '<='   |  '>='  |  '!='  |  '&&'   |  '||'  |  '++'   |
    '--'   |  '+'    |  '-'   |  '*'   |  '/'    |  '&'   |  '|'    |
    '^'    |  '%'    |  '<<'  |  '>>'  |  '>>>'  |  '+='  |  '-='   |
    '*='   |  '/='   |  '&='  |  '|='  |  '^='   |  '%='  |  '<<='  |
    '>>='  |  '>>>='
-}
{-
Input:
    InputElement(*)  Sub(?)

InputElement:
    WhiteSpace
    Comment
    Token

Token:
    Identifier
    Literal
    Separator
    Operator
-}