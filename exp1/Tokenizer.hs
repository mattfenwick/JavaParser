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
{-
Input:
    InputElement(*)  Sub(?)

InputElement:
    WhiteSpace
    Comment
    Token

Token:
    Identifier
    Keyword
    Literal
    Separator
    Operator
-}
{-
Identifier:
    [a-zA-Z_$]  [a-zA-Z_$0-9](*)     -- <-- but not a Keyword or BooleanLiteral or NullLiteral

Keyword:
    'abstract'  |  'continue'  |  'for'         |  'new'        |  'switch'        |
    'assert'    |  'default'   |  'if'          |  'package'    |  'synchronized'  |
    'boolean'   |  'do'        |  'goto'        |  'private'    |  'this'          |
    'break'     |  'double'    |  'implements'  |  'protected'  |  'throw'         |
    'byte'      |  'else'      |  'import'      |  'public'     |  'throws'        |
    'case'      |  'enum'      |  'instanceof'  |  'return'     |  'transient'     |
    'catch'     |  'extends'   |  'int'         |  'short'      |  'try'           |
    'char'      |  'final'     |  'interface'   |  'static'     |  'void'          |
    'class'     |  'finally'   |  'long'        |  'strictfp'   |  'volatile'      |
    'const'     |  'float'     |  'native'      |  'super'      |  'while' 
-}


classify :: String -> Token
classify str = case stringToKeyword str of (Just k) -> Keyword k
                                           Nothing  -> Identifier str


identifierOrKeyword = fmap (\f rs -> classify (f:rs)) first <*> many0 rest
  where first = jSatisfy (flip elem (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_$"))
        rest = first <|> jSatisfy (flip elem ['0' .. '9'])
