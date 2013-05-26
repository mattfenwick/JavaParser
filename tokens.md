UnicodeInputCharacter:
    UnicodeEscape
    RawInputCharacter

UnicodeEscape:
    '\\'  'u'(+)  HexDigit(4)

RawInputCharacter:
    any Unicode character


LineTerminator:
    the ASCII LF character, also known as "newline"
    the ASCII CR character, also known as "return"
    the ASCII CR character followed by the ASCII LF character

InputCharacter:
    UnicodeInputCharacter but not CR or LF


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

Sub:
    the ASCII SUB character, also known as "control-Z"


WhiteSpace:
    the ASCII SP character, also known as "space"
    the ASCII HT character, also known as "horizontal tab"
    the ASCII FF character, also known as "form feed"
    LineTerminator


Comment:
    '/*'  (not1 '*/')  '*/'  -- <-- where 'not1' is defined by InputCharacter
    '//'  InputCharacter(*)


Identifier:
    JavaLetter  JavaLetterOrDigit(*)     -- <-- but not a Keyword or BooleanLiteral or NullLiteral

JavaLetter:
    [a-zA-Z_$]

JavaLetterOrDigit:
    JavaLetter  |  [0-9]

{- Notes in Java spec:
A "Java letter" is a character for which the method Character.isJavaIdentifierStart(int) returns true.
A "Java letter-or-digit" is a character for which the method Character.isJavaIdentifierPart(int) returns true.

Letters and digits may be drawn from the entire Unicode character set, 
which supports most writing scripts in use in the world today, 
including the large sets for Chinese, Japanese, and Korean. 
This allows programmers to use identifiers in their programs that are written in their native languages.
-}

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


Literal:
    IntegerLiteral
    FloatingPointLiteral
    BooleanLiteral
    CharacterLiteral
    StringLiteral
    NullLiteral


IntegerLiteral:
    ( DecimalNumeral  |  HexNumeral  |    OctalNumeral  |  BinaryNumeral )  ( 'l'  |  'L' )(?)
    
DecimalNumeral:
    '0'
    NonZeroDigit  Digits(?)
    NonZeroDigit  '_'(+)  Digits 

Digits:
    Digit
    Digit  ( Digit  |  '_' )(*)  Digit 

Digit:
    '0'
    NonZeroDigit

NonZeroDigit:
    [1-9]
    
    
HexNumeral:
    '0'  ( 'x'  |  'X' ) HexDigits

HexDigits:
    HexDigit
    HexDigit  ( HexDigit  |  '_' )(*)  HexDigit 

HexDigit:
    [0-9a-fA-F]

    
OctalNumeral:
    '0'  OctalDigits
    '0'  '_'(+)  OctalDigits

OctalDigits:
    OctalDigit
    OctalDigit  ( OctalDigit  |  '_' )(*)  OctalDigit 

OctalDigit:
    [0-7]
    
    
BinaryNumeral:
    '0'  ( 'b'  |  'B')  BinaryDigits 

BinaryDigits:
    BinaryDigit 
    BinaryDigit  ( BinaryDigit  |  '_' )(*)  BinaryDigit

BinaryDigit:
    '0'  |  '1'
    
    
FloatingPointLiteral:
    DecimalFP
    HexFP

DecimalFP:
    Digits  '.'  Digits(?)  ExponentPart(?)  FloatTypeSuffix(?)
            '.'  Digits     ExponentPart(?)  FloatTypeSuffix(?)
    Digits                  ExponentPart     FloatTypeSuffix(?)
    Digits                  ExponentPart(?)  FloatTypeSuffix

ExponentPart:
    ( 'e'  |  'E' )  SignedInteger

SignedInteger:
    Sign(?)  Digits

Sign:
    '+'  |  '-'

FloatTypeSuffix:
    'f'  |  'F'  |  'd'  |  'D'

HexFP:
    HexSignificand  BinaryExponent  FloatTypeSuffix(?)

HexSignificand:
    HexNumeral
    HexNumeral  '.'
    '0'  ( 'x'  | 'X' )  HexDigits(?)  '.'  HexDigits

BinaryExponent:
    ( 'p'  |  'P' )  SignedInteger

    
BooleanLiteral:
    'true'  |  'false'
    
CharacterLiteral:
    '\''  SingleCharacter  '\''
    '\''  EscapeSequence '\''

SingleCharacter:
    InputCharacter but not '\'' or '\\'
        
StringLiteral:
    '"'  StringCharacter(*)  '"'

StringCharacter:
    InputCharacter but not '"' or '\\'
    EscapeSequence

EscapeSequence:
    '\\b'    /* \u0008: backspace BS */
    '\\t'    /* \u0009: horizontal tab HT */
    '\\n'    /* \u000a: linefeed LF */
    '\\f'    /* \u000c: form feed FF */
    '\\r'    /* \u000d: carriage return CR */
    '\\"'    /* \u0022: double quote " */
    '\\\''    /* \u0027: single quote ' */
    '\\\\'              /* \u005c: backslash \ */
    OctalEscape        /* \u0000 to \u00ff: from octal value */

OctalEscape:
    '\\'  OctalDigit
    '\\'  OctalDigit(2)
    '\\'  [0-3]  OctalDigit(2)

NullLiteral:
    'null'

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
