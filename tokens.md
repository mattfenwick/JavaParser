UnicodeInputCharacter:
    UnicodeEscape
    RawInputCharacter

UnicodeEscape:
    '\\'  UnicodeMarker  HexDigit(4)

UnicodeMarker:
    'u'(+)

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
    TraditionalComment
    EndOfLineComment

TraditionalComment:
    '/*'  CommentTail

EndOfLineComment:
    '//'  CharactersInLine(?)

CommentTail:
    '*'  CommentTailStar
    NotStar  CommentTail

CommentTailStar:
    '/'
    '*'  CommentTailStar
    NotStarNotSlash  CommentTail

NotStar:
    InputCharacter but not '*'
    LineTerminator

NotStarNotSlash:
    InputCharacter but not '*' or '/'
    LineTerminator

CharactersInLine:
    InputCharacter
    CharactersInLine  InputCharacter


Identifier:
    IdentifierChars but not a Keyword or BooleanLiteral or NullLiteral

IdentifierChars:
    JavaLetter
    IdentifierChars  JavaLetterOrDigit

JavaLetter:
    any Unicode character that is a Java letter (see below)

JavaLetterOrDigit:
    any Unicode character that is a Java letter-or-digit (see below)


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
    DecimalIntegerLiteral
    HexIntegerLiteral   
    OctalIntegerLiteral
    BinaryIntegerLiteral 

DecimalIntegerLiteral:
    DecimalNumeral  IntegerTypeSuffix(?)

HexIntegerLiteral:
    HexNumeral  IntegerTypeSuffix(?)

OctalIntegerLiteral:    
    OctalNumeral  IntegerTypeSuffix(?)

BinaryIntegerLiteral:
    BinaryNumeral  IntegerTypeSuffix(?)

IntegerTypeSuffix:
    'l'  |  'L'
    
    
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
    DecimalFloatingPointLiteral
    HexadecimalFloatingPointLiteral

DecimalFloatingPointLiteral:
    Digits  '.'  Digits(?)  ExponentPart(?)  FloatTypeSuffix(?)
    '.'  Digits  ExponentPart(?)  FloatTypeSuffix(?)
    Digits  ExponentPart  FloatTypeSuffix(?)
    Digits  ExponentPart(?)  FloatTypeSuffix

ExponentPart:
    ExponentIndicator  SignedInteger

ExponentIndicator:
    'e'  |  'E'

SignedInteger:
    Sign(?)  Digits

Sign:
    '+'  |  '-'

FloatTypeSuffix:
    'f'  |  'F'  |  'd'  |  'D'

HexadecimalFloatingPointLiteral:
    HexSignificand  BinaryExponent  FloatTypeSuffix(?)

HexSignificand:
    HexNumeral
    HexNumeral  '.'
    '0'  ( 'x'  | 'X' )  HexDigits(?)  '.'  HexDigits

BinaryExponent:
    BinaryExponentIndicator  SignedInteger

BinaryExponentIndicator:
    'p'  |  'P'
    
    
BooleanLiteral:
    'true'  |  'false'
    
    
CharacterLiteral:
    '\''  SingleCharacter  '\''
    '\''  EscapeSequence '\''

SingleCharacter:
    InputCharacter but not '\'' or '\\'
    
    
StringLiteral:
    '"'  StringCharacters(?)  '"'

StringCharacters:
    StringCharacter
    StringCharacters  StringCharacter

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
    '\\'  OctalDigit  OctalDigit
    '\\'  ZeroToThree  OctalDigit  OctalDigit

ZeroToThree:
    [0-3]
    
    
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
