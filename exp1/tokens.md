UnicodeInputCharacter:
    UnicodeEscape
    RawInputCharacter

UnicodeEscape:
    '\\'  'u'(+)  HexDigit(4)

HexDigit:
    [0-9a-fA-F]

RawInputCharacter:
    any Unicode character


LineTerminator:
    '\n'  |  '\r'  |  '\r\n'

InputCharacter:
    UnicodeInputCharacter but not LineTerminator


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
    ' '  |  '\t'  |  '\f'  |  LineTerminator

Comment:
    '/*'  (not1 '*/')  '*/'  -- <-- where 'not1' is defined by InputCharacter
    '//'  InputCharacter(*)

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

Literal:
    IntegerLiteral
    BooleanLiteral
    CharacterLiteral
    StringLiteral
    NullLiteral

IntegerLiteral:
    DecimalNumeral  ( 'l'  |  'L' )(?)

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
    '\\'  ( 'b'  |  't'  |  'n'  |  'f'  |  'r'  |  '"'  |  '\''  |  '\\' )

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
