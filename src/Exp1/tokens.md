RawHexDigit:
    [0-9a-fA-F]

UnicodeEscape:
    '\\'  'u'(+)  RawHexDigit(4)

RawInputCharacter:
    any Unicode character

UnicodeInputCharacter:
    UnicodeEscape
    RawInputCharacter

 
-- note:  all following rules operate on unicode-unescaped input,
--   not on the raw input, according to `UnicodeInputCharacter` above

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
    IdentifierOrKeywordOrNullOrBoolean
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

IdentifierOrKeywordOrNullOrBoolean:
    [a-zA-Z_$]  [a-zA-Z_$0-9](*)     -- classified as Keyword, null, or boolean if it matches one of them

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
    CharacterLiteral
    StringLiteral

IntegerLiteral:
    DecimalNumeral  ( 'l'  |  'L' )(?)

DecimalNumeral:
    [1-9]  [0-9_](*)  [0-9]
    [0-9]

CharacterLiteral:
    '\''  SingleCharacter  '\''
    '\''  EscapeSequence  '\''

SingleCharacter:
    InputCharacter but not '\'' or '\\'
        
StringLiteral:
    '"'  StringCharacter(*)  '"'

StringCharacter:
    InputCharacter but not '"' or '\\'
    EscapeSequence

EscapeSequence:
    '\\'  ( 'b'  |  't'  |  'n'  |  'f'  |  'r'  |  '"'  |  '\''  |  '\\' )

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
