-- be really aggressive with the simplifications on this
--   change stuff if I have to
--   get to the bottom, the core, of things
--   but maybe keep track of the differences

Type:
    BaseType  ( '['  ']' )(*)
    
BaseType:
    'byte'  |  'short'  |  'int'     |  'long'     |  
    'char'  |  'float'  |  'double'  |  'boolean'  |
    sepBy1(Identifier  TypeArguments(?), '.')

TypeArguments:
    '<'  sepBy1(TypeArgument, ',')  '>'

TypeArgument:
    Type
    '?'
    '?'  'super'  Type
    '?'  'extends'  sepBy1(Type, '&')

FullName:
    sepBy1(Identifier, '.')
    

CompilationUnit:
    PackageDeclaration(?)  ImportDeclaration(*)  TypeDeclarations(*)

PackageDeclaration:
    Annotation(*)  'package'  FullName  ';'

ImportDeclaration: 
    'import'  'static'(?)  FullName  ( '.'  '*' )(?)  ';'

TypeDeclaration:
    Class
    Enum
    Interface
    AnnotationType
    ';'


Class:
    Modifier(*)  'class'  Identifier  TypeArguments(?)  Super(?)  Interfaces(?)  ClassBody

Modifier:
    Annotation  |  'public'   |  'protected'     |  'private'  |  'abstract'  |
    'static'    |  'final'    |  'synchronized'  |  'native'   |  'strictfp'  |
    'transient' |  'volatile' |

Super:
    'extends'  Type

Interfaces:
    'implements'  sepBy1(Type, ',')

ClassBody:
    '{'  ClassBodyDeclaration(*)  '}'

ClassBodyDeclaration:
    ClassMemberDeclaration
    'static'(?)  Block
    ConstructorDeclaration

ClassMemberDeclaration:
    VariableDeclaration  ';'
    MethodDeclaration
    TypeDeclaration

VariableDeclaration:
    Modifier(*)  Type  sepBy1(IdentBraces  ( '='  VariableInitializer )(?), ',')

IdentBraces:
    Identifier  ( '['  ']' )(*)

VariableInitializer:
    Expression
    ArrayInitializer

MethodDeclaration:
    Modifier(*)  TypeArguments(?)  Result  Identifier  FormalParameterList  Throws(?)  ( Block  |  ';' )
    
FormalParameterList:
    '('  ')'
    '('  LastFormalParameter  ')'
    '('  sepBy1(FormalParameter, ',')  ','  LastFormalParameter  ')'

FormalParameter:
    Modifier(*)  Type  IdentBraces

LastFormalParameter:
    Modifier(*)  Type  '...'  IdentBraces
    FormalParameter

Result:
    Type
    'void'

Throws:
    'throws'  sepBy1(FullName, ',')


ConstructorDeclaration:
    Modifier(*)  TypeArguments(?)  Identifier  FormalParameterList  Throws(?)  ConstructorBody

ConstructorBody:
    '{'  ConstructorInvocation(?)  BlockStatement(*)  '}'

ConstructorInvocation:
    TypeArguments(?)  'this'  ArgumentList  ';'
    TypeArguments(?)  'super'  ArgumentList  ';'
    Primary  '.'  TypeArguments(?)  'super'  ArgumentList  ';'

Enum:
    Modifier(*)  'enum'  Identifier  Interfaces(?)  EnumBody

EnumBody:
    '{'  sepBy0(EnumConstant, ',')  ','(?)  ( ';'  ClassBodyDeclaration(*) )(?)  '}'

EnumConstant:
    Annotation(*)  Identifier  ArgumentList(?)  ClassBody(?)


Interface:
    Modifier(*)  'interface'  Identifier  TypeArguments(?)  ExtendsInterfaces(?)  '{'  InterfaceMember(*)  '}'

ExtendsInterfaces:
    'extends'  sepBy1(Type, ',')

InterfaceMember:
    VariableDeclaration  ';'
    AbstractMethodDeclaration
    TypeDeclaration

AbstractMethodDeclaration:
    Modifier(*)  TypeArguments(?)  Result  Identifier  FormalParameterList  Throws(?)  ';'

AnnotationType:
    Modifier(*)  '@'  'interface'  Identifier  '{'  AnnotationTypeElement(*)  '}'

AnnotationTypeElement:
    Modifier(*)  Type  Identifier  '('  ')'  Dim(*)  DefaultValue(?)  ';'
    VariableDeclaration  ';'
    TypeDeclaration

DefaultValue:
    'default'  ElementValue

Annotation:
    NormalAnnotation
    MarkerAnnotation
    SingleElementAnnotation

NormalAnnotation:
    '@'  FullName  '('  sepBy0(ElementValuePair, ',')  ')'

ElementValuePair:
    Identifier  '='  ElementValue

ElementValue:
    ConditionalExpression
    Annotation
    ElementValueArrayInitializer

ElementValueArrayInitializer:
    '{'  sepBy0(ElementValue, ',')  ','(?)  '}'

MarkerAnnotation:
    '@'  Identifier

SingleElementAnnotation:
    '@'  Identifier  '('  ElementValue  ')'


ArrayInitializer:
    '{'  sepBy0(VariableInitializer, ',')  ','(?)  '}'


Block:
    '{'  BlockStatement(*)  '}'

BlockStatement:
    VariableDeclaration  ';'
    Class
    Enum
    Statement

Statement:
    StatementWithoutTrailingSubstatement
    LabeledStatement
    IfThenStatement
    IfThenElseStatement
    WhileStatement
    ForStatement

StatementWithoutTrailingSubstatement:
    Block
    EmptyStatement
    ExpressionStatement
    AssertStatement
    SwitchStatement
    DoStatement
    BreakStatement
    ContinueStatement
    ReturnStatement
    SynchronizedStatement
    ThrowStatement
    TryStatement

StatementNoShortIf:
    StatementWithoutTrailingSubstatement
    LabeledStatementNoShortIf
    IfThenElseStatementNoShortIf
    WhileStatementNoShortIf
    ForStatementNoShortIf

EmptyStatement:
    ';'

LabeledStatement:
    Identifier  ':'  Statement

LabeledStatementNoShortIf:
    Identifier  ':'  StatementNoShortIf

ExpressionStatement:
    StatementExpression  ';'

StatementExpression:
    Assignment
    PreIncrementExpression
    PreDecrementExpression
    PostIncrementExpression
    PostDecrementExpression
    MethodInvocation
    ClassInstanceCreationExpression

IfThenStatement:
    'if'  '('  Expression  ')'  Statement

IfThenElseStatement:
    'if'  '('  Expression  ')'  StatementNoShortIf  'else'  Statement

IfThenElseStatementNoShortIf:
    'if'  '('  Expression  ')'  StatementNoShortIf  'else'  StatementNoShortIf

AssertStatement:
    'assert'  Expression1  ';'
    'assert'  Expression1  ':'  Expression2  ';'

SwitchStatement:
    'switch'  '('  Expression  ')'  '{'  ( SwitchLabel(+)  BlockStatement(+) )(*)  SwitchLabel(*)  '}'

SwitchLabel:
    'case'  ConstantExpression  ':'
    'case'  EnumConstantName  ':'
    'default'  ':'

EnumConstantName:
    Identifier

WhileStatement:
    'while'  '('  Expression  ')'  Statement

WhileStatementNoShortIf:
    'while'  '('  Expression  ')'  StatementNoShortIf

DoStatement:
    'do'  Statement  'while'  '('  Expression  ')'  ';'

ForStatement:
    BasicForStatement
    EnhancedForStatement

BasicForStatement:
    'for'  '('  ForInit(?)  ';'  Expression(?)  ';'  ForUpdate(?)  ')'  Statement

ForStatementNoShortIf:
    'for'  '('  ForInit(?)  ';'  Expression(?)  ';'  ForUpdate(?)  ')'  StatementNoShortIf

ForInit:
    sepBy1(StatementExpression, ',')
    VariableDeclaration

ForUpdate:
    sepBy1(StatementExpression, ',')

EnhancedForStatement:
    'for'  '('  FormalParameter  ':'  Expression  ')'  Statement

BreakStatement:
    'break'  Identifier(?)  ';'

ContinueStatement:
    'continue'  Identifier(?)  ';'

ReturnStatement:
    'return'  Expression(?)  ';'

ThrowStatement:
    'throw'  Expression  ';'

SynchronizedStatement:
    'synchronized'  '('  Expression  ')'  Block

TryStatement:
    'try'  Block  CatchClause(+)
    'try'  Block  CatchClause(*)  Finally
    TryWithResourcesStatement

CatchClause:
    'catch'  '('  Modifier(*)  CatchType  IdentBraces  ')'  Block

CatchType:
    sepBy1(Type, '|')

Finally:
    'finally'  Block

TryWithResourcesStatement:
    'try'  ResourceSpecification  Block  CatchClause(*)  Finally(?)

ResourceSpecification:
    '('  sepBy1(Resource, ';')  ';'(?)  ')'

Resource:
    Modifier(*)  Type  IdentBraces  '='  Expression


Primary:
    PrimaryNoNewArray
    ArrayCreationExpression

PrimaryNoNewArray:
    Literal
    Type  '.'  'class'
    'void'  '.'  'class'
    'this'
    FullName  '.'  'this'
    '('  Expression  ')'
    ClassInstanceCreationExpression
    FieldAccess
    MethodInvocation
    ArrayAccess

Literal:
    IntegerLiteral
    FloatingPointLiteral
    BooleanLiteral
    CharacterLiteral
    StringLiteral
    NullLiteral

ClassInstanceCreationExpression:
    'new'  TypeArguments(?)  TypeDeclSpecifier  TypeArgumentsOrDiamond(?)  ArgumentList  ClassBody(?)
    Primary  '.'  'new'  TypeArguments(?)  Identifier  TypeArgumentsOrDiamond(?)  ArgumentList  ClassBody(?)

TypeDeclSpecifier:
    sepBy1(Identifier, '.')  ( TypeArguments(?)  '.'  Identifier )(*)

TypeArgumentsOrDiamond:
    TypeArguments
    '<'  '>' 

ArgumentList:
    '('  sepBy0(Expression, ',')  ')'

ArrayCreationExpression:
    'new'  Type  DimExpr(+)  Dim(*)
    'new'  Type  Dim(+)  ArrayInitializer

DimExpr:
    '['  Expression  ']'

Dim:
    '['  ']'

FieldAccess: 
    Primary  '.'  Identifier
    'super'  '.'  Identifier
    FullName  '.'  'super'  '.'  Identifier

MethodInvocation:
    FullName  ArgumentList
    Primary  '.'  TypeArguments(?)  Identifier  ArgumentList
    'super'  '.'  TypeArguments(?)  Identifier  ArgumentList
    FullName  '.'  'super'  '.'  TypeArguments(?)  Identifier  ArgumentList
    FullName  '.'  TypeArguments  Identifier  ArgumentList

ArrayAccess:
    FullName  '['  Expression  ']'
    PrimaryNoNewArray  '['  Expression  ']'

PostfixExpression:
    Primary
    FullName
    PostIncrementExpression
    PostDecrementExpression

PostIncrementExpression:
    PostfixExpression  '++'

PostDecrementExpression:
    PostfixExpression  '--'

UnaryExpression:
    PreIncrementExpression
    PreDecrementExpression
    ( '+'  |  '-' )  UnaryExpression
    UnaryExpressionNotPlusMinus

PreIncrementExpression:
    '++'  UnaryExpression

PreDecrementExpression:
    '--'  UnaryExpression

UnaryExpressionNotPlusMinus:
    PostfixExpression
    ( '~'  |  '!' )  UnaryExpression
    CastExpression

CastExpression:
    '('  PrimitiveType  ')'  UnaryExpression
    '('  Type  ')'  UnaryExpressionNotPlusMinus

MultiplicativeExpression:
    UnaryExpression
    MultiplicativeExpression  ( '*'  |  '/'  |  '%' )  UnaryExpression

AdditiveExpression:
    MultiplicativeExpression
    AdditiveExpression  ( '+'  |  '-' )  MultiplicativeExpression

ShiftExpression:
    AdditiveExpression
    ShiftExpression  ( '<<'  |  '>>'  |  '>>>' ) AdditiveExpression

RelationalExpression:
    ShiftExpression
    RelationalExpression  ( '<'  |  '>'  |  '<='  |  '>=' )  ShiftExpression
    RelationalExpression  'instanceof'  Type

EqualityExpression:
    RelationalExpression
    EqualityExpression  ( '=='  |  '!=' )  RelationalExpression

AndExpression:
    EqualityExpression
    AndExpression  '&'  EqualityExpression

ExclusiveOrExpression:
    AndExpression
    ExclusiveOrExpression  '^'  AndExpression

InclusiveOrExpression:
    ExclusiveOrExpression
    InclusiveOrExpression  '|'  ExclusiveOrExpression

ConditionalAndExpression:
    InclusiveOrExpression
    ConditionalAndExpression  '&&'  InclusiveOrExpression

ConditionalOrExpression:
    ConditionalAndExpression
    ConditionalOrExpression  '||'  ConditionalAndExpression

ConditionalExpression:
    ConditionalOrExpression
    ConditionalOrExpression  '?'  Expression  ':'  ConditionalExpression

AssignmentExpression:
    ConditionalExpression
    Assignment

Assignment:
    LeftHandSide AssignmentOperator AssignmentExpression

LeftHandSide:
    FullName
    FieldAccess
    ArrayAccess

AssignmentOperator:
    '='   |  '*='   |  '/='   |  '%='    |  '+='  |
    '-='  |  '<<='  |  '>>='  |  '>>>='  |  '&='  |
    '^='  |  '|='

Expression:
    AssignmentExpression

ConstantExpression:
    Expression
