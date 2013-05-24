Type:
    PrimitiveType
    ReferenceType
    
PrimitiveType:
    'byte'  |  'short'  |  'int'  |  'long'  |  'char'  |  'float'  |  'double'  |  'boolean'

ReferenceType:
    sepBy1(Identifier  TypeArguments(?), '.')
    Type  '['  ']'

TypeArguments:
    '<'  sepBy1(TypeArgument, ',')  '>'

TypeArgument:
    ReferenceType
    '?'
    '?'  'extends'  ReferenceType
    '?'  'super'  ReferenceType


ClassOrInterfaceType:
    ClassType
    InterfaceType

ClassType:
InterfaceType:
    sepBy1(Identifier  TypeArguments(?), '.')

PackageName:
TypeName:
ExpressionName:
MethodName:
PackageOrTypeName:
AmbiguousName:
    sepBy1(Identifier, '.')


CompilationUnit:
    PackageDeclaration(?)  ImportDeclaration(*)  TypeDeclarations(*)

PackageDeclaration:
    Annotation(*)  'package'  PackageName  ';'

ImportDeclaration:
    SingleTypeImportDeclaration
    TypeImportOnDemandDeclaration   
    SingleStaticImportDeclaration   
    StaticImportOnDemandDeclaration

SingleTypeImportDeclaration:
    'import'  TypeName  ';'

TypeImportOnDemandDeclaration:
    'import'  PackageOrTypeName  '.'  '*'  ';'

SingleStaticImportDeclaration:
    'import'  'static'  TypeName  '.'  Identifier  ';'

StaticImportOnDemandDeclaration:
    'import'  'static'  TypeName  '.'  '*'  ';'

TypeDeclaration:
    Class
    Enum
    Interface
    AnnotationType
    ';'


Class:
    ClassModifiers(*)  'class'  Identifier  TypeParameters(?)  Super(?)  Interfaces(?)  ClassBody

ClassModifier:
    Annotation  |  'public'  |  'protected'  |  'private
    'abstract'  |  'static'  |  'final'      |  'strictfp

TypeParameters:
    '<'  sepBy1(TypeParameter, ',')  '>'

TypeParameter:
    Identifier
    Identifier  'extends'  Identifier
    Identifier  'extends'  sepBy1(sepBy1(Identifier  TypeArguments(?), '.'), '&')

Super:
    'extends'  ClassType

Interfaces:
    'implements'  InterfaceTypeList

InterfaceTypeList:
    sepBy1(InterfaceType, ',')

ClassBody:
    '{'  ClassBodyDeclaration(*)  '}'

ClassBodyDeclaration:
    ClassMemberDeclaration
    'static'(?)  Block
    ConstructorDeclaration

ClassMemberDeclaration:
    FieldDeclaration
    MethodDeclaration
    TypeDeclaration

FieldDeclaration:
    FieldModifier(*)  Type  VariableDeclarators  ';'

VariableDeclarators:
    sepBy1(VariableDeclarator, ',')

VariableDeclarator:
    IdentBraces  ( '='  VariableInitializer )(?)

IdentBraces:
    Identifier  ( '['  ']' )(*)

VariableInitializer:
    Expression
    ArrayInitializer

FieldModifier:
    Annotation  |  'public'  |  'protected'  |  'private'
    'static'    |  'final'   |  'transient'  |  'volatile'

MethodDeclaration:
    MethodHeader  MethodBody

MethodHeader:
    MethodModifier(*)  TypeParameters(?)  Result  MethodDeclarator  Throws(?)

MethodDeclarator:
    Identifier  FormalParameterList
    MethodDeclarator  '['  ']'         <-- spec says this is obsolete and don't use it!!!!  instead use the first alternative

FormalParameterList:
    '('  ')'
    '('  LastFormalParameter  ')'
    '('  sepBy1(FormalParameter, ',')  ','  LastFormalParameter  ')'

FormalParameter:
    VariableModifier(*)  Type  IdentBraces

VariableModifier:
    Annotation  |  'final'

LastFormalParameter:
    VariableModifier(*)  Type  '...'  IdentBraces
    FormalParameter

MethodModifier:
    Annotation  |  'public'  |  'protected'     |  'private'  |  'abstract'
    'static'    |  'final'   |  'synchronized'  |  'native'   |  'strictfp'

Result:
    Type
    'void'

Throws:
    'throws'  sepBy1(TypeName, ',')

MethodBody:
    Block 
    ';'

ConstructorDeclaration:
    ConstructorModifier(*)  TypeParameters(?)  SimpleTypeName  FormalParameterList  Throws(?)  ConstructorBody

ConstructorModifier:
    Annotation  |  'public'  |  'protected'  |  'private'

ConstructorBody:
    '{'  ConstructorInvocation(?)  BlockStatement(*)  '}'

ConstructorInvocation:
    NonWildTypeArguments(?)  'this'  ArgumentList  ';'
    NonWildTypeArguments(?)  'super'  ArgumentList  ';'
    Primary  '.'  NonWildTypeArguments(?)  'super'  ArgumentList  ';'

NonWildTypeArguments:
    '<'  sepBy1(ReferenceType, ',')  '>'

Enum:
    ClassModifiers(?)  'enum'  Identifier  Interfaces(?)  EnumBody

EnumBody:
    '{'  sepBy0(EnumConstant, ',')  ','(?)  ( ';'  ClassBodyDeclaration(*) )(?)  '}'

EnumConstant:
    Annotation(*)  Identifier  ArgumentList(?)  ClassBody(?)


Interface:
    InterfaceModifier(*)  'interface'  Identifier  TypeParameters(?)  ExtendsInterfaces(?)  '{'  InterfaceMember(*)  '}'

InterfaceModifier:
    Annotation  |  'public'  |  'protected'  |  'private'
    'abstract'  |  'static'  |  'strictfp'

ExtendsInterfaces:
    'extends'  InterfaceTypeList

InterfaceMember:
    ConstantDeclaration
    AbstractMethodDeclaration
    TypeDeclaration

ConstantDeclaration:
    ConstantModifier(*)  Type  VariableDeclarators  ';'

ConstantModifier:
    Annotation  |  'public'  |  'static'  |  'final'

AbstractMethodDeclaration:
    AbstractMethodModifier(*)  TypeParameters(?)  Result  MethodDeclarator  Throws(?)  ';'

AbstractMethodModifier:
    Annotation  |  'public'  |  'abstract'

AnnotationType:
    InterfaceModifier(*)  '@'  'interface'  Identifier  '{'  AnnotationTypeElement(*)  '}'

AnnotationTypeElement:
    AbstractMethodModifier(*)  Type  Identifier  '('  ')'  Dim(*)  DefaultValue(?)  ';'
    ConstantDeclaration
    TypeDeclaration

DefaultValue:
    'default'  ElementValue

Annotation:
    NormalAnnotation
    MarkerAnnotation
    SingleElementAnnotation

NormalAnnotation:
    '@'  TypeName  '('  sepBy0(ElementValuePair, ',')  ')'

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
    LocalVariableDeclaration  ';'
    Class
    Enum
    Statement

LocalVariableDeclaration:
    VariableModifier(*)  Type  VariableDeclarators

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
    LocalVariableDeclaration

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
    'catch'  '('  VariableModifier(*)  CatchType  IdentBraces  ')'  Block

CatchType:
    sepBy1(ClassType, '|')

Finally:
    'finally'  Block

TryWithResourcesStatement:
    'try'  ResourceSpecification  Block  CatchClause(*)  Finally(?)

ResourceSpecification:
    '('  sepBy1(Resource, ';')  ';'(?)  ')'

Resource:
    VariableModifier(*)  Type  IdentBraces  '='  Expression


Primary:
    PrimaryNoNewArray
    ArrayCreationExpression

PrimaryNoNewArray:
    Literal
    Type  '.'  'class'
    'void'  '.'  'class'
    'this'
    ClassName  '.'  'this'
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
    'new'  PrimitiveType  DimExpr(+)  Dim(*)
    'new'  ClassOrInterfaceType  DimExpr(+)  Dim(*)
    'new'  PrimitiveType  Dim(+)  ArrayInitializer 
    'new'  ClassOrInterfaceType  Dim(+)  ArrayInitializer

DimExpr:
    '['  Expression  ']'

Dim:
    '['  ']'

FieldAccess: 
    Primary  '.'  Identifier
    'super'  '.'  Identifier
    ClassName  '.'  'super'  '.'  Identifier

MethodInvocation:
    MethodName  ArgumentList
    Primary  '.'  NonWildTypeArguments(?)  Identifier  ArgumentList
    'super'  '.'  NonWildTypeArguments(?)  Identifier  ArgumentList
    ClassName  '.'  'super'  '.'  NonWildTypeArguments(?)  Identifier  ArgumentList
    TypeName  '.'  NonWildTypeArguments  Identifier  ArgumentList

ArrayAccess:
    ExpressionName  '['  Expression  ']'
    PrimaryNoNewArray  '['  Expression  ']'

PostfixExpression:
    Primary
    ExpressionName
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
    '('  ReferenceType  ')'  UnaryExpressionNotPlusMinus

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
    RelationalExpression  'instanceof'  ReferenceType

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
    ExpressionName
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
