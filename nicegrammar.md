Type:
    PrimitiveType
    ReferenceType
    
PrimitiveType: one of
    byte short int long char float double boolean

ReferenceType:
    ClassOrInterfaceType
    TypeVariable
    ArrayType

ClassOrInterfaceType:
    ClassType
    InterfaceType

ClassType:
    TypeDeclSpecifier  TypeArguments(?)

InterfaceType:
    TypeDeclSpecifier  TypeArguments(?)

TypeDeclSpecifier:
    TypeName  
    ClassOrInterfaceType  '.'  Identifier

TypeVariable:
    Identifier

ArrayType:
    Type  '['  ']'
    
TypeParameter:
    TypeVariable  TypeBound(?)

TypeBound:
    'extends'  TypeVariable
    'extends'  ClassOrInterfaceType  ( '&'  InterfaceType )(*)

TypeArguments:
    '<'  sepBy1(TypeArgument, ',')  '>'

TypeArgument:
    ReferenceType
    '?'
    '?'  'extends'  ReferenceType
    '?'  'super'  ReferenceType


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
    ClassDeclaration
    InterfaceDeclaration
    ';'


ClassDeclaration:
    NormalClassDeclaration
    EnumDeclaration

NormalClassDeclaration:
    ClassModifiers(*)  'class'  Identifier  TypeParameters(?)  Super(?)  Interfaces(?)  ClassBody

ClassModifier: one of
    Annotation  'public'  'protected'  'private
    'abstract'  'static'  'final'  'strictfp

TypeParameters:
    '<'  sepBy1(TypeParameter, ',')  '>'

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
    InstanceInitializer
    StaticInitializer
    ConstructorDeclaration

ClassMemberDeclaration:
    FieldDeclaration
    MethodDeclaration
    ClassDeclaration    
    InterfaceDeclaration
    ';'

FieldDeclaration:
    FieldModifier(*)  Type  VariableDeclarators  ';'

VariableDeclarators:
    sepBy1(VariableDeclarator, ',')

VariableDeclarator:
    VariableDeclaratorId
    VariableDeclaratorId  '='  VariableInitializer

VariableDeclaratorId:
    Identifier
    VariableDeclaratorId  '['  ']'

VariableInitializer:
    Expression
    ArrayInitializer

FieldModifier: one of
    Annotation  'public'  'protected'  'private'
    'static'  'final'  'transient'  'volatile'

MethodDeclaration:
    MethodHeader  MethodBody

MethodHeader:
    MethodModifier(*)  TypeParameters(?)  Result  MethodDeclarator  Throws(?)

MethodDeclarator:
    Identifier  '('  FormalParameterList(?)  ')'
    MethodDeclarator  '['  ']'         <-- spec says this is obsolete and don't use it!!!!  instead use the first alternative

FormalParameterList:
    LastFormalParameter
    sepBy1(FormalParameter, ',')  ','  LastFormalParameter

FormalParameter:
    VariableModifier(*)  Type  VariableDeclaratorId

VariableModifier: one of
    Annotation  'final'

LastFormalParameter:
    VariableModifier(*)  Type  '...'  VariableDeclaratorId
    FormalParameter

MethodModifier: one of
    Annotation  'public'  'protected'  'private'  'abstract'
    'static'  'final'  'synchronized'  'native'  'strictfp'

Result:
    Tpype
    'void'

Throws:
    'throws'  sepBy1(ExceptionType, ',')

ExceptionType:
    TypeName 
    TypeVariable

MethodBody:
    Block 
    ';'

InstanceInitializer:
    Block
    
StaticInitializer:
    'static'  Block

ConstructorDeclaration:
    ConstructorModifier(*)  ConstructorDeclarator  Throws(?)  ConstructorBody

ConstructorDeclarator:
    TypeParameters(?)  SimpleTypeName  '('  FormalParameterList(?)  ')'

ConstructorModifier: one of
    Annotation  'public'  'protected'  'private'

ConstructorBody:
    '{'  ExplicitConstructorInvocation(?)  BlockStatement(*)  '}'

ExplicitConstructorInvocation:
    NonWildTypeArguments(?)  'this'  '('  ArgumentList(?)  ')'  ';'
    NonWildTypeArguments(?)  'super'  '('  ArgumentList(?)  ')'  ';'
    Primary  '.'  NonWildTypeArguments(?)  'super'  '('  ArgumentList(?)  ')'  ';'

NonWildTypeArguments:
    '<'  sepBy1(ReferenceType, ',')  '>'

EnumDeclaration:
    ClassModifiers(?)  'enum'  Identifier  Interfaces(?)  EnumBody

EnumBody:
    '{'  sepBy0(EnumConstant, ',')  ','(?)  EnumBodyDeclarations(?)  '}'

EnumConstant:
    Annotation(*)  Identifier  Arguments(?)  ClassBody(?)

Arguments:
    '('  ArgumentList(?)  ')'

EnumBodyDeclarations:
    ';'  ClassBodyDeclaration(*)


InterfaceDeclaration:
    NormalInterfaceDeclaration
    AnnotationTypeDeclaration

NormalInterfaceDeclaration:
    InterfaceModifier(*)  'interface'  Identifier
    TypeParameters(?)  ExtendsInterfaces(?) p InterfaceBody

InterfaceModifier: one of
    Annotation  'public'  'protected'  'private'
    'abstract'  'static'  'strictfp'

ExtendsInterfaces:
    'extends'  InterfaceTypeList

InterfaceBody:
    '{'  InterfaceMemberDeclaration(*)  '}'

InterfaceMemberDeclaration:
    ConstantDeclaration
    AbstractMethodDeclaration
    ClassDeclaration 
    InterfaceDeclaration
    ';'   

ConstantDeclaration:
    ConstantModifier(*)  Type  VariableDeclarators  ';'

ConstantModifier: one of
    Annotation  'public'  'static'  'final'

AbstractMethodDeclaration:
    AbstractMethodModifier(*)  TypeParameters(?)  Result  MethodDeclarator  Throws(?)  ';'

AbstractMethodModifier: one of
    Annotation  'public'  'abstract'

AnnotationTypeDeclaration:
    InterfaceModifier(*)  '@'  'interface'  Identifier  AnnotationTypeBody

AnnotationTypeBody:
    '{'  AnnotationTypeElementDeclarations(?)  '}'

AnnotationTypeElementDeclarations:
    AnnotationTypeElementDeclaration(+)

AnnotationTypeElementDeclaration:
    AbstractMethodModifier(*)  Type  Identifier  '('  ')'  Dim(*)  DefaultValue(?)  ';'
    ConstantDeclaration
    ClassDeclaration
    InterfaceDeclaration
    EnumDeclaration
    AnnotationTypeDeclaration
    ';'

DefaultValue:
    'default'  ElementValue

Annotation:
    NormalAnnotation
    MarkerAnnotation
    SingleElementAnnotation

NormalAnnotation:
    '@'  TypeName  '('  ElementValuePairs(?)  ')'

ElementValuePairs:
    sepBy1(ElementValuePair, ',')

ElementValuePair:
    Identifier  '='  ElementValue

ElementValue:
    ConditionalExpression
    Annotation
    ElementValueArrayInitializer

ElementValueArrayInitializer:
    '{'  ElementValues(?)  ','(?)  '}'

ElementValues:
    sepBy1(ElementValue, ',')

MarkerAnnotation:
    '@'  Identifier

SingleElementAnnotation:
    '@'  Identifier  '('  ElementValue  ')'


ArrayInitializer:
    '{'  VariableInitializers(?)  ','(?)  '}'

VariableInitializers:
    sepBy1(VariableInitializer, ',')


Block:
    '{'  BlockStatement(*)  '}'

BlockStatement:
    LocalVariableDeclarationStatement
    ClassDeclaration
    Statement

LocalVariableDeclarationStatement:
    LocalVariableDeclaration  ';'

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
    switch  '('  Expression  ')'  SwitchBlock

SwitchBlock:
    '{'  SwitchBlockStatementGroups(?)  SwitchLabel(*)  '}'

SwitchBlockStatementGroups:
    SwitchBlockStatementGroup(+)

SwitchBlockStatementGroup:
    SwitchLabel(+)  BlockStatement(+)

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
    'catch'  '('  CatchFormalParameter  ')'  Block

CatchFormalParameter:
    VariableModifier(*)  CatchType  VariableDeclaratorId

CatchType:
    sepBy1(ClassType, '|')

Finally:
    'finally'  Block

TryWithResourcesStatement:
    'try'  ResourceSpecification  Block  CatchClause(*)  Finally(?)

ResourceSpecification:
    '('  sepBy1(Resource, ';')  ';'(?)  ')'

Resources:
    sepBy1(Resource, ';')

Resource:
    VariableModifier(*)  Type  VariableDeclaratorId  '='  Expression


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
    'new'  TypeArguments(?)  TypeDeclSpecifier  TypeArgumentsOrDiamond(?)  '('  ArgumentList(?)  ')'  ClassBody(?)
    Primary  '.'  'new'  TypeArguments(?)  Identifier  TypeArgumentsOrDiamond(?)  '('  ArgumentList(?)  ')'  ClassBody(?)

TypeArgumentsOrDiamond:
    TypeArguments
    '<'  '>' 

ArgumentList:
    sepBy1(Expression, ',')

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
    MethodName  '('  ArgumentList(?)  ')'
    Primary  '.'  NonWildTypeArguments(?)  Identifier  '('  ArgumentList(?)  ')'
    'super'  '.'  NonWildTypeArguments(?)  Identifier  '('  ArgumentList(?)  ')'
    ClassName  '.'  'super'  '.'  NonWildTypeArguments(?)  Identifier  '('  ArgumentList(?)  ')'
    TypeName  '.'  NonWildTypeArguments  Identifier  '('  ArgumentList(?)  ')'

ArrayAccess:
    ExpressionName  '['  Expression  ']'
    PrimaryNoNewArray  '['  Expression  ']'

PostfixExpression:
    Primary
    ExpressionName
    PostIncrementExpression
    PostDecrementExpression

PostIncrementExpression:
    PostfixExpression ++

PostDecrementExpression:
    PostfixExpression --

UnaryExpression:
    PreIncrementExpression
    PreDecrementExpression
    + UnaryExpression
    - UnaryExpression
    UnaryExpressionNotPlusMinus

PreIncrementExpression:
    ++ UnaryExpression

PreDecrementExpression:
    -- UnaryExpression

UnaryExpressionNotPlusMinus:
    PostfixExpression
    ~ UnaryExpression
    ! UnaryExpression
    CastExpression

CastExpression:
    ( PrimitiveType ) UnaryExpression
    ( ReferenceType ) UnaryExpressionNotPlusMinus

MultiplicativeExpression:
    UnaryExpression
    MultiplicativeExpression * UnaryExpression
    MultiplicativeExpression / UnaryExpression
    MultiplicativeExpression % UnaryExpression

AdditiveExpression:
    MultiplicativeExpression
    AdditiveExpression + MultiplicativeExpression
    AdditiveExpression - MultiplicativeExpression

ShiftExpression:
    AdditiveExpression
    ShiftExpression << AdditiveExpression
    ShiftExpression >> AdditiveExpression
    ShiftExpression >>> AdditiveExpression

RelationalExpression:
    ShiftExpression
    RelationalExpression < ShiftExpression
    RelationalExpression > ShiftExpression
    RelationalExpression <= ShiftExpression
    RelationalExpression >= ShiftExpression
    RelationalExpression instanceof ReferenceType

EqualityExpression:
    RelationalExpression
    EqualityExpression == RelationalExpression
    EqualityExpression != RelationalExpression

AndExpression:
    EqualityExpression
    AndExpression & EqualityExpression

ExclusiveOrExpression:
    AndExpression
    ExclusiveOrExpression ^ AndExpression

InclusiveOrExpression:
    ExclusiveOrExpression
    InclusiveOrExpression | ExclusiveOrExpression

ConditionalAndExpression:
    InclusiveOrExpression
    ConditionalAndExpression && InclusiveOrExpression

ConditionalOrExpression:
    ConditionalAndExpression
    ConditionalOrExpression || ConditionalAndExpression

ConditionalExpression:
    ConditionalOrExpression
    ConditionalOrExpression ? Expression : ConditionalExpression

AssignmentExpression:
    ConditionalExpression
    Assignment

Assignment:
    LeftHandSide AssignmentOperator AssignmentExpression

LeftHandSide:
    ExpressionName
    FieldAccess
    ArrayAccess

AssignmentOperator: one of
    = *= /= %= += -= <<= >>= >>>= &= ^= |=

Expression:
    AssignmentExpression

ConstantExpression:
    Expression
