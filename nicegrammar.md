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
    TypeDeclSpecifier TypeArguments(?)

InterfaceType:
    TypeDeclSpecifier TypeArguments(?)

TypeDeclSpecifier:
    TypeName  
    ClassOrInterfaceType . Identifier

TypeVariable:
    Identifier

ArrayType:
    Type  '['  ']'
    
TypeParameter:
    TypeVariable TypeBound(?)

TypeBound:
    extends  TypeVariable
    extends  ClassOrInterfaceType  ( '&'  InterfaceType )(*)

TypeArguments:
    '<'  sepBy1(TypeArgument, ',')  '>'

TypeArgument:
    ReferenceType
    '?'
    '?'  extends ReferenceType
    '?'  super ReferenceType


PackageName:
TypeName:
ExpressionName:
MethodName:
PackageOrTypeName:
AmbiguousName:
    sepBy1(Identifier, '.')


CompilationUnit:
    PackageDeclaration(?) ImportDeclaration(*) TypeDeclarations(*)

PackageDeclaration:
    Annotations(?) package PackageName ;

ImportDeclaration:
    SingleTypeImportDeclaration
    TypeImportOnDemandDeclaration   
    SingleStaticImportDeclaration   
    StaticImportOnDemandDeclaration

SingleTypeImportDeclaration:
    import TypeName ;

TypeImportOnDemandDeclaration:
    import PackageOrTypeName . * ;

SingleStaticImportDeclaration:
    import static TypeName . Identifier ;

StaticImportOnDemandDeclaration:
    import static TypeName . * ;

TypeDeclaration:
    ClassDeclaration
    InterfaceDeclaration
    ;


ClassDeclaration:
    NormalClassDeclaration
    EnumDeclaration

NormalClassDeclaration:
    ClassModifiers(*) class Identifier TypeParameters(?)
                                               Super(?) Interfaces(?) ClassBody

ClassModifier: one of
    Annotation public protected private
    abstract static final strictfp

TypeParameters:
    '<'  sepBy1(TypeParameter, ',')  '>'

Super:
    extends ClassType

Interfaces:
    implements InterfaceTypeList

InterfaceTypeList:
    InterfaceType
    InterfaceTypeList , InterfaceType

ClassBody:
    { ClassBodyDeclarations(?) }

ClassBodyDeclarations:
    ClassBodyDeclaration
    ClassBodyDeclarations ClassBodyDeclaration

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
    ;

FieldDeclaration:
    FieldModifier(*) Type VariableDeclarators ;

VariableDeclarators:
    sepBy1(VariableDeclarator, ',')

VariableDeclarator:
    VariableDeclaratorId
    VariableDeclaratorId = VariableInitializer

VariableDeclaratorId:
    Identifier
    VariableDeclaratorId [ ]

VariableInitializer:
    Expression
    ArrayInitializer

FieldModifier: one of
    Annotation public protected private
    static final transient volatile

MethodDeclaration:
    MethodHeader MethodBody

MethodHeader:
    MethodModifiers(?) TypeParameters(?) Result MethodDeclarator Throws(?)

MethodDeclarator:
    Identifier ( FormalParameterList(?) )

MethodDeclarator:
    MethodDeclarator [ ]

FormalParameterList:
    LastFormalParameter
    sepBy1(FormalParameter, ',')  ','  LastFormalParameter

FormalParameter:
    VariableModifiers(?) Type VariableDeclaratorId

VariableModifiers:
    VariableModifier(+)

VariableModifier: one of
    Annotation final

LastFormalParameter:
    VariableModifiers(?)  Type  '...'  VariableDeclaratorId
    FormalParameter

MethodModifiers:
    MethodModifier(+)

MethodModifier: one of
    Annotation public protected private abstract
    static final synchronized native strictfp

Result:
    Type
    void

Throws:
    throws ExceptionTypeList

ExceptionTypeList:
    sepBy1(ExceptionType, ',')

ExceptionType:
    TypeName 
    TypeVariable

MethodBody:
    Block 
    ;

InstanceInitializer:
    Block
    
StaticInitializer:
    static Block

ConstructorDeclaration:
    ConstructorModifiers(?) ConstructorDeclarator
                                Throws(?) ConstructorBody

ConstructorDeclarator:
    TypeParameters(?) SimpleTypeName ( FormalParameterList(?) )

ConstructorModifiers:
    ConstructorModifier(+)

ConstructorModifier: one of
    Annotation public protected private

ConstructorBody:
    { ExplicitConstructorInvocation(?) BlockStatements(?) }

ExplicitConstructorInvocation:
    NonWildTypeArguments(?) this ( ArgumentList(?) ) ;
    NonWildTypeArguments(?) super ( ArgumentList(?) ) ;
    Primary . NonWildTypeArguments(?) super ( ArgumentList(?) ) ;

NonWildTypeArguments:
    < ReferenceTypeList >

ReferenceTypeList: 
    ReferenceType
    ReferenceTypeList , ReferenceType

EnumDeclaration:
    ClassModifiers(?) enum Identifier Interfaces(?) EnumBody

EnumBody:
    { EnumConstants(?) ,(?) EnumBodyDeclarations(?) }

EnumConstants:
    EnumConstant
    EnumConstants , EnumConstant

EnumConstant:
    Annotations(?) Identifier Arguments(?) ClassBody(?)

Arguments:
    ( ArgumentList(?) )

EnumBodyDeclarations:
    ; ClassBodyDeclarations(?)


InterfaceDeclaration:
    NormalInterfaceDeclaration
    AnnotationTypeDeclaration

NormalInterfaceDeclaration:
    InterfaceModifiers(?) interface Identifier
    TypeParameters(?) ExtendsInterfaces(?) InterfaceBody

InterfaceModifiers:
    InterfaceModifier
    InterfaceModifiers InterfaceModifier

InterfaceModifier: one of
    Annotation public protected private
    abstract static strictfp

ExtendsInterfaces:
    extends InterfaceTypeList

InterfaceBody:
    { InterfaceMemberDeclarations(?) }

InterfaceMemberDeclarations:
    InterfaceMemberDeclaration
    InterfaceMemberDeclarations InterfaceMemberDeclaration

InterfaceMemberDeclaration:
    ConstantDeclaration
    AbstractMethodDeclaration
    ClassDeclaration 
    InterfaceDeclaration
    ;   

ConstantDeclaration:
    ConstantModifiers(?) Type VariableDeclarators ;

ConstantModifiers: 
    ConstantModifier
    ConstantModifier ConstantModifers 

ConstantModifier: one of
    Annotation public static final

AbstractMethodDeclaration:
    AbstractMethodModifiers(?) TypeParameters(?) Result
                                           MethodDeclarator Throws(?) ;

AbstractMethodModifiers:
    AbstractMethodModifier
    AbstractMethodModifiers AbstractMethodModifier

AbstractMethodModifier: one of
    Annotation public abstract

AnnotationTypeDeclaration:
    InterfaceModifiers(?) @ interface Identifier AnnotationTypeBody

AnnotationTypeBody:
    { AnnotationTypeElementDeclarations(?) }

AnnotationTypeElementDeclarations:
    AnnotationTypeElementDeclaration
    AnnotationTypeElementDeclarations AnnotationTypeElementDeclaration

AnnotationTypeElementDeclaration:
    AbstractMethodModifiers(?) Type Identifier ( ) Dims(?) DefaultValue(?) ;
    ConstantDeclaration
    ClassDeclaration
    InterfaceDeclaration
    EnumDeclaration
    AnnotationTypeDeclaration
    ;

DefaultValue:
    default ElementValue

Annotations:
    Annotation
    Annotations Annotation

Annotation:
    NormalAnnotation
    MarkerAnnotation
    SingleElementAnnotation

NormalAnnotation:
    @ TypeName ( ElementValuePairs(?) )

ElementValuePairs:
    ElementValuePair
    ElementValuePairs , ElementValuePair

ElementValuePair:
    Identifier = ElementValue

ElementValue:
    ConditionalExpression
    Annotation
    ElementValueArrayInitializer

ElementValueArrayInitializer:
    { ElementValues(?) ,(?) }

ElementValues:
    ElementValue
    ElementValues , ElementValue

MarkerAnnotation:
    @ Identifier

SingleElementAnnotation:
    @ Identifier ( ElementValue )


ArrayInitializer:
    { VariableInitializers(?) ,(?) }

VariableInitializers:
    VariableInitializer
    VariableInitializers , VariableInitializer


Block:
    { BlockStatements(?) }

BlockStatements:
    BlockStatement
    BlockStatements BlockStatement

BlockStatement:
    LocalVariableDeclarationStatement
    ClassDeclaration
    Statement

LocalVariableDeclarationStatement:
    LocalVariableDeclaration ;

LocalVariableDeclaration:
    VariableModifiers(?) Type VariableDeclarators

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
    ;

LabeledStatement:
    Identifier : Statement

LabeledStatementNoShortIf:
    Identifier : StatementNoShortIf

ExpressionStatement:
    StatementExpression ;

StatementExpression:
    Assignment
    PreIncrementExpression
    PreDecrementExpression
    PostIncrementExpression
    PostDecrementExpression
    MethodInvocation
    ClassInstanceCreationExpression

IfThenStatement:
    if ( Expression ) Statement

IfThenElseStatement:
    if ( Expression ) StatementNoShortIf else Statement

IfThenElseStatementNoShortIf:
    if ( Expression ) StatementNoShortIf else StatementNoShortIf

AssertStatement:
    assert Expression1 ;
    assert Expression1 : Expression2 ;

SwitchStatement:
    switch ( Expression ) SwitchBlock

SwitchBlock:
    { SwitchBlockStatementGroups(?) SwitchLabels(?) }

SwitchBlockStatementGroups:
    SwitchBlockStatementGroup
    SwitchBlockStatementGroups SwitchBlockStatementGroup

SwitchBlockStatementGroup:
    SwitchLabels BlockStatements

SwitchLabels:
    SwitchLabel
    SwitchLabels SwitchLabel

SwitchLabel:
    case ConstantExpression :
    case EnumConstantName :
    default :

EnumConstantName:
    Identifier

WhileStatement:
    while ( Expression ) Statement

WhileStatementNoShortIf:
    while ( Expression ) StatementNoShortIf

DoStatement:
    do Statement while ( Expression ) ;

ForStatement:
    BasicForStatement
    EnhancedForStatement

BasicForStatement:
    for ( ForInit(?) ; Expression(?) ; ForUpdate(?) ) Statement

ForStatementNoShortIf:
    for ( ForInit(?) ; Expression(?) ; ForUpdate(?) ) StatementNoShortIf

ForInit:
    StatementExpressionList
    LocalVariableDeclaration

ForUpdate:
    StatementExpressionList

StatementExpressionList:
    StatementExpression
    StatementExpressionList , StatementExpression

EnhancedForStatement:
    for ( FormalParameter : Expression ) Statement

BreakStatement:
    break Identifier(?) ;

ContinueStatement:
    continue Identifier(?) ;

ReturnStatement:
    return Expression(?) ;

ThrowStatement:
    throw Expression ;

SynchronizedStatement:
    synchronized ( Expression ) Block

TryStatement:
    try Block Catches
    try Block Catches(?) Finally
    TryWithResourcesStatement

Catches:
    CatchClause
    Catches CatchClause

CatchClause:
    catch ( CatchFormalParameter ) Block

CatchFormalParameter:
    VariableModifiers(?) CatchType VariableDeclaratorId

CatchType:
    ClassType
    ClassType | CatchType 

Finally:
    finally Block

TryWithResourcesStatement:
    try ResourceSpecification Block Catches(?) Finally(?)

ResourceSpecification:
    ( Resources ;(?) )

Resources:
    Resource
    Resource ; Resources

Resource:
    VariableModifiers(?) Type VariableDeclaratorId = Expression


Primary:
    PrimaryNoNewArray
    ArrayCreationExpression

PrimaryNoNewArray:
    Literal
    Type . class
    void . class
    this
    ClassName . this
    ( Expression )
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
    new TypeArguments(?) TypeDeclSpecifier TypeArgumentsOrDiamond(?)
                                                            ( ArgumentList(?) ) ClassBody(?)
    Primary . new TypeArguments(?) Identifier TypeArgumentsOrDiamond(?)
                                                            ( ArgumentList(?) ) ClassBody(?)

TypeArgumentsOrDiamond:
    TypeArguments
    <> 

ArgumentList:
    Expression
    ArgumentList , Expression

ArrayCreationExpression:
    new PrimitiveType DimExprs Dims(?)
    new ClassOrInterfaceType DimExprs Dims(?)
    new PrimitiveType Dims ArrayInitializer 
    new ClassOrInterfaceType Dims ArrayInitializer

DimExprs:
    DimExpr
    DimExprs DimExpr

DimExpr:
    [ Expression ]

Dims:
    [ ]
    Dims [ ]

FieldAccess: 
    Primary . Identifier
    super . Identifier
    ClassName . super . Identifier

MethodInvocation:
    MethodName ( ArgumentList(?) )
    Primary . NonWildTypeArguments(?) Identifier ( ArgumentList(?) )
    super . NonWildTypeArguments(?) Identifier ( ArgumentList(?) )
    ClassName . super . NonWildTypeArguments(?) Identifier ( ArgumentList(?) )
    TypeName . NonWildTypeArguments Identifier ( ArgumentList(?) )

ArrayAccess:
    ExpressionName [ Expression ]
    PrimaryNoNewArray [ Expression ]

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

