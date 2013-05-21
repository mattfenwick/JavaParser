Type:
    PrimitiveType
    ReferenceType
    
PrimitiveType:
    NumericType
    boolean

NumericType:
    IntegralType
    FloatingPointType

IntegralType: one of
    byte short int long char

FloatingPointType: one of
    float double

ReferenceType:
    ClassOrInterfaceType
    TypeVariable
    ArrayType

ClassOrInterfaceType:
    ClassType
    InterfaceType

ClassType:
    TypeDeclSpecifier TypeArgumentsopt

InterfaceType:
    TypeDeclSpecifier TypeArgumentsopt

TypeDeclSpecifier:
    TypeName  
    ClassOrInterfaceType . Identifier

TypeName:
    Identifier
    TypeName . Identifier

TypeVariable:
    Identifier

ArrayType:
    Type [ ]
    
TypeParameter:
    TypeVariable TypeBoundopt

TypeBound:
    extends TypeVariable
    extends ClassOrInterfaceType AdditionalBoundListopt

AdditionalBoundList:
    AdditionalBound AdditionalBoundList
    AdditionalBound

AdditionalBound:
    & InterfaceType

TypeArguments:
    < TypeArgumentList >

TypeArgumentList: 
    TypeArgument
    TypeArgumentList , TypeArgument

TypeArgument:
    ReferenceType
    Wildcard

Wildcard:
    ? WildcardBoundsopt

WildcardBounds:
    extends ReferenceType
    super ReferenceType


PackageName:
    Identifier
    PackageName . Identifier

TypeName:
    Identifier
    PackageOrTypeName . Identifier

ExpressionName:
    Identifier
    AmbiguousName . Identifier

MethodName:
    Identifier
    AmbiguousName . Identifier

PackageOrTypeName:
    Identifier
    PackageOrTypeName . Identifier

AmbiguousName:
    Identifier
    AmbiguousName . Identifier


CompilationUnit:
    PackageDeclarationopt ImportDeclarationsopt TypeDeclarationsopt

ImportDeclarations:
    ImportDeclaration
    ImportDeclarations ImportDeclaration

TypeDeclarations:
    TypeDeclaration
    TypeDeclarations TypeDeclaration

PackageDeclaration:
    Annotationsopt package PackageName ;

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
    ClassModifiersopt class Identifier TypeParametersopt
                                               Superopt Interfacesopt ClassBody

ClassModifiers:
    ClassModifier
    ClassModifiers ClassModifier

ClassModifier: one of
    Annotation public protected private
    abstract static final strictfp

TypeParameters:
    < TypeParameterList >

TypeParameterList:
    TypeParameterList , TypeParameter
    TypeParameter

Super:
    extends ClassType

Interfaces:
    implements InterfaceTypeList

InterfaceTypeList:
    InterfaceType
    InterfaceTypeList , InterfaceType

ClassBody:
    { ClassBodyDeclarationsopt }

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
    FieldModifiersopt Type VariableDeclarators ;

VariableDeclarators:
    VariableDeclarator
    VariableDeclarators , VariableDeclarator

VariableDeclarator:
    VariableDeclaratorId
    VariableDeclaratorId = VariableInitializer

VariableDeclaratorId:
    Identifier
    VariableDeclaratorId [ ]

VariableInitializer:
    Expression
    ArrayInitializer

FieldModifiers:
    FieldModifier
    FieldModifiers FieldModifier

FieldModifier: one of
    Annotation public protected private
    static final transient volatile

MethodDeclaration:
    MethodHeader MethodBody

MethodHeader:
    MethodModifiersopt TypeParametersopt Result MethodDeclarator Throwsopt

MethodDeclarator:
    Identifier ( FormalParameterListopt )

MethodDeclarator:
    MethodDeclarator [ ]

FormalParameterList:
    LastFormalParameter
    FormalParameters , LastFormalParameter

FormalParameters:
    FormalParameter
    FormalParameters , FormalParameter

FormalParameter:
    VariableModifiersopt Type VariableDeclaratorId

VariableModifiers:
    VariableModifier
    VariableModifiers VariableModifier

VariableModifier: one of
    Annotation final

LastFormalParameter:
    VariableModifiersopt Type... VariableDeclaratorId
    FormalParameter

MethodModifiers:
    MethodModifier
    MethodModifiers MethodModifier

MethodModifier: one of
    Annotation public protected private abstract
    static final synchronized native strictfp

Result:
    Type
    void

Throws:
    throws ExceptionTypeList

ExceptionTypeList:
    ExceptionType
    ExceptionTypeList , ExceptionType

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
    ConstructorModifiersopt ConstructorDeclarator
                                Throwsopt ConstructorBody

ConstructorDeclarator:
    TypeParametersopt SimpleTypeName ( FormalParameterListopt )

ConstructorModifiers:
    ConstructorModifier
    ConstructorModifiers ConstructorModifier

ConstructorModifier: one of
    Annotation public protected private

ConstructorBody:
    { ExplicitConstructorInvocationopt BlockStatementsopt }

ExplicitConstructorInvocation:
    NonWildTypeArgumentsopt this ( ArgumentListopt ) ;
    NonWildTypeArgumentsopt super ( ArgumentListopt ) ;
    Primary . NonWildTypeArgumentsopt super ( ArgumentListopt ) ;

NonWildTypeArguments:
    < ReferenceTypeList >

ReferenceTypeList: 
    ReferenceType
    ReferenceTypeList , ReferenceType

EnumDeclaration:
    ClassModifiersopt enum Identifier Interfacesopt EnumBody

EnumBody:
    { EnumConstantsopt ,opt EnumBodyDeclarationsopt }

EnumConstants:
    EnumConstant
    EnumConstants , EnumConstant

EnumConstant:
    Annotationsopt Identifier Argumentsopt ClassBodyopt

Arguments:
    ( ArgumentListopt )

EnumBodyDeclarations:
    ; ClassBodyDeclarationsopt


InterfaceDeclaration:
    NormalInterfaceDeclaration
    AnnotationTypeDeclaration

NormalInterfaceDeclaration:
    InterfaceModifiersopt interface Identifier
    TypeParametersopt ExtendsInterfacesopt InterfaceBody

InterfaceModifiers:
    InterfaceModifier
    InterfaceModifiers InterfaceModifier

InterfaceModifier: one of
    Annotation public protected private
    abstract static strictfp

ExtendsInterfaces:
    extends InterfaceTypeList

InterfaceBody:
    { InterfaceMemberDeclarationsopt }

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
    ConstantModifiersopt Type VariableDeclarators ;

ConstantModifiers: 
    ConstantModifier
    ConstantModifier ConstantModifers 

ConstantModifier: one of
    Annotation public static final

AbstractMethodDeclaration:
    AbstractMethodModifiersopt TypeParametersopt Result
                                           MethodDeclarator Throwsopt ;

AbstractMethodModifiers:
    AbstractMethodModifier
    AbstractMethodModifiers AbstractMethodModifier

AbstractMethodModifier: one of
    Annotation public abstract

AnnotationTypeDeclaration:
    InterfaceModifiersopt @ interface Identifier AnnotationTypeBody

AnnotationTypeBody:
    { AnnotationTypeElementDeclarationsopt }

AnnotationTypeElementDeclarations:
    AnnotationTypeElementDeclaration
    AnnotationTypeElementDeclarations AnnotationTypeElementDeclaration

AnnotationTypeElementDeclaration:
    AbstractMethodModifiersopt Type Identifier ( ) Dimsopt DefaultValueopt ;
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
    @ TypeName ( ElementValuePairsopt )

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
    { ElementValuesopt ,opt }

ElementValues:
    ElementValue
    ElementValues , ElementValue

MarkerAnnotation:
    @ Identifier

SingleElementAnnotation:
    @ Identifier ( ElementValue )


ArrayInitializer:
    { VariableInitializersopt ,opt }

VariableInitializers:
    VariableInitializer
    VariableInitializers , VariableInitializer


Block:
    { BlockStatementsopt }

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
    VariableModifiersopt Type VariableDeclarators

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
    { SwitchBlockStatementGroupsopt SwitchLabelsopt }

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
    for ( ForInitopt ; Expressionopt ; ForUpdateopt ) Statement

ForStatementNoShortIf:
    for ( ForInitopt ; Expressionopt ; ForUpdateopt ) StatementNoShortIf

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
    break Identifieropt ;

ContinueStatement:
    continue Identifieropt ;

ReturnStatement:
    return Expressionopt ;

ThrowStatement:
    throw Expression ;

SynchronizedStatement:
    synchronized ( Expression ) Block

TryStatement:
    try Block Catches
    try Block Catchesopt Finally
    TryWithResourcesStatement

Catches:
    CatchClause
    Catches CatchClause

CatchClause:
    catch ( CatchFormalParameter ) Block

CatchFormalParameter:
    VariableModifiersopt CatchType VariableDeclaratorId

CatchType:
    ClassType
    ClassType | CatchType 

Finally:
    finally Block

TryWithResourcesStatement:
    try ResourceSpecification Block Catchesopt Finallyopt

ResourceSpecification:
    ( Resources ;opt )

Resources:
    Resource
    Resource ; Resources

Resource:
    VariableModifiersopt Type VariableDeclaratorId = Expression


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
    new TypeArgumentsopt TypeDeclSpecifier TypeArgumentsOrDiamondopt
                                                            ( ArgumentListopt ) ClassBodyopt
    Primary . new TypeArgumentsopt Identifier TypeArgumentsOrDiamondopt
                                                            ( ArgumentListopt ) ClassBodyopt

TypeArgumentsOrDiamond:
    TypeArguments
    <> 

ArgumentList:
    Expression
    ArgumentList , Expression

ArrayCreationExpression:
    new PrimitiveType DimExprs Dimsopt
    new ClassOrInterfaceType DimExprs Dimsopt
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
    MethodName ( ArgumentListopt )
    Primary . NonWildTypeArgumentsopt Identifier ( ArgumentListopt )
    super . NonWildTypeArgumentsopt Identifier ( ArgumentListopt )
    ClassName . super . NonWildTypeArgumentsopt Identifier ( ArgumentListopt )
    TypeName . NonWildTypeArguments Identifier ( ArgumentListopt )

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

