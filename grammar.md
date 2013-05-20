    Braces:  
        '['  ']'

    Identifier:
        IDENTIFIER
    
    QualifiedIdentifier:
        sepBy1(Identifier, '.')
    
    QualifiedIdentifierList: 
        sepBy1(QualifiedIdentifier, ',')
    
    CompilationUnit: 
        ( Annotation(*)  'package'  QualifiedIdentifier  ';' )(?)  
        ImportDeclaration(*)  ( TypeDeclaration  |  ';' )(*)
    
    ImportDeclaration: 
        'import'  'static'(?)  sepBy1(Identifier, '.')  ( '.'  '*' )(?)  ';'
    
    TypeDeclaration: 
        Modifier(*)  ( ClassDeclaration    |  EnumDeclaration    |  
                       Interface           |  AnnotationType )
    
    
    ClassDeclaration: 
        'class'  Identifier  TypeParameters(?)  ( 'extends'  Type )(?)  
        ( 'implements'  TypeList )(?)  ClassBody
    
    EnumDeclaration:
        'enum'  Identifier  ( 'implements'  TypeList )(?)  EnumBody
    
    Interface: 
        'interface'  Identifier  TypeParameters(?)  ( 'extends'  TypeList )(?)  InterfaceBody
    
    AnnotationType:
        '@'  'interface'  Identifier  AnnotationTypeBody
    
    Type:
        ( BasicType  |  ReferenceType )  Braces(*)
    
    BasicType: 
        'byte'   |   'short'   |   'char'     |   'int'      |
        'long'   |   'float'   |   'double'   |   'boolean'
    
    ReferenceType:
        sepBy1(Identifier  typeargs(?), '.')
      where
        typeargs = '<'  sepBy1(TypeArgument, ',')  '>'
    
    TypeArgument:  
        ReferenceType  |
        ( '?'  ( ( 'extends'  |  'super' )  ReferenceType )(?) )
    
    NonWildcardTypeArguments:
        '<'  TypeList  '>'
    
    TypeList:  
        sepBy1(ReferenceType, ',')
    
    
    TypeParameters:
        '<'  sepBy1(TypeParameter, ',')  '>'
    
    TypeParameter:
        Identifier  ( 'extends'  sepBy1(ReferenceType, '&') )(?)
    
    Modifier: 
        Annotation     |    'public'      |    'protected'     |
        'private'      |    'static'      |    'abstract'      |
        'final'        |    'native'      |    'synchronized'  |
        'transient'    |    'volatile'    |    'strictfp'      
    
    Annotation:
        '@'  QualifiedIdentifier  ( '('  AnnotationElement(?)  ')' )(?)
    
    AnnotationElement:
        sepBy1(Identifier  '='  ElementValue, ',')  |
        ElementValue
    
    ElementValue:
        Annotation    |
        Expression1   |
        ElementValueArrayInitializer
    
    ElementValueArrayInitializer:
        '{'  sepBy0(ElementValue, ',')  ','(?)  '}'
    
    ClassBody: 
        '{'  ClassBodyDeclaration(*)  '}'
    
    ClassBodyDeclaration:
        ';'                           | 
        ( Modifier(*)  ClassMember )  |
        ( 'static'(?)  Block )
    
    ClassMember:
        Method              |
        Field               |
        Constructor         |
        ClassDeclaration    |
        EnumDeclaration     |
        Interface           |
        AnnotationType
    
    Method:
        TypeParameters(?)  ( Type  |  'void' )  Identifier  FormalParameters  Braces(*)  
        ( 'throws'  QualifiedIdentifierList )(?)  ( Block  |  ';' )
    
    Field:
        Type Identifier  VariableDeclaratorRest  ( ','  VariableDeclarator )(*)  ';'

    Constructor:
        TypeParameters(?)  Identifier  FormalParameters  
        ( 'throws'  QualifiedIdentifierList )(?)  Block
    
    InterfaceBody: 
        '{'  ( ';'  |  ( Modifier(*)  InterfaceMember ) )(*)  '}'
    
    InterfaceMember:
        InterfaceField      |
        InterfaceMethod     |
        ClassDeclaration    |
        EnumDeclaration     |
        Interface           |
        AnnotationType
    
    InterfaceField:
        Type  Identifier  ConstantDeclaratorsRest  ';'
    
    ConstantDeclaratorsRest: 
        ConstantDeclaratorRest  ( ','  Identifier  ConstantDeclaratorRest )(*)
    
    ConstantDeclaratorRest: 
        Braces(*)  '='  VariableInitializer
    
    InterfaceMethod:
        TypeParameters(?)  ( Type  |  'void' )  Identifier  FormalParameters  
        Braces(*)  ( 'throws'  QualifiedIdentifierList )(?)  ';'
    
    FormalParameters: 
        '('  FormalParameterDecls(?)  ')'
    
    FormalParameterDecls: 
        VariableModifier(*)  Type  FormalParameterDeclsRest
    
    VariableModifier:
        'final'     |
        Annotation
    
    FormalParameterDeclsRest: 
        ( VariableDeclaratorId  ( ','  FormalParameterDecls )(?) )  |
        ( '...'  VariableDeclaratorId )
    
    
    VariableDeclaratorId:
        Identifier  Braces(*)
    
    
    VariableDeclarators:
        sepBy1(VariableDeclarator, ',')
    
    VariableDeclarator:
        Identifier  VariableDeclaratorRest
    
    VariableDeclaratorRest:
        Braces(*)  ( '='  VariableInitializer )(?)
    
    VariableInitializer:
        ArrayInitializer  |
        Expression
    
    ArrayInitializer:
        '{'  ( sepBy1(VariableInitializer, ',')  ','(?) )(?)  '}'
    
    Block: 
        '{'  BlockStatement(*)  '}'
    
    BlockStatement:
        LocalVariableDeclarationStatement      |
        TypeDeclaration                        |
        ( ( Identifier  ':' )(?)  Statement )
    
    LocalVariableDeclarationStatement:
        VariableModifier(*)  Type  VariableDeclarators  ';'
    
    Statement:
        Block                                                           |
        ';'                                                             |
        ( Identifier  ':'  Statement )                                  |
        ( Expression  ';' )                                             |
        ( 'if'  ParExpression  Statement  ( 'else'  Statement )(?) )    | 
        ( 'assert'    Expression   ( ':'  Expression )(?)  ';'   )      |
        ( 'switch'    ParExpression  '{'      SwitchBlock  '}'   )      | 
        ( 'while'     ParExpression           Statement          )      |
        ( 'do'        Statement      'while'  ParExpression  ';' )      |
        ( 'for'  '('  ForControl     ')'      Statement          )      |
        ( 'break'     Identifier(?)  ';' )                              |
        ( 'continue'  Identifier(?)  ';' )                              |
        ( 'return'    Expression(?)  ';' )                              |
        ( 'throw'     Expression     ';' )                              |
        ( 'synchronized'  ParExpression  Block )                        |
        ( 'try'  Block  ( Catch(+)  |  ( Catch(*)  Finally ) ) )        |
        ( 'try'  ResourceSpecification  Block  Catch(*)  Finally(?) )
    
    Catch:
        'catch'  '('  VariableModifier(*)  sepBy1(QualfiedIdentifier, '|')  Identifier  ')'  Block
    
    Finally:
        'finally'  Block
    
    ResourceSpecification:
        '('  sepBy1(Resource, ';')  ';'(?)  ')'
    
    Resource:
        VariableModifier(*)  ReferenceType  VariableDeclaratorId  '='  Expression 
    
    SwitchBlock: 
        ( SwitchLabel(+)  BlockStatement(*) )(*)
    
    SwitchLabel: 
        ( 'case'  Expression  ':'       )  |
        ( 'case'  EnumConstantName  ':' )  |
        ( 'default'  ':' )
    
    EnumConstantName:
        Identifier
    
    
    ForControl:
        ForVarControl   |
        ( ForInit  ';'  Expression(?)  ';'  ForUpdate(?) )
    
    ForVarControl:
        VariableModifier(*)  Type  VariableDeclaratorId  ForVarControlRest
    
    ForVarControlRest:
        ( ForVariableDeclaratorsRest  ';'  Expression(?)  ';'  ForUpdate(?) )   |
        ( ':'  Expression )
    
    ForVariableDeclaratorsRest:
        ( '='  VariableInitializer )(?)  ( ','  VariableDeclarator )(*)
    
    ForInit: 
        sepBy1(Expression, ',')
    
    ForUpdate:
        sepBy1(Expression, ',')
    
    Expression: 
        Expression1  ( AssignmentOperator  Expression1 )(?)
    
    AssignmentOperator: 
        '='    |  '+='   |  '-='   |  '*='   |
        '/='   |  '&='   |  '|='   |  '^='   |
        '%='   |  '<<='  |  '>>='  |  '>>>='
    
    Expression1: 
        Expression2  Expression1Rest(?)
    
    Expression1Rest: 
        '?'  Expression  ':'  Expression1
    
    Expression2:
        Expression3  Expression2Rest(?)
    
    Expression2Rest:
        ( InfixOp Expression3 )(*)   |
        ( 'instanceof'  Type )
    
    InfixOp: 
        '||'  |  '&&'  |  '|'   |  '^'   |
        '&'   |  '=='  |  '!='  |  '<'   |
        '>'   |  '<='  |  '>='  |  '<<'  |
        '>>'  |  '>>>' |  '+'   |  '-'   |
        '*'   |  '/'   |  '%'
    
    Expression3: 
        ( PrefixOp  Expression3 )                           |
        ( '('  ( Expression  |  Type )  ')'  Expression3 )  |
        ( Primary  Selector(*)  PostfixOp(*) )
    
    PrefixOp: 
        '++'  |  '--'  |  '!'   |  '~'   |
        '+'   |  '-'
    
    PostfixOp: 
        '++'  |  '--'
    
    Primary: 
        Literal                                                               |
        ParExpression                                                         |
        ( 'this'  Arguments(?) )                                              |
        ( 'super'  SuperSuffix )                                              |
        ( 'new'  Creator )                                                    |
        ( NonWildcardTypeArguments  ( ExplicitGenericInvocationSuffix  |  
                                      ( 'this'  Arguments ) ) )               |
        ( sepBy1(Identifier, '.')  IdentifierSuffix(?) )                      |
        ( BasicType  Braces(*)  '.'  'class' )                                |
        ( 'void'  '.'  'class' )
    
    
    Literal:
        IntegerLiteral        |
        FloatingPointLiteral  |
        CharacterLiteral      |
        StringLiteral         |
        BooleanLiteral        |
        NullLiteral
    
    ParExpression: 
        '('  Expression  ')'
    
    Arguments:
        '('  sepBy0(Expression, ',')  ')'
    
    SuperSuffix: 
        Arguments   | 
        ( '.'  Identifier  Arguments(?) )
    
    ExplicitGenericInvocationSuffix: 
        ( 'super'  SuperSuffix )  |
        ( Identifier  Arguments )
    
    Creator:  
        ( NonWildcardTypeArguments  CreatedName  ClassCreatorRest )  |
        ( CreatedName  ( ClassCreatorRest  |  ArrayCreatorRest ) )
    
    CreatedName:
        sepBy1(x, '.')
      where
        x = Identifier  ( ( '<'  '>' )  |  TypeArguments )(?)
    
    ClassCreatorRest: 
        Arguments  ClassBody(?)
    
    ArrayCreatorRest:
        '['  ( ( ']'  
                 Braces(*)
                 ArrayInitializer )  |  
               ( Expression  
                 ']'  
                 ( '['  Expression  ']' )(*)  
                 Braces(*) ) )
    
    
    
    IdentifierSuffix:
        ( '['  ( ( Braces(*)  '.'  'class' )  |  
                 Expression )
          ']' )         |
        Arguments       |
        ( '.'  ( 'class'                   | 
                 ExplicitGenericInvocation | 
                 'this'                    | 
                 ( 'super'  Arguments )    |
                 ( 'new'  NonWildcardTypeArguments(?)  InnerCreator ) ) )
    
    ExplicitGenericInvocation:
        NonWildcardTypeArguments  ExplicitGenericInvocationSuffix
    
    InnerCreator:  
        Identifier  ( ( '<'  '>' )  |  NonWildcardTypeArguments )(?)  ClassCreatorRest
    
    
    Selector:
        ( '.'  Identifier  Arguments(?) )                          |
        ( '.'  ExplicitGenericInvocation )                         |
        ( '.'  'this' )                                            |
        ( '.'  'super'  SuperSuffix )                              |
        ( '.'  'new'  NonWildcardTypeArguments(?)  InnerCreator )  |
        ( '['  Expression  ']' )
    
    EnumBody:
        '{'  EnumConstants(?)  ','(?)  EnumBodyDeclarations(?)  '}'
    
    EnumConstants:
        sepBy1(EnumConstant, ',')
    
    EnumConstant:
        Annotation(*)  Identifier  Arguments(?)  ClassBody(?)
    
    EnumBodyDeclarations:
        ';'  ClassBodyDeclaration(*)
    
    
    AnnotationTypeBody:
        '{'  ( Modifier(*)  AnnotationTypeElementRest )(*)  '}'
    
    AnnotationTypeElementRest:
        ( Type  Identifier  AnnotationMethodOrConstantRest  ';' )  |
        ClassDeclaration                                           |
        EnumDeclaration                                            |
        Interface                                                  |
        AnnotationType
    
    AnnotationMethodOrConstantRest:
        AnnotationMethodRest        |
        ConstantDeclaratorsRest  
    
    AnnotationMethodRest:
        '('  ')'  Braces(?)  ( 'default'  ElementValue )(?)
