    Braces:  
        '['  ']'

    Identifier:
        IDENTIFIER
    
    QualifiedIdentifier:
        sepBy1(Identifier, '.')
    
    CompilationUnit: 
        ( Annotation(*)  'package'  QualifiedIdentifier  ';' )(?)  
        ImportDeclaration(*)  
        ( ( Modifier(*)  TypeDeclaration )  |  ';' )(*)
    
    ImportDeclaration: 
        'import'  'static'(?)  sepBy1(Identifier, '.')  ( '.'  '*' )(?)  ';'
    
    TypeDeclaration: 
        ClassDeclaration  |  EnumDeclaration  |  Interface  |  AnnotationType
    
    ClassDeclaration: 
        'class'  Identifier  TypeParameters(?)  ( 'extends'  Type )(?)  
        ( 'implements'  TypeList )(?)  ClassBody
    
    EnumDeclaration:
        'enum'  Identifier  ( 'implements'  TypeList )(?)
        '{'  sepBy0(EnumConstant, ',')  ','(?)  ( ';'  ClassBodyDeclaration(*) )(?)  '}'
    
    Interface: 
        'interface'  Identifier  TypeParameters(?)  ( 'extends'  TypeList )(?)
        '{'  ( ';'  |  ( Modifier(*)  InterfaceMember ) )(*)  '}'
    
    AnnotationType:
        '@'  'interface'  Identifier  '{'  AnnotationTypeElement(*)  '}'    
    
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
        ReferenceType                       |
        '?'                                 |
        ( '?'  'extends'  ReferenceType )   |
        ( '?'  'super'    ReferenceType )
    
    NonWildcardTypeArguments:
        '<'  TypeList  '>'
    
    TypeList:
        sepBy1(ReferenceType, ',')
    
    
    TypeParameters:
        '<'  sepBy1(TypeParameter, ',')  '>'
    
    TypeParameter:
        Identifier  'extends'  sepBy1(ReferenceType, '&')  |
        Identifier
    
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
        TypeDeclaration
    
    Method:
        TypeParameters(?)  ( Type  |  'void' )  Identifier  
        FormalParameters  Braces(*)  Throws(?)  ( Block  |  ';' )
    
    Throws:
        'throws'  sepBy1(QualifiedIdentifier, ',')
        
    Field:
        Type  sepBy1(VariableDeclarator, ',')  ';'

    Constructor:
        TypeParameters(?)  Identifier  FormalParameters  Throws(?)  Block
    
    InterfaceMember:
        InterfaceField      |
        InterfaceMethod     |
        TypeDeclaration
    
    InterfaceField:
        Type  sepBy1(Identifier  Braces(*)  '='  VariableInitializer, ',')  ';'
    
    InterfaceMethod:
        TypeParameters(?)  ( Type  |  'void' )  Identifier  
        FormalParameters  Braces(*)  Throws(?)  ';'
    
    FormalParameters: 
        '('  sepBy0(FormalParameter, ',')  VarArgs(?)  ')'
    
    FormalParameter:
        VariableModifier(*)  Type  IdentBraces
        
    VarArgs:
        VariableModifier(*)  Type  '...'  IdentBraces
    
    VariableModifier:
        'final'     |
        Annotation
    
    EnumConstant:
        Annotation(*)  Identifier  Arguments(?)  ClassBody(?)
    
    AnnotationTypeElement:
        Modifier(*)  ( AnnotationMethod  |  InterfaceField  |  TypeDeclaration )
    
    AnnotationMethod:
        Type  Identifier  '('  ')'  Braces(?)  ( 'default'  ElementValue )(?)  ';'
    
    IdentBraces:
        Identifier  Braces(*)
    
    VariableDeclarator:
        Identifier  Braces(*)  ( '='  VariableInitializer )(?)
    
    VariableInitializer:
        ArrayInitializer  |
        Expression
    
    ArrayInitializer:
        '{'  ( sepBy1(VariableInitializer, ',')  ','(?) )(?)  '}'
    
    Block: 
        '{'  BlockStatement(*)  '}'
    
    BlockStatement:
        LocalVariableDecl                      |
        Modifier(*)  TypeDeclaration           |
        Statement                              |
        ( Identifier  ':'  Statement )
    
    LocalVariableDecl:
        VariableModifier(*)  Type  sepBy1(VariableDeclarator, ',')  ';'
    
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
        'catch'  '('  VariableModifier(*)  sepBy1(QualifiedIdentifier, '|')  Identifier  ')'  Block
    
    Finally:
        'finally'  Block
    
    ResourceSpecification:
        '('  sepBy1(Resource, ';')  ';'(?)  ')'
    
    Resource:
        VariableModifier(*)  ReferenceType  IdentBraces  '='  Expression 
    
    SwitchBlock: 
        ( SwitchLabel(+)  BlockStatement(*) )(*)
    
    SwitchLabel: 
        ( 'case'  Expression  ':'       )  |
        ( 'case'  EnumConstantName  ':' )  |
        ( 'default'  ':' )
    
    EnumConstantName:
        Identifier
    
    
    ForControl:
        ForControl1   |  ForControl2  |  ForControl3

    ForControl1:  
        VariableModifier(*)  Type  sepBy1(VariableDeclarator, ',')  ';'  
        Expression(?)  ';'  sepBy1(Expression, ',')(?)
    
    ForControl2: 
        VariableModifier(*)  Type  IdentBraces  ':'  Expression
    
    ForControl3:
        sepBy1(Expression, ',')  ';'  Expression(?)  ';'  sepBy1(Expression, ',')(?)
    

    Expression: 
        Expression1  ( AssignmentOperator  Expression1 )(?)
    
    AssignmentOperator: 
        '='    |  '+='   |  '-='   |  '*='   |
        '/='   |  '&='   |  '|='   |  '^='   |
        '%='   |  '<<='  |  '>>='  |  '>>>='
    
    Expression1: 
        Expression2  ( '?'  Expression  ':'  Expression1 )(?)
    
    Expression2:
        Expression3  ( ( InfixOp Expression3 )(*)  |  
                       ( 'instanceof'  Type )      )(?)
    
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
        x =  Identifier  sepBy1(TypeArgument, ',')  |
             Identifier  '<'  '>'                   |
             Identifier
    
    ClassCreatorRest: 
        Arguments  ClassBody(?)
    
    ArrayCreatorRest:
        ( ( '['  Expression  ']' )(+)  Braces(*) )  |
        ( Braces(*)  ArrayInitializer )
    
    
    IdentifierSuffix:
        ( '['  Braces(*)  '.'  'class'  ']' )                       |
        ( '['  Expression  ']' )                                    |
        Arguments                                                   |
        ( '.'  'new'  NonWildcardTypeArguments(?)  InnerCreator )   |
        ( '.'  'class' )                                            |
        ( '.'  ExplicitGenericInvocation )                          |
        ( '.'  'this' )                                             |
        ( '.'  'super'  Arguments )
    
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
