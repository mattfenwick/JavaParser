
    Method:
        Modifier(*)  ( Type  |  'void' )  Identifier  TypeParameters(?)  FormalParameters  '{'  Statement(+)  '}'
        
    Modifier:
        
    
    Type:
        BasicType  ( '['  ']' )(*)
    
    BasicType:
        PrimitiveType
        sepBy1(Identifier  TypeParameters(?), '.')
    
    TypeParameters:
        '<'  sepBy1(Type, ',')  '>'
    
    FormalParameters:
        '('  sepBy0(Modifier(*)  Type  Identifier, ',')  ')'
    
    Statement:
        For
        While
        If
        Return
        Block
    
    For:
    
    
    While:
        'while'  '('  Expression  ')'  Statement
    
    If:
    
    
    Return: