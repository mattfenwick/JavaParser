
    Method:
        Modifier(*)  ( Type  |  'void' )  Identifier  TypeParameters(?)  FormalParameters  '{'  Statement  '}'
        
    Modifier:
        'strictfp'     |    'public'      |    'protected'     |
        'private'      |    'static'      |    'abstract'      |
        'final'        |    'native'      |    'synchronized'  |
        'transient'    |    'volatile'
    
    Type:
        BasicType  ( '['  ']' )(*)
    
    BasicType:
        'byte'   |   'short'   |   'char'     |   'int'      |
        'long'   |   'float'   |   'double'   |   'boolean'  |
        sepBy1(Identifier  TypeParameters(?), '.')
    
    TypeParameters:
        '<'  sepBy1(Type, ',')  '>'
    
    FormalParameters:
        '('  sepBy0(Modifier(*)  Type  Identifier, ',')  ')'
    
    Statement:
        'return'  Identifier(?)  ';'
    