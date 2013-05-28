
    Method:
        Modifier(*)  TypeParameters(?)  ( Type  |  'void' )  Identifier  FormalParameters  '{'  Statement  '}'
        
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
        '<'  sepBy1(Identifier, ',')  '>'
    
    FormalParameters:
        '('  sepBy0(Modifier(*)  Type  Identifier, ',')  ')'
    
    Statement:
        'return'  Identifier(?)  ';'
    