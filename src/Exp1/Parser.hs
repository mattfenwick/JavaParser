{-# LANGUAGE NoMonomorphismRestriction #-}
module Exp1.Parser (

    identifier
  , statement
  , modifier
  , typeParams
  , typeArgs
  , primitiveType
  , referenceType
  , type'
  , formalParams
  , method

) where

import Combinaparse
import Exp1.Tokens
import Exp1.AST


op :: Operator -> Parser e Token Token
op = literal . Operator

sep :: Separator -> Parser e Token Token
sep = literal . Separator

key :: Keyword -> Parser e Token Token
key = literal . Keyword


identifier :: Parser e Token String
identifier = 
    item >>= \x -> case x of (Identifier i) -> pure i;
                             _              -> empty;


statement :: Parser e Token Statement
statement = key Kreturn *> fmap SReturn (optionalM identifier) <* sep Semicolon


legalModifiers = [(Kstrictfp    ,  Mstrictfp    ),
                  (Kpublic      ,  Mpublic      ),
                  (Kprotected   ,  Mprotected   ),
                  (Kprivate     ,  Mprivate     ),
                  (Kstatic      ,  Mstatic      ),
                  (Kabstract    ,  Mabstract    ),
                  (Kfinal       ,  Mfinal       ),
                  (Knative      ,  Mnative      ),
                  (Ksynchronized,  Msynchronized),
                  (Ktransient   ,  Mtransient   ),
                  (Kvolatile    ,  Mvolatile    ) ]

modifier :: Parser e Token Modifier
modifier = foldr (<|>) empty $ map (\(tk, val) -> key tk *> pure val) legalModifiers

typeParams = 
    op LessThan                     *> 
    sepBy1 identifier (sep Comma)  <* 
    op GreaterThan

reinterprets :: [(Operator, Operator)]
reinterprets = [(TripleGreaterThanEquals, DoubleGreaterThanEquals),
                (DoubleGreaterThanEquals, GreaterThanOrEquals    ),
                (GreaterThanOrEquals    , Equals                 ),
                (TripleGreaterThan      , DoubleGreaterThan      ),
                (DoubleGreaterThan      , GreaterThan            ) ]
-- could build 'reinterprets' into a map,
-- then have a parser that checks for membership in the map, tries >, else fails

-- >>>=, >>=, >=, >>>, >>
reinterpretOperator = 
    item >>= f
  where
    f (Operator TripleGreaterThanEquals) = get >>= \ts -> put (Operator DoubleGreaterThanEquals:ts)
    f (Operator DoubleGreaterThanEquals) = get >>= \ts -> put (Operator GreaterThanOrEquals:ts)
    f (Operator GreaterThanOrEquals)     = get >>= \ts -> put (Operator Equals:ts)
    f (Operator TripleGreaterThan)       = get >>= \ts -> put (Operator DoubleGreaterThan:ts)
    f (Operator DoubleGreaterThan)       = get >>= \ts -> put (Operator GreaterThan:ts)
    f (Operator GreaterThan)             = pure ()
    f               _                    = empty

typeArgs = 
    op LessThan                        *>
    sepBy1 type' (sep Comma)          <*
    reinterpretOperator -- in case the next token is an operator beginning with '>'

primTypes = [(Kbyte   , Tbyte   ),
             (Kshort  , Tshort  ),
             (Kchar   , Tchar   ),
             (Kint    , Tint    ),
             (Klong   , Tlong   ),
             (Kfloat  , Tfloat  ),
             (Kdouble , Tdouble ),
             (Kboolean, Tboolean) ]

primitiveType :: Parser e Token BasicType
primitiveType = foldr (<|>) empty $ map (\(tk, val) -> key tk *> pure val) primTypes

referenceType :: Parser e Token BasicType
referenceType = fmap RefType $ sepBy1 (fmap (,) identifier <*> optional [] typeArgs) (sep Period)

basicType :: Parser e Token BasicType
basicType = primitiveType <|> referenceType

type' :: Parser e Token Type
type' = fmap Type basicType <*> fmap length (many0 braces)
  where braces = sep OpenSquare *> sep CloseSquare
  
formalParameter :: Parser e Token FormalParameter
formalParameter =
    pure FormalParameter <*>
    many0 modifier       <*>
    type'                <*>
    identifier
    
formalParams :: Parser e Token [FormalParameter]
formalParams = 
    sep OpenParen                        *> 
    sepBy0 formalParameter (sep Comma)  <*
    sep CloseParen 


method :: Parser e Token Method
method = 
    pure Method              <*>
    many0 modifier           <*>
    optional [] typeParams   <*>
    (fmap Just type' <|> (key Kvoid *> pure Nothing))    <*> 
    identifier               <*>
    formalParams             <*>
    (sep OpenCurly *> statement <* sep CloseCurly)
