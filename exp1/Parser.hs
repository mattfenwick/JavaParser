{-# LANGUAGE NoMonomorphismRestriction #-}
module Parser (

    identifier
  , statement
  , modifier
  , typeParams
  , primitiveType
  , referenceType
  , type'
  , formalParams
  , method

) where

import Combinaparse
import Tokens
import AST


identifier :: Parser e Token String
identifier = 
    item >>= \x -> case x of (Identifier i) -> pure i;
                             _              -> empty;


statement :: Parser e Token Statement
statement = literal (Keyword Kreturn) *> fmap SReturn (optionalM identifier) <* literal (Separator Semicolon)


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
modifier = foldr (<|>) empty $ map (\(tk, val) -> literal (Keyword tk) *> pure val) legalModifiers

typeParams = 
    literal (Operator LessThan)                     *> 
    sepBy1 identifier (literal $ Separator Comma)  <* 
    literal (Operator GreaterThan)

primTypes = [(Kbyte   , Tbyte   ),
             (Kshort  , Tshort  ),
             (Kchar   , Tchar   ),
             (Kint    , Tint    ),
             (Klong   , Tlong   ),
             (Kfloat  , Tfloat  ),
             (Kdouble , Tdouble ),
             (Kboolean, Tboolean) ]

primitiveType :: Parser e Token BasicType
primitiveType = foldr (<|>) empty $ map (\(tk, val) -> literal (Keyword tk) *> pure val) primTypes

referenceType :: Parser e Token BasicType
referenceType = fmap RefType $ sepBy1 (fmap (,) identifier <*> optional [] typeParams) (literal $ Separator Period)

basicType :: Parser e Token BasicType
basicType = primitiveType <|> referenceType

type' :: Parser e Token Type
type' = fmap Type basicType <*> fmap length (many0 braces)
  where braces = literal (Separator OpenSquare) *> literal (Separator CloseSquare)
  
formalParameter :: Parser e Token FormalParameter
formalParameter =
    pure FormalParameter <*>
    many0 modifier       <*>
    type'                <*>
    identifier
    
formalParams :: Parser e Token [FormalParameter]
formalParams = 
    literal (Separator OpenParen)                        *> 
    sepBy0 formalParameter (literal $ Separator Comma)  <*
    literal (Separator CloseParen) 


method :: Parser e Token Method
method = 
    pure Method              <*>
    many0 modifier           <*>
    optional [] typeParams   <*>
    (fmap Just type' <|> (literal (Keyword Kvoid) *> pure Nothing))    <*> 
    identifier               <*>
    formalParams             <*>
    (literal (Separator OpenCurly) *> statement <* literal (Separator CloseCurly))
