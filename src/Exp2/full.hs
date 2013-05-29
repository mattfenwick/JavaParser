import Exp1.Tokenizer
import Exp2.Parser
import Exp2.AST
import Exp1.Tokens
import Combinaparse


data ParseResult a
    = Success a
    | TokenError String
    | TokenFailure
    | ParseError String
    | ParseFailure
  deriving (Show, Eq)


full :: Parser String Token a -> String -> ParseResult a
full p inp = 
    case (runParser input inp) of
      Left e                -> TokenError e;
      Right Nothing         -> TokenFailure;
      Right (Just (xs, "")) -> case (runParser p (ditchJunk xs)) of  -- TokenError ("actually it worked: " ++ show xs) -- 
                                 Left e2     -> ParseError e2;
                                 Right Nothing -> ParseFailure;
                                 Right (Just (x, [])) -> Success x;
                                 Right (Just (_, ys)) -> ParseError ("unconsumed input: " ++ show ys);
      Right (Just (_, ts))  -> TokenError ("unconsumed input: " ++ ts);

-- parser = full

ditchJunk :: [InputElement] -> [Token]
ditchJunk [] = []
ditchJunk (Token t : xs) = t : ditchJunk xs
ditchJunk (_:xs) = ditchJunk xs