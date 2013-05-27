{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts       
             , FlexibleInstances    
             , FunctionalDependencies
             , UndecidableInstances      #-}

module Combinaparse (

    Parser
  , runParser
  
  , many0
  , many1
  
  , item
  , check
  , satisfy
  , literal
  , Switch(..)
  , not1
  , end
  
  , optional
  , optionalM
  
  , MonadError(..)
  , commit
  
--  , Token
  , countLineCol
  , character
  
  , MonadState(..)
  , Applicative(..)
  , Alternative(..)

) where

import Control.Monad.State       (MonadState (..), StateT(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Applicative       (Applicative(..), Alternative(..))



type Parser e t a = StateT [t] (MaybeT (Either e)) a

runParser :: Parser e t a -> [t] -> Either e (Maybe (a, [t]))
runParser p xs = runMaybeT (runStateT p xs)

many0 = many
many1 = some

item :: (MonadState [t] m, Alternative m) => m t
item =
    get >>= \xs -> case xs of
                        (t:ts) -> put ts *> pure t;
                        []     -> empty;

check :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
check f p =
    p >>= \x ->
    if (f x) then return x else empty

satisfy :: (MonadState [t] m, Alternative m) => (t -> Bool) -> m t
satisfy = flip check item

literal :: (MonadState [t] m, Alternative m, Eq t) => t -> m t
literal t = satisfy (== t)


class Switch f where
  switch :: f a -> f ()

instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing  = Just ()

instance (Functor m, Switch m) => Switch (StateT s m) where
  switch (StateT f) = StateT (\s -> fmap (const ((), s)) . switch $ f s)

instance Functor m => Switch (MaybeT m) where
  switch (MaybeT m) = MaybeT (fmap switch m)

not1 :: (MonadState [t] m, Alternative m, Switch m) => m a -> m t
not1 p = switch p *> item

end :: (MonadState [t] m, Alternative m, Switch m) => m ()
end = switch item


optional :: Alternative f => a -> f a -> f a
optional x p = p <|> pure x

optionalM :: Alternative f => f a -> f (Maybe a)
optionalM p = fmap Just p <|> pure Nothing


class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance MonadError e (Either e) where
  throwError               =  Left
  catchError  (Right x) _  =  Right x
  catchError  (Left e)  f  =  f e
  
instance MonadError e m => MonadError e (StateT s m) where
  throwError      =  lift . throwError
  catchError m f  =  StateT g
    where
      g s = catchError (runStateT m s) 
                       (\e -> runStateT (f e) s)

instance MonadError e m => MonadError e (MaybeT m) where
  throwError      =  lift . throwError
  catchError m f  =  MaybeT $ catchError (runMaybeT m) (runMaybeT . f)

commit :: (MonadError e m, Alternative m) => e -> m a -> m a
commit err p = p <|> throwError err


type Token = (Char, Int, Int)
chr  (a, _, _)  =  a
line (_, b, _)  =  b
col  (_, _, c)  =  c

countLineCol :: [Char] -> [Token]
countLineCol = reverse . snd . foldl f ((1, 1), [])
  where
    f ((l, c), ts) '\n'   = ((l + 1, 1), ('\n', l, c):ts)
    f ((l, c), ts)  char  = ((l, c + 1), (char, l, c):ts)


character :: (MonadState [Token] m, Alternative m) => Char -> m Token
character c = satisfy ((==) c . chr)

