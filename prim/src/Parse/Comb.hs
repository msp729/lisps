module Parse.Comb (
    Error (..),
    ErrorContents (..),
    Parser (Parser),
    Offset (..),
    runParser,
    match,
    char,
    string,
    eof,
    mapError,
    option,
    optional,
    takeWhile,
    takeWhile1,
    notEof,
    unwrap,
    grab,
) where

import Control.Applicative (Alternative (..))
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Parse.Monad
import Prelude hiding (takeWhile)

match :: (i -> Bool) -> Parser i e i
match p = Parser $ \cases
    [] o -> Left [Error o EndOfInput]
    (x : xs) o -> if p x then Right (succ o, x, xs) else Left [Error o $ Unexpected x]

grab :: (i -> Maybe a) -> Parser i e a
grab f = Parser $ \cases
    [] o -> Left [Error o EndOfInput]
    (x : xs) o -> maybe (Left [Error o (Unexpected x)]) (\a -> Right (o, a, xs)) (f x)

mapError :: ([Error i e] -> [Error i e]) -> Parser i e a -> Parser i e a
mapError f = Parser . fmap (fmap (first f)) . runParser

char :: (Eq i) => i -> Parser i e i
char c = Parser $ \cases
    [] o -> Left [Error o EndOfInput]
    (x : xs) o -> if c == x then Right (o, x, xs) else Left [Error o $ Expected c x]

string :: (Eq i) => [i] -> Parser i e [i]
string = traverse char

eof :: Parser i e ()
eof = Parser $ \cases
    [] o -> Right (o, (), [])
    (x : _) o -> Left [Error o $ ExpectedEof x]

optional :: (Alternative f) => f a -> f (Maybe a)
optional x = Just <$> x <|> pure Nothing

option :: (Alternative f) => a -> f a -> f a
option fallback parser = fromMaybe fallback <$> optional parser

notEof :: Parser i e ()
notEof = Parser $ \cases
    [] o -> Left [Error o EndOfInput]
    xs o -> Right (o, (), xs)

takeWhile, takeWhile1 :: (Eq i, Eq e) => (i -> Bool) -> Parser i e [i]
takeWhile p = takeWhile1 p <|> pure []
takeWhile1 p = (:) <$> match p <*> takeWhile p

unwrap :: (Eq i, Eq e) => Parser i e (Maybe a) -> Parser i e a
unwrap = (>>= maybe empty pure)
