module Lex (Token (..), lex, unIdent) where

import qualified Data.Char as C
import qualified Data.Text as T

import Control.Applicative
import Parse.Comb
import Prelude hiding (lex, takeWhile)

data Token = LP | RP | DOT | QUOTE | Ident T.Text deriving (Show, Eq)

isIdent :: Char -> Bool
isIdent x = C.isPrint x && not (C.isSpace x) && not (elem x ("().'" :: String))

lp, rp, dot, quote, ident :: (Eq e) => Parser Char e Token
lp = LP <$ char '('
rp = RP <$ char ')'
dot = DOT <$ char '.'
quote = QUOTE <$ char '\''
ident = Ident <$> T.pack <$> takeWhile1 isIdent

ws :: (Eq e) => Parser Char e ()
ws = () <$ (many $ match C.isSpace)

token :: (Eq e) => Parser Char e Token
token = ws >> asum [lp, rp, dot, quote, ident]

lex :: (Eq e) => Parser Char e [Token]
lex = liftA2 (:) token lex <|> pure []

unIdent :: Token -> Maybe T.Text
unIdent (Ident x) = Just x
unIdent _ = Nothing
