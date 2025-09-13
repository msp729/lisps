module Parse (parse) where

import Control.Applicative
import Core
import Lex (Token (..), unIdent)
import Parse.Comb
import Prelude hiding (takeWhile)

parse :: (Eq e) => Parser Token e [S]
parse = many s_expr

s_expr, lp, lpse, atom :: (Eq e) => Parser Token e S
s_expr = asum [atom, (char LP >> lp), (char QUOTE >> Q <$> s_expr)]
lp = (Atom "nil" <$ char RP) <|> liftA2 Cons s_expr lpse
lpse = asum [char DOT >> s_expr <* char RP, lp]
atom = Atom <$> Name <$> grab unIdent

-- <s-expr> ::= <atom> | "(" <s-expr> "." <s-expr> ")" | "(" <s-expr>* ")" | "'" <s-expr>
-- <s-expr> ::= <atom> | "(" <lp> | "'" <s-expr>
-- <lp> ::= ")" | <s-expr> <lpse>
-- <lpse> ::= "." <s-expr> ")" | <s-expr> <lpse>
