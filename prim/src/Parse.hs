module Parse (parse) where

import Control.Applicative
import Core
import Lex (Token (..), unIdent)
import Parse.Comb
import Prelude hiding (takeWhile)

parse :: Parser Token String [S]
parse = many s_expr

s_expr, lp, lpse, atom :: Parser Token String S
s_expr = suppress (asum [atom, char LP >> lp, char QUOTE >> Q <$> s_expr]) <|> raise (CustomError "S-expression expected")
lp = suppress (asum [Atom "nil" <$ char RP, liftA2 Cons s_expr lpse]) <|> raise (CustomError "LP-expr expected")
lpse = suppress (asum [char DOT >> s_expr <* char RP, lp]) <|> raise (CustomError "LPSE-expr expected")
atom = Atom <$> Name <$> grab unIdent

-- <s-expr> ::= <atom> | "(" <s-expr> "." <s-expr> ")" | "(" <s-expr>* ")" | "'" <s-expr>
-- <s-expr> ::= <atom> | "(" <lp> | "'" <s-expr>
-- <lp> ::= ")" | <s-expr> <lpse>
-- <lpse> ::= "." <s-expr> ")" | <s-expr> <lpse>
