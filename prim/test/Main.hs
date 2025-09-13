module Main (main) where

import Lex (Token, lex)
import Parse.Comb (Parser (runParser))
import Test.QuickCheck (Gen, Property, elements, forAll, listOf, oneof, quickCheck, verboseCheck, (==>))
import Prelude hiding (lex)

main :: IO ()
main = do
    quickCheck prop_lex_append

sourceChar :: Gen Char
sourceChar = elements "abc123() .'"
source :: Gen String
source = listOf sourceChar

prop_lex_append :: Property
prop_lex_append =
    forAll source $ \a ->
        forAll source $ \b ->
            let
                a' = runParser lex' a 0
                b' = case a' of
                    Left es -> Left es
                    Right (o, _, _) -> runParser lex' b o
                combined = liftA2 (\(_, x, _) (o, y, _) -> x <> y) a' b'

                comb' = (\(_, b, _) -> b) <$> runParser lex' (a <> " " <> b) 0
             in
                combined == comb'
  where
    lex' :: Parser Char () [Token]
    lex' = lex
