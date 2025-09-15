{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval (eval) where

import Control.Monad ((<=<))
import Core
import Data.Bool (bool)
import qualified Data.Char as C
import qualified Data.Text as T

isFix :: Name -> Bool
isFix (Name s) = s == "nil" || s == "t" || T.all C.isNumber s

eval :: S -> EV S
eval (Atom n) | isFix n = pure $ Atom n
eval (Atom s) = EV $ \env -> fmap pure $ env ?? s
eval (Q s) = pure s
eval (Cons "cond" conds) = evconds conds
eval (Cons f args) = apply f =<< lis args

apply :: S -> [S] -> EV S
apply "CAR" = grab1 "CAR" $ \case
    Cons x _ -> pure x
    x -> burn $ psh x <> " is not a cons-cell"
apply "CDR" = grab1 "CDR" $ \case
    Cons _ x -> pure x
    x -> burn $ psh x <> " is not a cons-cell"
apply "CONS" = \case
    (x : y : _) -> liftA2 Cons (eval x) (eval y)
    _ -> burn "not enough arguments given to CONS"
apply "ATOM" = grab1 "ATOM" $ \case
    (Atom _) -> pure "t"
    _ -> pure "nil"
apply "EQ" = \case
    (x : y : _) -> (bool "nil" "t") <$> liftA2 (==) (eval x) (eval y)
    _ -> burn "not enough arguments given to EQ"
apply "SET" = \case
    (Atom n : y : _) -> enter n =<< eval y
    (x : _ : _) -> burn $ psh x <> " is not an atom"
    _ -> burn "not enough arguments to SET"
apply x@(Atom n)
    | not (isFix n) = \args -> eval x >>= (`apply` args)
    | otherwise = pure $ burn $ psh x <> " is not a valid function name"
apply (Cons "lambda" (Cons "nil" lbody)) = (`locals` eval lbody) <=< pairlis "nil"
apply (Cons "lambda" (Cons arg@(Atom _) lbody)) = apply (Cons "lambda" (Cons (Cons arg "nil") lbody))
apply (Cons "lambda" (Cons argl lbody)) = (`locals` eval lbody) <=< pairlis argl
apply fn@(Q _) = const $ burn $ psh fn <> " is not a function"
apply fn = (eval fn >>=) . flip apply

pairlis :: S -> [S] -> EV [(Name, S)]
pairlis names values = do
    n' <- lis names
    n'' <- mapM (\case Atom x -> return x; y -> burn $ psh y <> " is not an atom") n'
    zip' n'' =<< mapM eval values

zip' :: [a] -> [b] -> EV [(a, b)]
zip' [] _ = pure []
zip' _ [] = burn "not enough arguments provided"
zip' (x : xs) (y : ys) = fmap ((:) (x, y)) (zip' xs ys)

grab1 :: T.Text -> (S -> EV S) -> [S] -> EV S
grab1 fn _ [] = burn $ "no argument given to " <> fn
grab1 _ k (x : _) = k =<< eval x

lis :: S -> EV [S]
lis "nil" = pure []
lis (Cons a b) = (a :) <$> lis b
lis x = burn $ psh x <> " is not a list."

evconds :: S -> EV S
evconds (Cons (Cons p e) rest) = do
    v <- eval p
    if truthy v then eval e else evconds rest
evconds x = burn $ "invalid argument to COND: " <> psh x
