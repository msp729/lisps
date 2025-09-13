{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Core (locals, psh, burn, S (..), Name (..), EV (..), handleIO, Eff (..), Env (..), (??), enter, truthy, handleIOCalm) where

import Control.Monad (liftM2)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString (fromString))
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype Name = Name {name :: T.Text}

instance Show Name where
    show = T.unpack . T.toUpper . name

instance IsString Name where
    fromString = Name . fromString

instance Eq Name where
    Name a == Name b = T.toCaseFold a == T.toCaseFold b

instance Ord Name where
    compare (Name a) (Name b) = compare (T.toCaseFold a) (T.toCaseFold b)

data S = Atom Name | Cons S S | Q S deriving (Eq)

instance IsString S where
    fromString = Atom . fromString

newtype Env = Env (Map Name S) deriving (Eq, Show)

newtype EV a = EV {ev :: Env -> Eff (Env, a)}

-- | algebraic effects, kinda
data Eff a = Return a | Out T.Text (Eff a) | Err T.Text deriving (Functor)

handleIO :: Eff a -> IO a
handleIO (Return x) = return x
handleIO (Out s k) = T.putStrLn s >> handleIO k
handleIO (Err s) = error $ T.unpack s

handleIOCalm :: Eff a -> IO (Maybe a)
handleIOCalm (Return x) = return $ Just x
handleIOCalm (Out s k) = T.putStrLn s >> handleIOCalm k
handleIOCalm (Err s) = Nothing <$ T.putStrLn s

instance Applicative Eff where
    Return f <*> x = fmap f x
    Out s k <*> x = Out s (k <*> x)
    Err s <*> _ = Err s
    pure = Return

instance Monad Eff where
    Return x >>= f = f x
    Out s k >>= f = Out s (k >>= f)
    Err s >>= _ = Err s

showS :: S -> String -> String
showS (Atom x) = (T.unpack (T.toUpper $ name x) <>)
showS (Q s) = ('\'' :) . showS s
showS (Cons a b) = ('(' :) . showS a . show' b

show' :: S -> String -> String
show' (Atom "nil") = (')' :)
show' (Cons a b) = (' ' :) . showS a . show' b
show' x = (" . " ++) . showS x . (')' :)

instance Show S where
    show x = showS x ""

instance Functor EV where
    -- the functor instance could probably be derived, it's just functor composition
    -- but applicative and monad are very different
    fmap f x = EV $ \env -> fmap f <$> ev x env

instance Applicative EV where
    -- innermost wraps it in Eff (Monad Eff), w/ no effects
    -- next is Monoid a => Monad (a, ) for x -> (mempty, x), the empty map
    -- then const from Monad (a ->)
    pure = EV . pure . pure . pure
    liftA2 = liftM2

instance Monad EV where
    x >>= f = EV $ \env -> do
        -- evaluate x with the given environment
        (e', rv) <- ev x env
        -- apply any changes necessary to the environment before evaluating the new EV
        let env' = env <> e'
        (e'', rv') <- ev (f rv) env'
        -- return only the changes to the environment
        return (e' <> e'', rv')

instance Semigroup Env where
    -- has to be backwards, because Map.union gives the first argument precedence
    -- I want (SET X 1) (SET X 2) to end with 2 for X
    -- I also want it to be oldEnv <> updater
    Env a <> Env b = Env (Map.union b a)

instance Monoid Env where
    mempty = Env Map.empty
    mappend = (<>)

(??) :: Env -> Name -> Eff S
Env env ?? n = maybe notFound pure $ Map.lookup n env
  where
    notFound = Err ("Variable " <> name n <> " is undefined")

enter :: Name -> S -> EV S
enter n s = EV $ pure $ pure (enterEnv n s, s)

enterEnv :: Name -> S -> Env
enterEnv n s = Env $ Map.fromList [(n, s)]

burn :: T.Text -> EV a
burn = EV . const . Err

psh :: (Show a) => a -> T.Text
psh = T.pack . show

locals :: [(Name, S)] -> EV a -> EV a
locals bs val = EV $ \env -> ev val $ env <> Env (Map.fromList bs)

truthy :: S -> Bool
truthy "nil" = False
truthy _ = True
