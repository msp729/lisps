module Parse.Monad (Error (..), ErrorContents (..), Parser (Parser), runParser, Offset (..)) where

import Control.Applicative (Alternative (..))
import Data.Bifunctor (first)
import Data.List (nub)

newtype Offset = Offset {unOffset :: Int} deriving (Show, Eq, Ord)

instance Enum Offset where
    toEnum = Offset . toEnum
    fromEnum = fromEnum . unOffset

instance Num Offset where
    Offset a + Offset b = Offset $ a + b
    Offset a * Offset b = Offset $ a * b
    abs = Offset . abs . unOffset
    signum = Offset . signum . unOffset
    fromInteger = Offset . fromInteger
    negate = Offset . negate . unOffset

data ErrorContents i e
    = EndOfInput -- Expected more input, but there is nothing
    | Unexpected i
    | Expected i i
    | ExpectedEof i
    | CustomError e -- Extra errors the user may want to create
    | Empty -- Used in `Alternative` implementation of `empty`
    deriving (Eq, Show, Ord)

data Error i e
    = Error {erOffset :: Offset, erContents :: ErrorContents i e} -- Expected more input, but there is nothing
    deriving (Eq, Show, Ord)

newtype Parser i e a = Parser
    { runParser :: [i] -> Offset -> Either [Error i e] (Offset, a, [i])
    }

instance Functor (Parser i e) where
    fmap f = Parser . fmap (fmap (fmap (first f))) . runParser

instance Applicative (Parser i e) where
    pure x = Parser $ \s o -> Right (o, x, s)
    pf <*> px = Parser $ \s o -> runParser pf s o >>= \(o', f, s') -> first f <$> runParser px s' o'

instance Monad (Parser i e) where
    px >>= fp = Parser $ \s o -> runParser px s o >>= \(o', x, s') -> runParser (fp x) s' o'

instance (Eq i, Eq e) => Alternative (Parser i e) where
    p1 <|> p2 = Parser $ \s o -> case runParser p1 s o of
        Left es -> case runParser p2 s o of
            Left es' -> Left $ nub $ es <> es'
            Right r -> Right r
        Right r -> Right r
    empty = Parser $ \_ o -> Left [Error o Empty]
