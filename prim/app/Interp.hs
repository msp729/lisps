module Interp (interpreter) where

import Control.Monad.Trans (lift)
import Core
import Data.List (unsnoc)
import Eval
import Lex
import Parse
import Parse.Monad
import System.Console.Haskeline
import System.Exit
import Prelude hiding (lex)

interpreter, interpreter' :: Env -> InputT IO Env
interpreter e =
    handleInterrupt (interpreter' e) $
        withInterrupt $
            getInputLine "> "
                >>= maybe quit (tag "l" lexer) -- if no input line, quit
                >>= maybe (pure Nothing) (tag "p" parser)
                >>= maybe (pure Nothing) (tag "r" $ runner e) -- recurse into more interpreter
                >>= maybe (pure mempty) return
interpreter' e =
    handleInterrupt quit $
        withInterrupt $
            getInputLine "> "
                >>= maybe quit (tag "l" lexer) -- if no input line, quit
                >>= maybe (pure Nothing) (tag "p" parser)
                >>= maybe (pure Nothing) (tag "r" $ runner e) -- recurse into more interpreter
                >>= maybe (pure mempty) return

tag :: String -> (a -> InputT IO b) -> a -> InputT IO b
tag s f x = (if False then (outputStrLn s >>) else id) $ f x

quit :: InputT IO a
quit = do
    outputStrLn "Goodbye!"
    lift exitSuccess

-- lexer splits up the input into lexemes for parsing
lexer :: String -> InputT IO (Maybe [Token])
lexer s =
    either
        (\(errors) -> Nothing <$ outputStrLn (show errors))
        (\(_, x, _) -> pure (pure x))
        $ runParser (lex :: Parser Char () [Token]) s 0

-- parser constructs S-expressions from lexemes
parser :: [Token] -> InputT IO (Maybe [S])
parser ts =
    either (\(errors) -> outputStrLn (show errors) >> return Nothing) (\(_, x, _) -> pure (pure x)) $
        runParser (parse :: Parser Token () [S]) ts 0

-- runner takes a list of Exprs and evals them, printing their values
runner :: Env -> [S] -> InputT IO (Maybe Env)
runner env exprs =
    (lift (handleIOCalm $ ev (mapM (eval) exprs) env) >>=) $ maybe (pure Nothing) $ \(env', vals) -> do
        maybe (pure ()) (outputStrLn . show . snd) $ unsnoc vals
        return $ Just env'
