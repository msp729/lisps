module Interp (interpreter, runFiles, lexer, parser) where

import Control.Monad ((>=>))
import Control.Monad.Trans (lift)
import Core
import Data.List (unsnoc)
import Debug.Trace
import Eval
import Lex
import Parse
import Parse.Monad
import System.Console.Haskeline
import System.Directory (doesFileExist)
import System.Exit
import System.IO (IOMode (ReadMode), hGetContents, withFile)
import Prelude hiding (lex)

interpreter, interpreter' :: Env -> InputT IO Env
interpreter e =
    handleInterrupt (interpreter' e) $
        withInterrupt $
            getInputLine "> "
                >>= maybe quit (tag "l" (lift . lexer)) -- if no input line, quit
                >>= maybe (pure Nothing) (tag "p" (lift . parser))
                >>= maybe (pure Nothing) (tag "r" $ (lift . runner e)) -- recurse into more interpreter
                >>= maybe (pure mempty) return
interpreter' e =
    handleInterrupt quit $
        withInterrupt $
            getInputLine "> "
                >>= maybe quit (tag "l" (lift . lexer)) -- if no input line, quit
                >>= maybe (pure Nothing) (tag "p" (lift . parser))
                >>= maybe (pure Nothing) (tag "r" $ (lift . runner e)) -- recurse into more interpreter
                >>= maybe (pure mempty) return

tag :: String -> (a -> InputT IO b) -> a -> InputT IO b
tag s f x = (if False then (outputStrLn s >>) else id) $ f x

quit :: InputT IO a
quit = do
    outputStrLn "Goodbye!"
    lift exitSuccess

-- lexer splits up the input into lexemes for parsing
lexer :: String -> IO (Maybe [Token])
lexer s =
    either
        (\(errors) -> Nothing <$ putStrLn (show errors))
        (\(_, x, _) -> pure (pure x))
        $ runParser (lex :: Parser Char () [Token]) s 0

-- parser constructs S-expressions from lexemes
parser :: [Token] -> IO (Maybe [S])
parser ts =
    either
        (\(errors) -> putStrLn (unlines $ map show errors) >> return Nothing)
        (\(_, x, _) -> pure (pure x))
        $ runParser parse ts 0

-- runner takes a list of Exprs and evals them, printing their values
runner, runner' :: Env -> [S] -> IO (Maybe Env)
runner env exprs =
    (handleIOCalm (ev (mapM (eval) exprs) env) >>=) $ maybe (pure Nothing) $ \(env', vals) -> do
        maybe (pure ()) (putStrLn . show . snd) $ unsnoc vals
        return $ Just env'
runner' env exprs =
    (handleIOCalm (ev (mapM (eval) exprs) env) >>=) $ maybe (pure Nothing) $ return . Just . fst

runFile :: String -> Env -> IO Env
runFile fname e = do
    ex <- doesFileExist fname
    if ex
        then
            withFile fname ReadMode $
                hGetContents
                    >=> lexer
                    >=> runpr (maybe (pure Nothing) parser)
                    >=> maybe (pure Nothing) (runner' e)
                    >=> (return . maybe mempty id)
        else return mempty
  where
    runpr f x = do
        v <- f x
        mapM_ (mapM print) v
        return v

runFiles :: [String] -> Env -> IO Env
runFiles [] = pure
runFiles (x : xs) = (\e -> (e <>) <$> runFile x e) >=> runFiles xs
