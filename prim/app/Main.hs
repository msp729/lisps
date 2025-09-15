module Main where

import Control.Monad.Trans (lift)
import Interp (interpreter, lexer, parser, runFiles)
import System.Console.Haskeline (defaultSettings, outputStrLn, runInputT)
import System.Environment (getArgs)

loopAroundDelta :: (Monad m, Semigroup a) => (a -> m a) -> a -> m a
loopAroundDelta f x = f x >>= loopAroundDelta f . (x <>)

main :: IO ()
main = do
    -- let src =
    -- "(set big '(lambda (a b). (natpat b '(lambda a. (natpat a '(lambda b. (wholepat '(lambda a/2. wholepat '(lambda b/2. (cons (+ a/2 b/2) 0)) '(lambda b-1/2. (cons (+ a/2 b-1/2) 1)) b) '(lambda a-1/2. wholepat '(lambda b/2. (cons (+ a-1/2 b/2) 1)) '(lambda b-1/2. (cons (inc (+ a-1/2 b-1/2)) 0)) b) a)) b)) a)))"
    -- tok <- lexer src
    -- maybe (return ()) print tok
    -- ast <- maybe (pure Nothing) parser tok
    -- maybe (return ()) (mapM_ print) ast
    runInputT defaultSettings $
        lift getArgs
            >>= lift . flip runFiles mempty
            >>= loopAroundDelta interpreter
            >> return ()
