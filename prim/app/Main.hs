module Main where

import Interp (interpreter)
import System.Console.Haskeline (defaultSettings, runInputT)

loopAroundDelta :: (Monad m, Semigroup a) => (a -> m a) -> a -> m a
loopAroundDelta f x = f x >>= loopAroundDelta f . (x <>)

main :: IO ()
main = runInputT defaultSettings $ loopAroundDelta interpreter mempty >> return ()
