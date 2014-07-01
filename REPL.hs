import System.Console.Haskeline
import Types
import Evaluator
import Control.Monad.Trans

main :: IO ()
main = do
  putStrLn "Welcome to the scheme REPL!"
  env <- primitiveBindings
  runInputT defaultSettings (loop env)
  where loop :: Env -> InputT IO ()
        loop env = do
            ln <- getInputLine "> "
            case ln of
                Nothing     -> return ()
                Just "quit" -> outputStrLn "Goodbye!" >> return ()
                Just input  -> do
                  str <- lift $ evalString env input
                  outputStrLn str
                  loop env