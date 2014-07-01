import System.Console.Haskeline
import Evaluator

main :: IO ()
main = do
  putStrLn "Welcome to the scheme REPL!"
  runInputT defaultSettings loop
  where loop :: InputT IO ()
        loop = do
            ln <- getInputLine "> "
            case ln of
                Nothing     -> return ()
                Just "quit" -> outputStrLn "Goodbye!" >> return ()
                Just input  -> (outputStrLn $ showEval input) >> loop