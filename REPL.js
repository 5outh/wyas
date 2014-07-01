import System.Console.Haskeline
import Evaluator

main :: IO ()
main = runInputT defaultSettings loop
  where 
      loop :: InputT IO ()
      loop = do
          ln <- getInputLine "> "
          case ln of
              Nothing     -> return ()
              Just ":q"   -> return ()
              Just input  -> outputStrLn (showEval ln) >>= loop