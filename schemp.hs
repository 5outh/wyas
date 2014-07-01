import REPL
import Evaluator
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    _  -> runOne args