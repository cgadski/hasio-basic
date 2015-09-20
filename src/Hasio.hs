module Hasio
    (
    ) where

import System.Environment
import Data.Char
import Control.Applicative

data InvocationType =
  HelpInvoke
  | ErrorInvoke

invocationFromArgs :: [String] -> InvocationType
invocationFromArgs (firstArg:arguments) =
  case map toLower firstArg of
    "help" -> HelpInvoke
    _ -> ErrorInvoke

printHelp, printError :: IO ()
printHelp = putStrLn
  "first help line\
  \second error line"
printError = putStrLn
  "first line\
  \second line"

main :: IO ()
main = do
  invocation <- invocationFromArgs <$> getArgs
  case invocation of
    HelpInvoke -> printHelp
    ErrorInvoke -> printError
