module Main (main) where

import Hasio

import Control.Applicative
import Data.Char
import System.Environment

data TestState = TestState { getKey :: Int }

-- testApplication
initState :: TestState
initState = TestState 1

displayState :: TestState -> AppDisplay
displayState = displayFromStrings . (:[]) . show . getKey

incrementState :: AppEvent -> TestState -> Maybe TestState
incrementState event state@(TestState key) =
    case event of
        KeyEvent key -> Just $ TestState (casioCodeFromKey key)
        _ -> Just state

testApplication :: Application TestState
testApplication =
    Application
        { initialApp = initState
        , displayApp = displayState
        , incrementApp = incrementState }

data InvocationType =
    HelpInvoke
    | TestInvoke
    | ErrorInvoke

invocationFromArgs :: [String] -> InvocationType
invocationFromArgs [] = ErrorInvoke
invocationFromArgs (firstArg:arguments) =
    case map toLower firstArg of
        "help" -> HelpInvoke
        "test" -> TestInvoke
        _ -> ErrorInvoke

printHelp, printError :: IO ()
printHelp = putStr . unlines $
    ["first help line"
    ,"second help line"]
printError = putStr . unlines $
    ["first error line"
    ,"second error line"]

main :: IO ()
main = do
    invocation <- invocationFromArgs <$> getArgs
    case invocation of
        HelpInvoke -> printHelp
        TestInvoke -> runApplication testApplication
        ErrorInvoke -> printError
