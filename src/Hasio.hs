module Hasio
    ( module Hasio.Application
    , testApplication
    ) where

import Hasio.Application

data TestState = TestState { getKey :: Int }

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
