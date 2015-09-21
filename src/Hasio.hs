module Hasio
    ( module Hasio.Application
    , testApplication
    ) where

import Hasio.Application

data TestState = TestState { getCounter :: Int }

initState :: TestState
initState = TestState 1

displayState :: TestState -> AppDisplay
displayState = displayFromStrings . (:[]) . show . getCounter

incrementState :: AppEvent -> TestState -> Maybe TestState
incrementState event (TestState counter) =
    Just $ TestState $ counter + 1

testApplication :: Application TestState
testApplication =
    Application
        { initialApp = initState
        , displayApp = displayState
        , incrementApp = incrementState }
