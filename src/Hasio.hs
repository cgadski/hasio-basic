module Hasio
    ( module Hasio.Application
    , testApplication
    ) where

import Hasio.Application

testApplication :: Application ()
testApplication =
    Application
        { initialApp = undefined
        , displayApp = return $ displayFromStrings ["hello world"]
        , incrementApp = return . Just }
