-- | Definition of an application, constructable from pure semantics, which
-- holds the functionality we can expect from a casio BASIC program and can
-- be executed via an ncurses interface.
module Hasio.Application
    ( Key(..), nameFromKey
    , Display(..), displayFromStrings
    , Application(..), runApplication
    ) where

newtype Key = Key { codeFromKey :: Int }

nameFromKey :: Key -> Maybe String
nameFromKey key = Nothing

newtype Display = Display { stringsFromDisplay :: [String] }

displayFromStrings :: [String] -> Display
displayFromStrings strings =
    Display . take 7 $ fmap normalString strings ++ repeat (replicate 21 ' ')
    where normalString = take 21 . (++ repeat ' ')

data Event =
    TickEvent Float
    | KeyEvent Key

data Application s = Application
    { displayApp :: s -> Display
    , incrementApp :: s -> Event -> Maybe s }

runApplication :: Application s -> IO ()
runApplication = undefined
