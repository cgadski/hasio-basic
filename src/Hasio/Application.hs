-- | Definition of an application, constructable from pure semantics, which
-- holds the functionality we can expect from a casio BASIC program and can
-- be executed via an ncurses interface.
module Hasio.Application
    ( Key(..), nameFromKey
    , Display(..), displayFromStrings
    , Application(..), runApplication
    ) where

import UI.NCurses
import Control.Monad

newtype AppKey = Key { codeFromKey :: Int }

nameFromKey :: AppKey -> Maybe String
nameFromKey key = Nothing

newtype Display = Display { stringsFromDisplay :: [String] }

displayFromStrings :: [String] -> Display
displayFromStrings strings =
    Display . take 7 $ fmap normalString strings ++ repeat (replicate 21 ' ')
    where normalString = take 21 . (++ repeat ' ')

data AppEvent =
    TickEvent Float
    | KeyEvent Key

data Application s = Application
    { displayApp :: s -> Display
    , incrementApp :: s -> AppEvent -> Maybe s }

runApplication :: Application s -> IO ()
runApplication app = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        moveCursor 1 10
        drawString "Hello world!"
        moveCursor 3 10
        drawString "(press q to quit)"
        moveCursor 0 0
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> unless (p ev') loop
