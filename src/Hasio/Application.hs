-- | Definition of an application, constructable from pure semantics, which
-- holds the functionality we can expect from a casio BASIC program and can
-- be executed via an ncurses interface.
module Hasio.Application
    ( AppKey(..), nameFromKey
    , AppDisplay(..), displayFromStrings
    , Application(..), runApplication
    ) where

import UI.NCurses
import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class

newtype AppKey = AppKey { codeFromKey :: Int }

nameFromKey :: AppKey -> Maybe String
nameFromKey key = Nothing

newtype AppDisplay = AppDisplay { stringsFromAppDisplay :: [String] }

displayFromStrings :: [String] -> AppDisplay
displayFromStrings strings =
    AppDisplay . take 7 $ fmap normalString strings ++ repeat (replicate 21 ' ')
    where normalString = take 21 . (++ repeat ' ')

data AppEvent =
    TickEvent Float
    | KeyEvent AppKey

data Application s = Application
    { initialApp :: s
    , displayApp :: s -> AppDisplay
    , incrementApp :: AppEvent -> s -> Maybe s }

updateFromDisplay :: AppDisplay -> Update ()
updateFromDisplay (AppDisplay strings) =
    mapM_ (\i ->
        moveCursor 1 i >> drawString (strings !! fromIntegral i))
        [0..6]

runApplication :: Application s -> IO ()
runApplication app = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ updateFromDisplay (displayApp app $ initialApp app)
    render
    handleEvents w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

appLoop :: Window -> Application s -> s -> Curses ()
appLoop window app state = do
    updateWindow window . updateFromDisplay $ displayApp app state
    event <- getEvent window Nothing
    let
        mutateEvent = case event of
            Nothing -> return
            Just event ->
                case event of
                    EventCharacter char ->
                        incrementApp app (KeyEvent (AppKey 1))
                    _ -> return
        mutateDelay = incrementApp app (TickEvent 1)
    liftIO $ threadDelay 15
    forM_ (mutateEvent =<< mutateDelay state) $ appLoop window app

handleEvents :: Window -> (Event -> Bool) -> Curses ()
handleEvents w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> unless (p ev') loop
