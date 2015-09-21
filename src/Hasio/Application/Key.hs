module Hasio.Application.Key
    ( AppKey
    -- * Constructors
    , keyFromName, keyFromEvent, keyFromCasioCode
    -- * Accessors
    , nameFromKey, eventPredFromKey, casioCodeFromKey
    -- , nameFromKey, casioCodeFromKey
    ) where

import Data.List
import Control.Applicative
import UI.NCurses

-- | A key that was pressed. Could have been pressed on a calculator or on
-- a normal keyboard. Internally represented as the casio key code.
newtype AppKey = AppKey Int deriving (Eq)

-- | Internal representation of what we know about a certain kind of key.
data KeyData =
    KeyData
        { nameFromKeyData :: String -- ^ name of the key, alphanumeric
        , casioCodeFromKeyData :: Int -- ^ the key code on a calculator
        , ncursesPredFromKeyData :: Event -> Bool
            -- ^ does an ncurses event trigger the key?
        }

keyData :: [KeyData]
keyData =
    map fromStr
        -- in the form of <name> <casio code> <ncurses event>
        [ "0 71 char 0", "exe 31 enter"
        , "1 72 char 1" , "2 62 char 2" , "3 52 char 3"
        , "4 73 char 4" , "5 63 char 5" , "6 53 char 6"
        , "7 74 char 7" , "8 64 char 8" , "9 54 char 9"
        , "left 38 left" , "right 27 right" , "up 28 up" , "down 37 down" ]
    where
        eventIsChar :: Char -> Event -> Bool
        eventIsChar char (EventCharacter echar) = char == echar
        eventIsChar _ _ = False

        eventIsSpecial :: String -> Event -> Bool
        eventIsSpecial name (EventSpecialKey key) =
            case key of
                KeyUpArrow -> name == "up"
                KeyDownArrow -> name == "down"
                KeyLeftArrow -> name == "left"
                KeyRightArrow -> name == "right"
                KeyEnter -> name == "enter"
                _ -> False
        eventIsSpecial _ _ = False

        fromStr str =
            -- it's okay to use unsafe methods here because this references
            -- internal static data
            KeyData
                { nameFromKeyData = head $ words str
                , casioCodeFromKeyData = read $ words str !! 1
                , ncursesPredFromKeyData =
                    case words str !! 2 of
                        "char" -> eventIsChar (head $ words str !! 3)
                        _ -> eventIsSpecial (words str !! 2)
                }

-- internal, assumes AppKey was constructed safely
keyDataFromKey :: AppKey -> KeyData
keyDataFromKey (AppKey code) = head $ filter ((== code) . casioCodeFromKeyData) keyData

-- * Constructors

keyFromKeyData = AppKey . casioCodeFromKeyData

keyFromName :: String -> Maybe AppKey
keyFromName name = keyFromKeyData <$>
    find ((== name) . nameFromKeyData) keyData

keyFromEvent :: Event -> Maybe AppKey
keyFromEvent event = keyFromKeyData <$>
    find (($ event) . ncursesPredFromKeyData) keyData

keyFromCasioCode :: Int -> Maybe AppKey
keyFromCasioCode code = keyFromKeyData <$>
    find ((== code) . casioCodeFromKeyData) keyData

-- * Accessors

nameFromKey :: AppKey -> String
nameFromKey = nameFromKeyData . keyDataFromKey

eventPredFromKey :: AppKey -> Event -> Bool
eventPredFromKey = ncursesPredFromKeyData . keyDataFromKey

casioCodeFromKey :: AppKey -> Int
casioCodeFromKey = casioCodeFromKeyData . keyDataFromKey

instance Show AppKey where
    show = nameFromKeyData . keyDataFromKey
