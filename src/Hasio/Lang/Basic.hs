-- |
module Hasio.Lang.Basic
    (
    -- * AST
    Expr, Command, Program
    -- * Parser
    , parseCommand
    , parseProgram
    ) where

import Data.List

-- * AST

newtype NumVar = NumVar Char deriving (Eq, Show)
newtype Label = Label Int deriving (Eq, Show)

data Expr =
    LiteralInt -- ^ an integer
    | RefNumVar NumVar -- ^ references a numeric var
    | Add Expr Expr
    | Multiply Expr Expr
    | Power Expr Expr deriving (Eq, Show)

data Command =
    Empty -- ^ Empty, does nothing.
    | Display Expr -- ^ Puts the expression on the screen, debug style.

    | DefLabel Label -- ^ Defines a label.
    | GotoLabel Label -- ^ Goes to a label.

    | DefIfBegin Expr
        -- ^ Begins an if statement, with an expression as predicate.
    | DefElse -- ^ Begins the else branch of an if statement.
    | DefIfEnd -- ^ Ends an if statement.

    | AssignVar Expr NumVar deriving (Eq, Show)

type Program = [Command]

-- * Parser

type Location = Int

parseFail :: Location -> String -> Either a String
parseFail loc str = Right . concat $
    [ "BASIC parse error: " , str, " (line ", show loc, ")"]

parseLabel :: String -> Maybe Label
parseLabel [x] = Label . snd <$> find ((== x) . fst) (zip ['0'..'9'] [0..9])
parseLabel _ = Nothing

parseExpr :: String -> Maybe Expr
parseExpr str =
    case str of
        '(':xs -> undefined
        _ -> undefined
    where
        toNextParens =
            foldr ()

parseCommand :: Location -> String -> Either Command String
parseCommand loc str =
    case words str of
        ["Lbl", labelStr] -> case parseLabel labelStr of
            Nothing -> parseFail loc "bad label identifier in DefLabel"
            Just label -> Left $ DefLabel label
        ["Goto", labelStr] -> case parseLabel labelStr of
            Nothing -> parseFail loc "bad label identifier in GotoLabel"
            Just label -> Left $ GotoLabel label
        ["If", _] -> case parseExpr (unwords . tail . words $ str) of
            Nothing -> parseFail loc "bad predicate expression in DefIfBegin"
            Just expr -> Left $ DefIfBegin expr
        _ -> parseFail loc "bad command"

parseProgram :: String -> Either Program String
parseProgram str = Right "haven't implemented yet"
