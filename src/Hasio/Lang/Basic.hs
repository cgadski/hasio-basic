module Hasio.Lang.Basic
    ( Expr, Command, Program
    ) where

newtype NumVar = NumVar Char deriving (Eq)
newtype Label = Label Int deriving (Eq)

data Expr =
    LiteralInt -- ^ an integer
    | RefNumVar NumVar -- ^ references a numeric var
    | Add Expr Expr
    | Multiply Expr Expr
    | Power Expr Expr

data Command =
    Empty -- ^ Empty, does nothing.
    | Display Expr -- ^ Puts the expression on the screen, debug style.

    | DefLabel Label -- ^ Defines a label.
    | GotoLabel Label -- ^ Goes to a label.

    | DefIfBegin Expr
        -- ^ Begins an if statement, with an expression as predicate.
    | DefElse -- ^ Begins the else branch of an if statement.
    | DefIfEnd -- ^ Ends an if statement.

    | AssignVar Expr NumVar


type Program = [Command]
