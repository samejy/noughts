module Types where

import qualified Data.Text as T

data Player = AI | Human | Noone
            deriving Show

data Move = Move Player T.Text T.Text
          deriving Show

data Cell = Nought | Cross | Empty
          deriving (Show, Eq)

type Board = [Cell]

data Status = Play | Win | Lose | Draw
            deriving Show
