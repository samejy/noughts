module Lib
    ( play
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer
import Text.Read (readMaybe)
import qualified Data.Text as T
import System.Random

type Game = WriterT [Move] IO (Player, Board)

data Player = AI | Human | Noone
            deriving Show

data Move = Move Player T.Text T.Text
          deriving Show

data Cell = Nought | Cross | Empty
  deriving (Show, Eq)

type Board = [Cell]

start :: Board
start = take 9 $ repeat Empty

put :: Board -> Cell -> Int -> Maybe Board
put (b:bs) c ind
    | ind > 8 || ind < 0 = Nothing
    | ind == 0 = case b of
        Empty -> Just $ c : bs
        otherwise -> Nothing
    | otherwise = do
        rest <- put bs c (ind - 1)
        Just $ b : rest

randPut :: Board -> Cell -> Int -> Maybe Board
randPut b c seed = do
  idx <- (pick (empties b 0) seed)
  put b c idx
  where
    empties (x:xs) ix = case x of
      Empty -> ix : empties xs (ix + 1)
      otherwise -> empties xs (ix + 1)
    empties [] _ = []
    pick [] _ = Nothing
    pick xs seed =
      let (rind,_) = randomR (0, (length xs) - 1) (mkStdGen seed)
      in Just $ xs !! rind

showCell Nought = "0"
showCell Cross = "X"
showCell Empty = "_"

showBoard :: Board -> IO ()
showBoard [] = return ()
showBoard b = do
  putStrLn $ "|" ++ (foldr (++) "" $ map (++ "|") $ map showCell $ take 3 b)
  showBoard $ drop 3 b

headOrDefault (x:_) _ = x
headOrDefault [] d = d

serialiseBoards :: Board -> Board -> (T.Text, T.Text)
serialiseBoards b b' = (serialise b, serialise b')
  where
    fst = headOrDefault (dropWhile (\c -> c == Empty) b) Cross
    xfirst c
      | c == fst = Cross
      | c == Empty = Empty
      | otherwise = Nought
    serialise = foldr (T.append . T.pack . showCell . xfirst) T.empty

wins :: Board -> Cell -> Bool
wins (b1:b2:b3:b4:b5:b6:b7:b8:b9:[]) c
  | b1 == b2 && b2 == b3 && b3 == c ||
    b4 == b5 && b5 == b6 && b6 == c ||
    b7 == b8 && b8 == b9 && b9 == c ||
    b1 == b4 && b4 == b7 && b7 == c ||
    b2 == b5 && b5 == b8 && b8 == c ||
    b3 == b6 && b6 == b9 && b9 == c ||
    b1 == b5 && b5 == b9 && b9 == c ||
    b3 == b5 && b5 == b7 && b7 == c = True
  | otherwise  = False

canMove :: Board -> Bool
canMove = any (\c -> c == Empty)

aiMove :: Board -> Game
aiMove b = do
  s <- lift randomIO
  let mb = randPut b Nought s
  case mb of
    Nothing -> lift $ return (Noone, b)
    Just b' -> success b b' AI Nought humanMove

humanPlay :: Board -> IO Int
humanPlay b = do
  showBoard b
  putStrLn "Enter the index (1-9):"
  i <- getLine
  case readMaybe i of
    Just ind -> return (ind - 1)
    Nothing -> humanPlay b

humanMove :: Board -> Game
humanMove b = do
  i <- lift $ humanPlay b
  let mb = put b Cross i
  case mb of
    Nothing -> (lift $ putStrLn "Invalid, try again") >> humanMove b
    Just b' -> success b b' Human Cross aiMove

success :: Board -> Board -> Player -> Cell -> (Board -> Game) -> Game
success bd bd' p c nxt = do
  let (s, s') = (serialiseBoards bd bd')
  tell [Move p s s']
  if wins bd' c
    then lift $ return (p, bd')
    else if canMove bd' then nxt bd' else lift $ return (Noone, bd')

humanStart = humanMove start
aiStart = aiMove start

play :: IO ()
play = do
  ind <- randomRIO (0, 1)
  let fst = [humanStart, aiStart] !! ind
  ((winner,b),log) <- runWriterT fst
  putStrLn $ show winner ++ " wins!"
  showBoard b
  putStrLn $ show log
