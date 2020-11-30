module Lib
    ( play
    ) where

import Types
import Repo


import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Text.Read (readMaybe)
import qualified Data.Text as T
import System.Random

start :: Board
start = take 9 $ repeat Empty

putC :: Board -> Cell -> Int -> Maybe Board
putC (b:bs) c ind
    | ind > 8 || ind < 0 = Nothing
    | ind == 0 = case b of
        Empty -> Just $ c : bs
        otherwise -> Nothing
    | otherwise = do
        rest <- putC bs c (ind - 1)
        Just $ b : rest

randPut :: Board -> Cell -> Int -> Maybe Board
randPut b c seed = do
  idx <- (pick (empties b 0) seed)
  putC b c idx
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

move Human = humanMove
move AI = aiMove

nextPlayer Human = AI
nextPlayer AI = Human

playerCell Human = Cross
playerCell AI = Nought

otherCell Nought = Cross
otherCell Cross = Nought

canMove :: Board -> Bool
canMove = any (\c -> c == Empty)

status :: Board -> Cell -> Status
status b c
  | wins b c = Win
  | not $ canMove b = Draw
  | wins b $ otherCell c = Lose
  | otherwise = Play

aiMove :: Board -> IO Board
aiMove b = do
  s <- randomIO
  let mb = randPut b Nought s
  case mb of
    Nothing -> error "ARGH. This shouldn't be possible."
    Just b' -> return b'

askHuman :: Board -> IO Int
askHuman b = do
  showBoard b
  putStrLn "Enter the index (1-9):"
  i <- getLine
  case readMaybe i of
    Just ind -> return (ind - 1)
    Nothing -> askHuman b

humanMove :: Board -> IO Board
humanMove b = do
  i <- askHuman b
  let mb = putC b Cross i
  case mb of
    Nothing -> putStrLn "Invalid, try again" >> humanMove b
    Just b' -> return b'

iterateG :: StateT (Player, Board) (WriterT [Move] IO) (Player, Board)
iterateG = do
  (p,b) <- get 
  let p' = nextPlayer p
  b' <- liftIO $ move p' b
  let (s, s') = (serialiseBoards b b')
  lift $ tell [Move p' s s']
  case status b' (playerCell p') of
    Win -> liftIO $ return (p', b')
    Draw -> liftIO $ return (Noone, b')
    Lose -> liftIO $ return (p, b')
    Play -> put (p', b') >> iterateG

runGame first = runWriterT (runStateT iterateG (first, start))

play :: IO ()
play = do
  ind <- randomRIO (0, 1)
  let fst = [Human, AI] !! ind
  (((w,b),_),log) <- runGame fst
  putStrLn $ show w ++ " wins!"
  showBoard b
  putStrLn $ show log
  test
