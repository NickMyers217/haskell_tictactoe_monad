module Main where

import Control.Monad.State
import Data.List

-- | Defines the type for a Cell on the Board
-- | It can be either an X, O, or Empty
data Cell = X
          | O
          | Empty
          deriving Eq

instance Show Cell where
  show X     = "X"
  show O     = "O"
  show Empty = "."

-- | Defines the type for a Board
-- | It is a grid of Cells
-- | The defaultBoard is a 9 x 9 grid of Empty Cells
type Board = [[Cell]]
type Pos   = (Int, Int) -- ^A pos is just a (x,y) pair

defaultBoard :: Board
defaultBoard = [ [Empty, Empty, Empty]
               , [Empty, Empty, Empty]
               , [Empty, Empty, Empty]
               ]

-- | Defines the State for the game
-- | It consists of:
-- | board :: the current state of the Board
-- | turn  :: the Cell representing the current player
data GameState = GameState { board :: Board
                           , turn  :: Cell
                           }
defaultState :: GameState
defaultState = GameState { board = defaultBoard, turn = X }

-- | The TicTacToe monad is outlined as follows:
-- | The first layer is a State monad transformer that encapsulates a GameState
-- | The second layer is an IO monad that encapsulate IO actions
type TTT a = StateT GameState IO a

-- Checks to see if a pos is valid
validPos :: Board -> Pos -> Bool
validPos b (x,y)
  | x < 0 || x > 2 || y < 0 || y > 2 = False
  | b !! y !! x == Empty             = True
  | otherwise                        = False

-- | Turns a board into a String
showBoard :: Board -> String
showBoard = unlines . map (concatMap show)

-- | Replace the nth element in a list
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) xs (i, v) = take i xs ++ [v] ++ drop (i + 1) xs

-- | Modifies a Cell and returns the new Board
modifyBoard :: Board -> (Pos, Cell) -> Board
modifyBoard b ((x,y), v) = b !!= (y, row !!= (x, v))
  where row = b !! y

-- Gets all the threes on the board
boardThrees :: Board -> Board
boardThrees b = b ++ transpose b ++ map (map (\(x,y) -> b !! y !! x)) diags
  where diags = [ [(0,0), (1,1), (2,2)], [(0,2), (1,1), (2,0)] ]

-- | Checks for vicotry
boardWon :: Board -> Bool
boardWon b = any matches $ boardThrees b
  where matches xs = xs == replicate 3 X || xs == replicate 3 O

boardTied :: Board -> Bool
boardTied = notElem Empty . concat

gameOver :: GameState -> Bool
gameOver GameState { board = b } = boardWon b || boardTied b


-- | Gets a player's move
getMove :: GameState -> IO Pos
getMove g@(GameState { board = b }) = do
  putStr "Enter an x value: "
  x <- getLine
  putStr "Enter an y value: "
  y <- getLine
  if not $ validPos b (read x, read y)
    then getMove g
    else return (read x, read y)


-- | TicTacToe inside the TicTacToe monad
tictactoe :: TTT ()
tictactoe = do
  -- Get the current GameState, print out the Board
  s <- get
  liftIO $ putStrLn $ showBoard $ board s

  -- Make sure the game isn't over
  if gameOver s
     then liftIO $ putStrLn "Game Over!"
     else do
         -- Grab the user input for their move
         pos <- liftIO $ getMove s

         -- Update the GameState accordingly
         put GameState { board = modifyBoard (board s) (pos, turn s)
                       , turn  = if turn s == X then O else X
                       }
         -- Recurse
         tictactoe


-- | The main function
main :: IO ()
main = do
  (_, st) <- runStateT tictactoe defaultState
  putStrLn . showBoard . board $ st
