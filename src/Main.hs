module Main where

  import Control.Monad (forever)
  import Data.Char (toLower)
  import Data.Maybe (isJust)
  import Data.List (intersperse)
  import System.Exit (exitSuccess)
  import System.Random (randomRIO)

  type WordList = [String]

  main :: IO ()
  main = do
    putStrLn "hello world"

  allWords :: IO WordList
  allWords = do
    dict <- readFile "data/words"
    return (lines dict)

  minWordLength :: Int
  minWordLength = 5

  maxWordLength :: Int
  maxWordLength = 9

  randomWord :: WordList -> IO String
  randomWord wl = do
    randomIndex <- randomRIO (0, index)
    return $ wl !! randomIndex
      where
        index = (length wl - 1)

  gameWords :: IO WordList
  gameWords = do
    aw <- allWords
    return (filter gameLength aw)
      where gameLength w =
                let l = length (w :: String)
                in l >= minWordLength
                  && l < maxWordLength

  randomWord :: IO String
  randomWord = gameWords >>= randomWord

  data Puzzle =
          Puzzle String [Maybe Char] [Char]

  instance Show Puzzle where
      show (Puzzle _ discovered guessed) =
          (intersperse ' ' $
            fmap renderPuzzleChar discovered)
            ++ " Guessed so far: " ++ guessed

  freshPuzzle :: String -> Puzzle
  freshPuzzle = map (const Nothing)

  charInWord :: Puzzle -> Char -> Bool
  charInWord (Puzzle word _ _) c = word !! c

  alreadyGuessed :: Puzzle -> Char -> Bool
  alreadyGuessed = undefined

