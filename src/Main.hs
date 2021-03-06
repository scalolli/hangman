module Main where

  import Control.Monad (forever)
  import Data.Char (toLower)
  import Data.Maybe (isJust)
  import Data.List (intersperse)
  import System.Exit (exitSuccess)
  import System.Random (randomRIO)

  newtype WordList = WordList [String] deriving (Eq, Show)

  allWords :: IO WordList
  allWords = do
    dict <- readFile "data/words"
    return $ WordList (lines dict)

  minWordLength :: Int
  minWordLength = 5

  maxWordLength :: Int
  maxWordLength = 9

  randomWord :: WordList -> IO String
  randomWord (WordList wl) = do
    randomIndex <- randomRIO (0, index)
    return $ wl !! randomIndex
      where
        index = (length wl - 1)

  gameWords :: IO WordList
  gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
      where gameLength w =
                let l = length (w :: String)
                in l >= minWordLength
                  && l < maxWordLength

  randomWord' :: IO String
  randomWord' = gameWords >>= randomWord

  data Puzzle =
          Puzzle String [Maybe Char] [Char]

  instance Show Puzzle where
      show (Puzzle _ discovered guessed) =
          (intersperse ' ' $
            fmap renderPuzzleChar discovered)
            ++ " Guessed so far: " ++ guessed

  freshPuzzle :: String -> Puzzle
  freshPuzzle w = Puzzle w (map (const Nothing) w) []

  charInWord :: Puzzle -> Char -> Bool
  charInWord (Puzzle word _ _) c = c `elem` word

  alreadyGuessed :: Puzzle -> Char -> Bool
  alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed

  renderPuzzleChar :: Maybe Char -> Char
  renderPuzzleChar (Just c) = c
  renderPuzzleChar        _ = '_'

  fillInCharacter :: Puzzle -> Char -> Puzzle
  fillInCharacter (Puzzle word filledInSoFar s) c =
    Puzzle word newFilledInSoFar (c:s)
    where zipper guessed wordChar guessChar =
                if wordChar == guessed
                 then Just wordChar
                 else guessChar

          newFilledInSoFar = zipWith (zipper c) word filledInSoFar


  handleGuess :: Puzzle -> Char -> IO Puzzle
  handleGuess puzzle guess = do
      putStrLn $ "Your guess was: " ++ [guess]
      case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
          putStrLn "You already guess that\
                    \ character pick \
                    \ somehting else!"
          return puzzle
        (True, _) -> do
          putStrLn "This character was in the word\
                   \ filling in the word\
                   \ accordingly"
          return (fillInCharacter puzzle guess)

        (False, _) -> do
          putStrLn "This character wasn't in\
                   \ the word, try again."
          return (fillInCharacter puzzle guess)


  gameOver :: Puzzle -> IO ()
  gameOver p@(Puzzle wordToGuess _ guessed) =
      if (countIncorrectGuesses p) > (length wordToGuess) then
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess
      else
        return ()

  countIncorrectGuesses :: Puzzle -> Int
  countIncorrectGuesses (Puzzle word _ guessed) =
      foldr (\a b -> if (not $ a `elem` word) then (b + 1) else b) 0 guessed

  gameWin :: Puzzle -> IO ()
  gameWin (Puzzle _ guessedSoFar _) =
    if all isJust guessedSoFar then
      do
        putStrLn "You Win!"
        exitSuccess
    else
      return ()

  runGame :: Puzzle -> IO ()
  runGame puzzle = forever $ do
      gameOver puzzle
      gameWin puzzle
      putStrLn $
        "Current puzzle is: " ++ show puzzle
      putStr "Guess a letter: "
      guess <- getLine
      case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ ->
            putStrLn "Your guess must be a single character"

  main :: IO ()
  main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle

