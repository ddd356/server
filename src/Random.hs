module Random where

import System.Random (getStdRandom, randomR, random, randomRs, newStdGen)

-- https://github.com/dmp1ce/haskell-examples/blob/master/random/random-examples.hs

-- Get a random item out of a list
selectRandomElement :: [a] -> IO a
selectRandomElement [] = error "Cannot select an element from an empty list."
selectRandomElement list = randomIntWithinRange >>=
  \r -> return $ list !! r
  where
  randomIntWithinRange = getStdRandom $ randomR (0, length list - 1)

randomLetterRange = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- Example of getting a random a-zA-Z 
getRandomLetter :: IO Char
getRandomLetter = selectRandomElement randomLetterRange

-- Example of getting a random string composed of a-zA-Z
getRandomString :: Int -> IO String
getRandomString size = addRandomLettersToString "" size
  where
  addRandomLettersToString :: String -> Int -> IO String
  addRandomLettersToString s n
    | length s >= n = return s
    | length s < n = do
      a <- getRandomLetter
      b <- addRandomLettersToString (s ++ a:[]) n
      return b
  addRandomLetterToString :: String -> IO String
  addRandomLetterToString s = do
    a <- getRandomLetter
    return (s ++ a:[])
