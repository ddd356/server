module Random where

import System.Random (getStdRandom, randomR)

-- https://github.com/dmp1ce/haskell-examples/blob/master/random/random-examples.hs

-- Get a random item out of a list
selectRandomElement :: [a] -> IO a
selectRandomElement [] = error "Cannot select an element from an empty list."
selectRandomElement list =
  randomIntWithinRange
    >>= \r -> return $ list !! r
  where
    randomIntWithinRange = getStdRandom $ randomR (0, length list - 1)

randomLetterRange :: String
randomLetterRange = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- Example of getting a random a-zA-Z
getRandomLetter :: IO Char
getRandomLetter = selectRandomElement randomLetterRange

-- Example of getting a random string composed of a-zA-Z
getRandomString :: Int -> IO String
getRandomString = addRandomLettersToString ""
  where
    addRandomLettersToString :: String -> Int -> IO String
    addRandomLettersToString s n
      | length s >= n = return s
      | length s < n = do
        a <- getRandomLetter
        addRandomLettersToString (s ++ [a]) n
    addRandomLettersToString _ _ = return ""

--addRandomLetterToString :: String -> IO String
--addRandomLetterToString s = do
--  a <- getRandomLetter
--  return (s ++ a:[])
