module Main where

import Hangman
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Maybe
import Data.Char (toLower)
import Control.Monad

letters :: Gen Char
letters = elements ['a'..'z']

makePuzzle :: String -> Gen Puzzle
makePuzzle word = do 
  guessedRight <-
          replicateM (length word) arbitrary :: Gen [Bool]
  let
    spotGuess guess c = if guess then Just c else Nothing
    guessed = zipWith spotGuess guessedRight word
    guesses = catMaybes guessed in
   return (Puzzle word guessed guesses)

prop_handleGuess :: String -> Property
prop_handleGuess word =
  forAll (makePuzzle word)
   (\puzzle@(Puzzle _ _ guesses) -> monadicIO $ forAllM letters
                (\letter -> do
                  newPuzzle@(Puzzle _ _ newGuesses) <- run $
                                     handleGuess puzzle letter
                  case alreadyGuessed puzzle letter of
                    True  -> return $ puzzle == newPuzzle
                    False -> return $ newGuesses == letter : guesses))

prop_characterFill :: Puzzle -> Property
prop_characterFill puzzle@(Puzzle word _ s) =
  forAll letters
  (\guess -> if guess `elem` word
             then case fillInCharacter puzzle guess of
                    (Puzzle _ newDiscoveredChars guesses) ->
                      Just guess `elem` newDiscoveredChars &&
                      guesses == guess : s
             else case fillInCharacter puzzle guess of
                    (Puzzle _ newDiscoveredChars guesses) ->
                      not (Just guess `elem` newDiscoveredChars) &&
                      guesses == guess : s)

main :: IO ()
main = do
  word <- randomWord'
  quickCheck $ forAll (makePuzzle word) prop_characterFill
  quickCheck $ prop_handleGuess word
