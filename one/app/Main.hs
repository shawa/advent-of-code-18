module Main where

import Data.Either as Either (rights)
import qualified Data.Set as Set (empty, insert, member)

import Data.Maybe as Maybe (fromJust)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

parseNumbers :: [String] -> [Int]
parseNumbers = Either.rights . map (parse int [])

partOne :: [Int] -> Int
partOne = sum

partTwo :: [Int] ->  Int
partTwo = Maybe.fromJust . firstRepeating . scanl (+) 0 . cycle
  -- we're looking for the first repeating element of an infinite list
  -- and firstRepeating only gives a Nothing if it reaches the end of the list
  -- so either it gives a Just and doesn't crash, or never terminates...

firstRepeating :: [Int] -> Maybe Int
firstRepeating = go Set.empty
  where go _ []                            = Nothing
        go acc (x:xs) | x `Set.member` acc = Just x
                      | otherwise          = go (Set.insert x acc) xs

main :: IO ()
main = do
  input <- getContents
  let parsed = parseNumbers $ lines input
  putStrLn $ "1: " ++ (show $ partOne parsed)
  putStrLn $ "2: " ++ (show $ partTwo parsed)
