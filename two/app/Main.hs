module Main where

import Control.Arrow ((>>>))

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

isComponent :: Int -> Bool
isComponent = flip Set.member $Set.fromList [2, 3]

occurrences :: Ord k => [k] -> Map.Map k Int
occurrences = List.map (flip (,) 1) >>> Map.fromListWith (+)

checksumComponents :: Map.Map k Int -> [Int]
checksumComponents = Map.elems >>> List.nub >>> filter isComponent

partOne :: [String] -> Int
partOne = List.map (occurrences >>> checksumComponents)
          >>> List.concat
          >>> occurrences
          >>> Map.elems
          >>> product

main :: IO ()
main = do
  input <- getContents
  putStrLn $ "1: " ++ (show . partOne . lines $ input)
