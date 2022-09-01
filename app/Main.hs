module Main where

import Data.Char (toLower)
import Data.List (intercalate, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import System.IO

dict :: Map.Map Char (Double, Double)
dict =
    Map.fromList
        [ ('a', (0.3, 1.0))
        , ('b', (4.6, 2.0))
        , ('c', (2.6, 2.0))
        , ('d', (2.3, 1.0))
        , ('e', (2.0, 0.0))
        , ('f', (3.3, 1.0))
        , ('g', (4.3, 1.0))
        , ('h', (5.3, 1.0))
        , ('i', (7.0, 0.0))
        , ('j', (6.3, 1.0))
        , ('k', (7.3, 1.0))
        , ('l', (8.3, 1.0))
        , ('m', (6.6, 2.0))
        , ('n', (5.6, 2.0))
        , ('o', (8.0, 0.0))
        , ('p', (9.0, 0.0))
        , ('q', (0.0, 0.0))
        , ('r', (3.0, 0.0))
        , ('s', (1.3, 1.0))
        , ('t', (4.0, 0.0))
        , ('u', (6.0, 0.0))
        , ('v', (3.6, 2.0))
        , ('w', (1.0, 0.0))
        , ('x', (1.6, 2.0))
        , ('y', (0.6, 2.0))
        , ('z', (5.0, 0.0))
        , ('0', (8.7, -1.0))
        , ('1', (-0.3, -1.0))
        , ('2', (0.7, -1.0))
        , ('3', (1.7, -1.0))
        , ('4', (2.7, -1.0))
        , ('5', (3.7, -1.0))
        , ('6', (4.7, -1.0))
        , ('7', (5.7, -1.0))
        , ('8', (6.7, -1.0))
        , ('9', (7.7, -1.0))
        , ('-', (9.6, 2.0))
        , ('ä', (10.3, 1.0))
        , ('ö', (9.3, 1.0))
        , ('ü', (10.0, 0.0))
        ]

distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2

getPos :: Char -> (Double, Double)
getPos = fromMaybe (0, 0) . flip Map.lookup dict . toLower

distanceOfWord :: [Char] -> Double
distanceOfWord (x:y:xs) =
    distance (getPos x) (getPos y) + distanceOfWord (y : xs)
distanceOfWord _ = 0.0

main :: IO ()
main = do
    handle <- openFile "words.txt" ReadMode
    contents <- hGetContents handle
    let allDistances =
            sortOn
                (\(_, _, z) -> z)
                [ results
                | results@(str, len, rel) <-
                      map
                          (\x ->
                               let dow = distanceOfWord x
                                in (x, dow, (dow / fromIntegral (length x)))) $
                      lines contents
                ]
    writeFile "list.txt" $
        unlines $
        map (\(str, len, rel) -> unwords [str, show len, show rel]) allDistances
    seperatorLine "Top 10 longest words"
    mapM_ print $ take 10 . reverse $ allDistances
    hClose handle

seperatorLine :: String -> IO ()
seperatorLine str =
    putStr $
    let size = length str + 4
     in unlines
            [ replicate size '-'
            , concat [replicate ((size - length str) `div` 2) ' ', str]
            , replicate size '-'
            ]
