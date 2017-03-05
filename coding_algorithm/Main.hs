
module Main where

import System.Environment
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

import Huffman

-- count the number of occurence of each letter in an ASCII file
countTimes :: String -> Map Char Int
countTimes str = Map.fromListWith (+) $ zip str (repeat 1)

probaMap :: Map Char Int -> Map Char Double
probaMap m = Map.map ((/numTot) . fromIntegral) m
  where numTot = fromIntegral $ sum $ Map.elems m

countFrequency :: String -> Map Char Double
countFrequency = probaMap . countTimes

-- coding function that converts a string to its code
-- if the coding algorithm is correct, the result is Just something
coding :: String -> Maybe String
coding str = fmap concat $  sequence $ map (\c -> Map.lookup c codebook) str
 where codebook = huffmanCoding . countFrequency $ str

main = do
  args <- getArgs
  fileContent <- readFile $ head args  
  case coding fileContent of
    Nothing -> hPutStrLn stderr
      "something wrong while trying to coding the source"
    Just code -> writeFile (args!!1) code


