
module Main where

import System.Environment
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char
import Data.Word
import Data.Digits (unDigits)
import qualified Data.ByteString.Lazy as B

import Huffman
import Lz78

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

groupWord :: String -> [Word8]
groupWord [] = []
groupWord s = fromIntegral (unDigits 2 (map digitToInt hd)) : groupWord tl
  where (hd, tl) = splitAt 8 s

-- convert a string of 0 and 1 to byte string
toByteString :: String -> B.ByteString
toByteString = B.pack . groupWord

main = do
  args <- getArgs
  case head args of
    "encode" -> do
      fileContent <- readFile $ args!!1
      B.writeFile (args!!2) $ lz78Encoding fileContent
    "decode" -> do
      fileContent <- B.readFile $ args!!1
      case lz78Decoding fileContent of
        Nothing -> hPutStrLn stderr 
          "something wrong while trying to decode the source"
        Just content -> writeFile (args!!2) content
        

  {-case coding fileContent of
    Nothing -> hPutStrLn stderr
      "something wrong while trying to code the source"
    Just code -> B.writeFile (args!!1) (toByteString code)-}


