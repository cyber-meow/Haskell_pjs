
module Lz78 (lz78Encoding, lz78Decoding) where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (ord, chr)
import Control.Monad (foldM)

import Data.Bits 
import Data.Word
import Data.Digits (digits, unDigits)
import qualified Data.ByteString.Lazy as B



blockSize = 12
dictMaxSize = 4096


type Code = [Int]

-- encoding dictionary structure, represented by a tree
data Dict = Node {repr :: Maybe Char, code :: Int, children :: [Dict]}
  deriving (Eq, Show)

-- for a new entry in the dictionary, nextNum is attributed
data Codebook = Codebook {nextNum :: Int, dict :: Dict} deriving Show
data PartialCode = PartialCode {done :: Code, rest :: String} deriving Show


-- manipulating different structures

emptyDict :: Dict
emptyDict = Node {repr = Nothing, code = -1, children = []}

emptyCodebook :: Codebook
emptyCodebook = Codebook {nextNum = 256, dict = emptyDict}

addChild :: Dict -> Dict -> Dict
addChild child dict = dict {children = child:children dict}

removeChild :: Dict -> Dict -> Dict
removeChild child dict = dict {children = List.delete child $ children dict}


-- parseHead dict s finds the longest prefix of (rest s) in dict, returning 
-- 1. the updated dictionary and 2. the partialCode in which this prefix 
-- is replaced by the corresponding index
parseHead :: Codebook -> PartialCode -> (Codebook, PartialCode)
parseHead cb@(Codebook nextNum dict) pC@(PartialCode processed rest)
  | rest == "" = (cb, PartialCode (code dict:processed) "")
  | otherwise = 
      let u = head rest in
       case List.find ((== Just u) . repr) $ children dict of
        Nothing -> case repr dict of
          Nothing -> 
            (Codebook nextNum $ addChild (Node (Just u) (ord u) []) dict, pC)
          _ ->
            let pC' = PartialCode (code dict:processed) $ rest in
            if nextNum >= dictMaxSize then (cb, pC')
            else
              (Codebook (nextNum + 1) $ 
              addChild (Node (Just u) nextNum []) dict, pC')
        Just dict' ->
          let pC' = PartialCode processed $ tail rest
              cb' = Codebook nextNum dict'
              (Codebook nextNum' dict'', pC'') = parseHead cb' pC' in
          (Codebook nextNum' $ addChild dict'' $ removeChild dict' dict, pC'')

-- do the coding with a given initial dictionary
codingStep :: Codebook -> PartialCode -> Code
codingStep codebook partialCode 
  | rest partialCode' == "" = done partialCode'
  | otherwise = codingStep codebook' partialCode'
  where (codebook', partialCode') = parseHead codebook partialCode

lz78EncodingPre :: String -> Code
lz78EncodingPre "" = []
lz78EncodingPre s = reverse . codingStep emptyCodebook $ PartialCode [] s



-- decoding part

data Decodebook = Decodebook {nextNumD :: Int, dictD :: Map Int String}
type Decoding = (Decodebook, String, String)

emptyDecodebook :: Decodebook
emptyDecodebook = Decodebook {nextNumD = 256, dictD = Map.empty}

decodingInit :: Char -> Decoding
decodingInit c = (emptyDecodebook, [c], [c])

decodebookUpdate :: Decodebook -> String -> Decodebook
decodebookUpdate dcb@(Decodebook n dictD) str
  | n >= dictMaxSize = dcb
  | otherwise = Decodebook (n + 1) $ Map.insert n str dictD


decodeSeg :: Decoding -> Int -> Maybe Decoding
decodeSeg (dcbook, res, oldphrase) code =
  fmap (\p -> (decodebookNew p, res ++ p, p)) phrase
  where phrase | code < 256 = Just [chr code]
               | code < nextNumD dcbook = Map.lookup code $ dictD dcbook
               | code == nextNumD dcbook = Just $ oldphrase ++ [oldphrase!!0]
               | otherwise = Nothing
        decodebookNew p = decodebookUpdate dcbook (oldphrase ++ [p!!0])

lz78DecodingPre :: Code -> Maybe String
lz78DecodingPre [] = Just ""
lz78DecodingPre (c:cs) = fmap (\(_, x, _) -> x) decodeRes
  where dcInit = decodingInit $ chr c
        decodeRes = foldM decodeSeg dcInit cs



-- deal with real input and output

{- These functions are conceived to be used with well constructed codes 
   (that is, each integer in the list must be smaller than dictMaxSize),
   if this is not hte case, the result will not make sense -}

-- from list of 0 and 1 to a list of Word8, add 0 at the end
toWords :: [Int] -> [Word8]
toWords s 
  | len <= 8 = [fromIntegral $ shiftL (unDigits 2 s) (8 - len)]
  | otherwise = fromIntegral (unDigits 2 hd) : toWords tl
  where len = length s
        (hd, tl) = splitAt 8 s

toN :: Num a => Int -> [a] -> [a]
toN k l = replicate (k - length l) 0 ++ l

fromWords :: [Word8] -> [Int]
fromWords = concat . map (toN 8 . digits 2 . fromIntegral)
              
-- from list of 0 and 1 to list of integers representing indices of dictionary
toIndices :: [Int] -> Code
toIndices c01
  | length c01 < blockSize = []
  | otherwise = unDigits 2 hd : toIndices tl
  where (hd, tl) = splitAt blockSize c01

-- now, from and to bytestring

toByteString :: Code -> B.ByteString
toByteString = B.pack . toWords . concat . map (toN blockSize . digits 2)

fromByteString :: B.ByteString -> Code
fromByteString = toIndices . fromWords . B.unpack


-- interface functions

lz78Encoding :: String -> B.ByteString
lz78Encoding = toByteString . lz78EncodingPre

lz78Decoding :: B.ByteString -> Maybe String
lz78Decoding = lz78DecodingPre . fromByteString
