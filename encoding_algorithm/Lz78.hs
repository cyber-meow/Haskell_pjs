
module Lz78 where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (ord, chr)
import Control.Monad (foldM)



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


-- parseHead dict s find the longest prefix of (rest s) in dict, returning 
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
            (Codebook (nextNum + 1) $ 
             addChild (Node (Just u) nextNum []) dict,
             PartialCode (code dict:processed) $ rest)
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

lz78Encoding :: String -> Code
lz78Encoding = reverse . codingStep emptyCodebook . PartialCode []



-- decoding part

data Decodebook = Decodebook {nextNumD :: Int, dictD :: Map Int String}
type Decoding = (Decodebook, String, String)

emptyDecodebook :: Decodebook
emptyDecodebook = Decodebook {nextNumD = 256, dictD = Map.empty}

decodingInit :: Char -> Decoding
decodingInit c = (emptyDecodebook, [c], [c])

decodebookUpdate :: Decodebook -> String -> Decodebook
decodebookUpdate (Decodebook n dictD) str = 
  Decodebook (n + 1) $ Map.insert n str dictD


decodeSeg :: Decoding -> Int -> Maybe Decoding
decodeSeg (dcbook, res, oldphrase) code =
  fmap (\p -> (decodebookNew p, res ++ p, p)) phrase
  where phrase | code < 256 = Just [chr code]
               | code < nextNumD dcbook = Map.lookup code $ dictD dcbook
               | code == nextNumD dcbook = Just $ oldphrase ++ [oldphrase!!0]
               | otherwise = Nothing
        decodebookNew p = decodebookUpdate dcbook (oldphrase ++ [p!!0])

lz78Decoding :: Code -> Maybe String
lz78Decoding [] = Just ""
lz78Decoding (c:cs) = fmap (\(_, x, _) -> x) decodeRes
  where dcInit = decodingInit $ chr c
        decodeRes = foldM decodeSeg dcInit cs

