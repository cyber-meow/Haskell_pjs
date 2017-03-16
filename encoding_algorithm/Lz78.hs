
module Lz78 where

import qualified Data.List as List
import Data.Char (ord)


type Code = [Int]

-- encoding dictionary structure, represented by a tree
data Dict = Node {repr :: Maybe Char, code :: Int, children :: [Dict]}
  deriving (Eq, Show)

-- for a new entry in the dictionary, next_num is attributed
data Codebook = Codebook {next_num :: Int, dict :: Dict} deriving Show
data PartialCode = PartialCode {done :: Code, rest :: String} deriving Show


-- manipulating different structures

emptyDict :: Dict
emptyDict = Node {repr = Nothing, code = -1, children = []}

emptyCodebook :: Codebook
emptyCodebook = Codebook {next_num = 256, dict = emptyDict}

addChild :: Dict -> Dict -> Dict
addChild child dict = dict {children = child:children dict}

removeChild :: Dict -> Dict -> Dict
removeChild child dict = dict {children = List.delete child $ children dict}


-- parseHead dict s find the longest prefix of (rest s) in dict, returning 
-- 1. the updated dictionary and 2. the partialCode in which this prefix 
-- is replaced by the corresponding index
parseHead :: Codebook -> PartialCode -> (Codebook, PartialCode)
parseHead cb@(Codebook next_num dict) pC@(PartialCode processed rest)
  | rest == "" = (cb, PartialCode (code dict:processed) "")
  | otherwise = 
      let u = head rest in
      case List.find ((== Just u) . repr) $ children dict of
        Nothing -> case repr dict of
          Nothing -> 
            (Codebook next_num $ addChild (Node (Just u) (ord u) []) dict, pC)
          _ ->
            (Codebook (next_num + 1) $ 
             addChild (Node (Just u) next_num []) dict,
             PartialCode (code dict:processed) $ rest)
        Just dict' ->
          let pC' = PartialCode processed $ tail rest
              cb' = Codebook next_num dict'
              (Codebook next_num' dict'', pC'') = parseHead cb' pC' in
          (Codebook next_num' $ addChild dict'' $ removeChild dict' dict, pC'')

-- do the coding with a given initial dictionary
codingStep :: Codebook -> PartialCode -> Code
codingStep codebook partialCode 
  | rest partialCode' == "" = done partialCode'
  | otherwise = codingStep codebook' partialCode'
  where (codebook', partialCode') = parseHead codebook partialCode

lz78Encoding :: String -> Code
lz78Encoding = reverse . codingStep emptyCodebook . PartialCode []
