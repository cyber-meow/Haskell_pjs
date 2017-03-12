
module Lz78 where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

type Code = String
type Codebook = Map String Code

data PartialCode = PartialCode {processed :: Code, rest :: String}

-- return word used to parse and the result
parseHead :: Codebook -> String -> (String, PartialCode)
parseHead codebook text = 
  
  where prefixes = List.inits text
        wordUsed = List.find (flip Map.member $ Codebook) prefixes

-- do one step of the algorithm
codingStep :: Codebook -> PartialCode -> Code
codingStep codebook partialCode
