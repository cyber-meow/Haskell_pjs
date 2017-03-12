
module Huffman (huffmanCoding) where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

data ProbaArbre = Feuille Char Double
                | Noeud Double ProbaArbre ProbaArbre deriving (Eq, Show)

type Code = String
type Codebook = Map Char Code

value :: ProbaArbre -> Double
value (Feuille _ x) = x
value (Noeud x _ _) = x

-- insérer un arbre dans la liste d'arbres en respectant l'ordre
insert :: Maybe ProbaArbre -> [ProbaArbre] -> [ProbaArbre]
insert Nothing _ = []
insert (Just a) [] = [a]
insert (Just a) (x:xs)
  | value a <= value x = a:x:xs
  | otherwise = x:insert (Just a) xs

-- la liste doit être triée dans l'ordre croissant 
-- fusionner les duex feuilles de probas minimum quand c'est possible
fusionP1P2 :: [ProbaArbre] -> (Maybe ProbaArbre, [ProbaArbre])
fusionP1P2 [] = (Nothing, [])
fusionP1P2 [x] = (Nothing, [])
fusionP1P2 (x:y:ys) = (Just $ Noeud (value x + value y) x y, ys)

-- arberList l = iterate (\l' -> uncurry insert (fusionP1P2 l')) l
arbreList :: [ProbaArbre] -> [[ProbaArbre]]
arbreList = iterate $ uncurry insert . fusionP1P2

arbresOfProbas :: [(Char, Double)] -> [ProbaArbre]
arbresOfProbas = map $ uncurry Feuille 

finalArbre :: Map Char Double -> [ProbaArbre]
finalArbre = last . takeWhile (/=[]) . arbreList .
             arbresOfProbas . (List.sortOn snd) . Map.toList

codage :: ProbaArbre -> Codebook
codage (Feuille c _) = Map.singleton c ""
codage (Noeud _ a1 a2) =
  Map.map ('0':) (codage a1) `Map.union` Map.map ('1':) (codage a2)

huffmanCoding :: Map Char Double -> Codebook
huffmanCoding = codage . head . finalArbre

