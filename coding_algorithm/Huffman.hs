
module Huffman where

import qualified Data.List

data ProbaArbre = Feuille Int Double
                  | Noeud Double ProbaArbre ProbaArbre deriving (Eq, Show)

data Code = Code Int String deriving (Show, Eq, Ord)

value (Feuille _ x) = x
value (Noeud x _ _) = x

insert Nothing _ = []
insert (Just a) [] = [a]
insert (Just a) (x:xs)
  | value a <= value x = a:x:xs
  | otherwise = x:insert (Just a) xs

{- la liste doit être triée dans l'ordre croissant -}
fusionP1P2 [] = (Nothing, [])
fusionP1P2 [x] = (Nothing, [])
fusionP1P2 (x:y:ys) = 
  (Just $ Noeud (value x + value y) x y, ys)

{- arberList1 l = iterate (\l' -> uncurry insert (fusionP1P2 l')) l -}
arbreList:: [ProbaArbre] -> [[ProbaArbre]]
arbreList = iterate $ uncurry insert . fusionP1P2

arbreOfProbas nps = [Feuille n x | (n,x) <- nps]

giveNumbers ps = zip [0..length ps - 1] ps

finalArbre = last . takeWhile (/=[]) . arbreList .
             arbreOfProbas . (Data.List.sortOn snd) . giveNumbers

ps = [0.3,0.2,0.2,0.12,0.1,0.03,0.02,0.02,0.01]

codage (Feuille n _) = [Code n ""]
codage (Noeud _ a1 a2) =
  [Code n ('0':x) | Code n x <- codage a1] ++ 
  [Code m ('1':y) | Code m y <- codage a2]

codeOfProbas = Data.List.sort . codage . head . finalArbre

main = putStrLn $ show $ codeOfProbas ps

bernouilli a b 1 = [a, b]
bernouilli a b n = [a*x | x <- tmp] ++ [b*x | x <- tmp] 
  where tmp = bernouilli a b $ n-1

averageCodelength cs ps = 
  sum [ fromIntegral (length c) * p | ((Code _ c), p) <- zip cs ps ]

entropy2 = (0-). sum . (map (\x -> x * logBase 2 x))

findEx6 m 
  | averageCodelength cs ps / m <= entropy2 [0.75, 0.25] + 0.01 = m
  | otherwise = findEx6 (m + 1)
  where ps = bernouilli 0.75 0.25 m
        cs = codeOfProbas ps

enAlphabet = map (/100) 
  [8.167, 1.492, 2.752, 4.253, 12.702, 2.228, 2.015, 6.094, 9.966, 0.153,
   0.772, 4.025, 2.406, 6.749, 7.507, 1.929, 0.095, 5.987, 6.327, 9.056,
   2.758, 0.978, 2.360, 0.150, 1.974, 0.074]
