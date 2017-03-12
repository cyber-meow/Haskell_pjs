
module STFT (stft, istft) where

import Data.Foldable (toList)
import Data.Complex
import Numeric.FFT
import DSP.Window (hanning)


-- Sublists having heads at positions [1,1+noverlap..] of length framesz
split :: Int -> Int -> [a] -> [[a]]
split framesz hop = reverse . split_aux [] where 
  split_aux res l 
    | length newSeg < framesz = res
    | otherwise = split_aux (newSeg:res) (drop hop l)
    where  newSeg = take framesz l 

hannWin :: [Complex Double] ->  [Complex Double]
hannWin l = zipWith (*) hann l
  where hann = map (:+0) $ toList $ hanning $ length l + 1

-- STFT function, calculate X(u,f)
stft :: Int -> Int -> [Complex Double] -> [[Complex Double]]
stft framesz hop = map dft . map hannWin . split framesz hop


-- Overlap function, the list must be no empty
overlap :: Num a => Int -> [[a]] -> [a]
overlap hop = foldr1 overlap_aux where
  overlap_aux seg signal = seg1 ++ (zipWith (+) seg2 signal1) ++ signal2
    where (seg1, seg2) = splitAt hop seg
          noverlap = length seg2
          (signal1, signal2) = splitAt noverlap signal

-- ISTFT function, calculate x(t)
istft :: Int -> [[Complex Double]] -> [Complex Double]
istft hop = overlap hop . map idft
