{-# LANGUAGE PackageImports #-}
module Main (main) where

import "base" Data.Maybe (isJust)
import "parallel" Control.Parallel.Strategies (using, parBuffer, rdeepseq, Strategy)

rsa2048 = read "\
\2519590847565789349402718324004839857142928212620403202777713783604366202070\
\7595556264018525880784406918290641249515082189298559149176184502808489120072\
\8449926873928072877767359714183472702618963750149718246911650776133798590957\
\0009733045974880842840179742910064245869181719511874612151517265463228221686\
\9987549182422433637259085141865462043576798423387184774447920739934236584823\
\8242811981638150106748104516603773060562016196762561338441436038339044149526\
\3443219011465754445417842402092461651572335077870774981712577246796292638635\
\6373289912154831438167899885040445364023527381951378636564391212010397122822\
\120720357"

divable :: Integer -> Integer -> Maybe (Integer, Integer)
divable large small
    | large < small = Just (large, 1)
    | remainder == 0 = Just (small, quotient)
    | otherwise = Nothing
    where (quotient, remainder) = large `divMod` small

getFactorFrom :: Integer -> Integer -> [Integer]
getFactorFrom from largeNumber
    | largeNumber < 2 = []
    | otherwise = nextFrom : getFactorFrom nextFrom nextLarge
    where
        strat = parBuffer 2048 rdeepseq
        worker = map (divable largeNumber) [from..] `using` strat
        Just (nextFrom, nextLarge) = head . filter isJust $ worker

getFactors = getFactorFrom 2

main = print $ getFactors rsa2048
