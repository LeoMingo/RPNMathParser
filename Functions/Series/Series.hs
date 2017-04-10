module Functions.Series.Series
(
) where

import Functions.Combinatorics.CombFunc  (fact) 
import Functions.Algebra                (incSum)
import Data.Number.CReal

powOfe ::Float -> Float 
powOfe x = incSum f 0 33
    where f i = x**i / (fact i)

powOfa :: Float -> Float-> Float
powOfa a x = incSum f 0 33
    where f i = (x*log a)**i / (fact i)


lnOfx :: Float -> Float
lnOfx x = netSum f 1 33
    where f i = 2*(1/oddTerm)*((x-1)/(x+1))**oddTerm
            where oddTerm = 2*i-1
          netSum f minB maxB = sum $ reverse [f i | i <- [minB .. maxB]]

{-
 - Or another way around
lnOfx2 :: Float -> Float
lnOfx2 x = netSum f 1 33
    where f i = (1/i)*((x-1)/x)**i
          netSum f minB maxB = sum $ reverse [f i | i <- [minB .. maxB]]
-}      




