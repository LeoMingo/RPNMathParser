module Functions.Combinatorics.CombFunc 
(fact
,comb
,perm
) where


import Functions.Algebra    (incSum)


--Single Factorial
fact :: Float -> Float
fact 0 = 1
fact n = n * (fact $ n - 1)


comb :: Float -> Float -> Float
comb n m | m > n = 0
         | otherwise = perm n m / fact m

perm :: Float -> Float -> Float
perm n m | m > n = 0
         | otherwise = (fact n) / fact (n - m)


