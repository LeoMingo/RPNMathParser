module Functions.Algebra
(incSum
) where

incSum :: (Float -> Float) -> Float -> Float -> Float
incSum f sumMinBound sumMaxBound = sum [f i | i <- [sumMinBound .. sumMaxBound]]



