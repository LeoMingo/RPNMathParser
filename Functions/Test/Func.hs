module Functions.Test.Func
(testNum
)where 

import Data.Char
import Data.List


--jiToI :: Num a => Maybe a -> a
jiToI (Just i) = i


isMemberOf :: Eq a => [a] -> a -> Bool
isMemberOf xs x | emptyIndices $ elemIndices x xs = False
                | otherwise = True
                where emptyIndices xs | xs == [] = True
                                      | otherwise = False

numArray :: [Char]
numArray = map intToDigit [0..9]
fractionDot = '.'
checkArray = fractionDot : numArray

--So far only support non-negative real numbers
testNum :: String -> Bool
testNum testeeString |  any (not . isMemberOf checkArray) testeeString || 
                        badFormattedDecimal testeeString = False 
                     |  otherwise = True
                     where badFormattedDecimal xs | ((sort xs) !! 0 == fractionDot) =    
                                                    if (length xs < 3 || multiDots xs) 
                                                        then True
                                                    else if (jiToI $ elemIndex fractionDot xs) == (length xs) - 1 
                                                        then True
                                                    else 
                                                        False
                                                  | otherwise = False
                                                  where multiDots xs | (sort xs) !! 1 == fractionDot = True
                                                                     | otherwise = False
                                        
















