module Functions.InfixOpParser
(infixParser
) where


import Functions.InfixOperations
import Functions.FunctionMaps



--Infix Operation Block Check
infFMKeyList :: [String]
infFMKeyList = getFMKeys infixFunctionMap
infFMValList :: [ (Double->Double->Double) ]
infFMValList = getFMVals infixFunctionMap



--Parentheses are used for making in-process rpn blocks
infixParser :: String -> String -> Int -> String
infixParser a b fmIndex | infCCheck a b = 
                                show $ (infFMValList !! fmIndex)  aNum bNum 
                        | infVCheck a b = unwords $ a:b:(infFMKeyList !! fmIndex):[]
                                where aNum = read a
                                      bNum = read b
--                      | error "Incorrect Input\n"



