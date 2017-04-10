import Data.List
import Data.Char
import System.IO

import Functions.Test.Func          (testNum)
import Functions.FunctionMap        

isConstant :: String -> Bool
isConstant a | testNum a = True
             | otherwise = False

varRange = ['A'..'Z'] ++ ['a' .. 'z']

isVariable :: String -> Bool
isVariable a | elem (a !! 0) varRange = True
             | otherwise = False

--Infix Operation Constant Check
infCCheck :: String -> String -> Bool
infCCheck a b | isConstant a && isConstant b = True
                                | otherwise = False

--Infix Operation Variable Check 
--examples like: 5 x1 * , a b -
infVCheck :: String -> String -> Bool
infVCheck a b | isVariable a || isVariable b = True
              | otherwise = False

infFMKeyList :: [String]
infFMKeyList = getFMKeys infixFunctionMap
infFMValList :: [ (Double->Double->Double) ]
infFMValList = getFMVals infixFunctionMap

--Parentheses are used for making in-process rpn blocks
infixParser :: String -> String -> Int -> String
infixParser a b fmIndex | infCCheck a b = 
                                "(" ++ (show $ (infFMValList !! fmIndex)) ++ ")" aNum bNum 
                        | infVCheck a b = "(" ++ (unwords $ a:b:(infFMKeyList !! fmIndex):[] ++ ")" 
                                where aNum = read a
                                      bNum = read b
--                      | error "Incorrect Input\n"



rpnCalc :: String -> String
rpnCalc = head . foldl foldingFunc [] . words
    where   foldingFunc (x1:x2:xs) str | elem str infFMKeyList = 
                                                let j (Just i) = i
                                                    fmIndex = (j (elemIndex str infFMKeyList))
                                                in (infixParser x2 x1 fmIndex) : xs
            foldingFunc (x1:x2:xs) ">" = (show (read x2 / read x1)) : xs
            foldingFunc xs string = string : xs   --Causes the reverse calculating order above              


