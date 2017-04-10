import Data.List
import Data.Char
import System.IO


import RPNCalc.InfixOperation       (infFMKeyList, infFMValList, infixParser)


rpnCalc :: String -> String
rpnCalc = head . foldl foldingFunc [] . words
    where   foldingFunc (x1:x2:xs) str | elem str infFMKeyList = 
                                                let j (Just i) = i
                                                    fmIndex = (j (elemIndex str infFMKeyList))
                                                in (infixParser x2 x1 fmIndex) : xs
            foldingFunc (x1:x2:xs) ">" = (show (read x2 / read x1)) : xs
            foldingFunc xs string = string : xs   --Causes the reverse calculating order above              


