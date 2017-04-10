module Functions.InfixOperations
(isConstant
,isVariable
,infCCheck
,infVCheck

,(.+)
,(.-)
,(.*)
,(./)
,(.**)
) where


import Functions.Test.Func          (testNum)
import Functions.Assets             (makeBlock, checkBlock)


isConstant :: String -> Bool
isConstant a | testNum a = True
             | otherwise = False

--Infix Constant Operation Check
infCCheck :: String -> String -> Bool
infCCheck a b | isConstant a && isConstant b = True
              | otherwise = False


varRange = ['A'..'Z'] ++ ['a' .. 'z']

isVariable :: String -> Bool
isVariable a | elem (a !! 0) varRange = True
             | otherwise = False

isBlock :: String -> Bool
isBlock = checkBlock 


--Infix Variable Operation Check 
--examples like: 5 x1 * , a b -
infVCheck :: String -> String -> Bool
infVCheck a b | isVariable a && isVariable b = True
              | isVariable a && (isConstant b || isBlock b) = True
              | isVariable b && (isConstant a || isBlock a) = True
              | otherwise = False



(.+) :: String -> String -> String
(.+) a b | infCCheck a b = makeBlock $ show $ (aNum + bNum)
         | infVCheck a b = makeBlock $ unwords $ aB : bB : "(+)" : []
                    where aNum = read a
                          bNum = read b
                          aB | isBlock a = a
                             | otherwise = makeBlock a
                          bB | isBlock b = b
                             | otherwise = makeBlock b


(.-) :: String -> String -> String
(.-) a b | infCCheck a b = makeBlock $ show $ (aNum - bNum)
         | infVCheck a b = makeBlock $ unwords $ aB : bB : "(-)" : []
                    where aNum = read a
                          bNum = read b
                          aB | isBlock a = a
                             | otherwise = makeBlock a
                          bB | isBlock b = b
                             | otherwise = makeBlock b

(.*) :: String -> String -> String
(.*) a b | infCCheck a b = makeBlock $ show $ (aNum * bNum)
         | infVCheck a b = makeBlock $ unwords $ aB : bB : "(*)" : []
                    where aNum = read a
                          bNum = read b
                          aB | isBlock a = a
                             | otherwise = makeBlock a
                          bB | isBlock b = b
                             | otherwise = makeBlock b

(./) :: String -> String -> String
(./) a b | infCCheck a b = makeBlock $ show $ (aNum / bNum)
         | infVCheck a b = makeBlock $ unwords $ aB : bB : "(/)" : []
                    where aNum = read a
                          bNum = read b
                          aB | isBlock a = a
                             | otherwise = makeBlock a
                          bB | isBlock b = b
                             | otherwise = makeBlock b

(.**) :: String -> String -> String
(.**) a b | infCCheck a b = makeBlock $ show $ (aNum ** bNum)
         | infVCheck a b = makeBlock $ unwords $ aB : bB : "(^)" : []
                    where aNum = read a
                          bNum = read b
                          aB | isBlock a = a
                             | otherwise = makeBlock a
                          bB | isBlock b = b
                             | otherwise = makeBlock b

