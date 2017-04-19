import Data.List    (elemIndices)



inParens :: String -> Bool
inParens str | (length str >= 2) && (head str == '(') && (last str == ')')
                        = True
             | otherwise = False

parenthesize :: String -> String
parenthesize str = "(" ++ str ++ ")"

unparenthesize :: String -> String
unparenthesize str | inParens str = init $ tail str
                   | otherwise = str

elem' :: (Foldable t, Eq a) => t a -> a -> Bool
elem' al a = elem a al

cArr = ['0' .. '9']
isC :: String -> Bool
isC "" = False
isC str | all (elem' cArr) str = True
        | otherwise = False
vArr = ['a' .. 'z']
isV :: String -> Bool
isV "" = False
isV str | all (elem' vArr) str = True
        | otherwise = False




subStr :: String -> Int -> Int -> String
subStr str begin len | len > (length str - begin) || len <= 0 = []
                     | len > 1 =  (str !! begin : []) ++ ((subStr str (begin+1) (len-1)))
                     | len == 1 = (str !! begin) : []
-- get substring by begining and ending indices
subStrI :: String -> Int -> Int -> String       
subStrI str begin end = subStr str begin (end - begin + 1)


makeTuple :: [a] -> [(a,a)]
makeTuple [] = []
makeTuple (x : []) = []
makeTuple (x1:x2:xs) = ((x1,x2) : []) ++ (makeTuple xs)

unTuple :: [(a,a)] -> [a]
unTuple [] = []
unTuple ((x1,y1):[]) = [x1, y2]
unTuple ((x1,y1):xs) = [x1, y1] ++ (unTuple xs)


allGreater :: Ord a => [a] -> [a] -> Bool
allGreater (x:[]) (y:[]) | x > y = True
                         | otherwise = False
allGreater (x:xs) (y:ys) | length xs /= length ys = False
                         | x > y = allGreater xs ys
                         | otherwise = False


sortedSet :: Ord a => [a] -> Bool
sortedSet [] = True
sortedSet (x:[]) = True
sortedSet (x1:xs) | x1 < (head xs) = sortedSet xs   -- Mathematically speaking, there are 
                  | otherwise = False               -- no 2 elements equal to each other in a set









{-
 - This function needs modifications for more cases e.g. fNT 2 11 [(1,6), (4,9)] which would 
 -1 ignore 6 4 and fill other spaces if snd $ prevTup < fst $ sucTup,
 -2 and it would be able to change tuple list's boundary like in this case (1,6) => (2,6)
 

 e.g. fNT 0 11 [(1,3),(6,9)] ==> [(0,0), (1,3), (4,5), (6,9), (10,11)]
-}

fillNumTuple :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
fillNumTuple begin end [] = fill begin end
                                where fill x1 x2 | x1 <= x2 = (x1,x2):[]
                                                 | otherwise = []
                    
fillNumTuple begin end (t : []) = (fill  begin ((fst t)-1)) 
                               ++ (t:[])
                               ++ (fill ((snd t)+1) end)
                                        where fill x1 x2 | x1 <= x2 = (x1,x2):[]
                                                         | otherwise = []
fillNumTuple begin end (t1 : t2 : []) = (fill  begin ((fst t1)-1))         ++ (t1:[])
                                     ++ (fill  ((snd t1)+1) ((fst t2)-1))  ++ (t2:[]) 
                                     ++ (fill  ((snd t2)+1) end)
                                            where fill x1 x2 | x1 <= x2 = (x1,x2):[]
                                                             | otherwise = []
fillNumTuple begin end (t1 : t2 : ts) = (fill  begin ((fst t1)-1))         ++ (t1:[])
                                     ++ (fill  ((snd t1)+1) ((fst t2)-1))  ++ (t2:[]) 
                                     ++ fillNumTuple ((snd t2)+1) end ts
                                            where fill x1 x2 | x1 <= x2 = (x1,x2):[]
                                                             | otherwise = []

