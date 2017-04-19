module Functions.FunctionMaps
(getFMKeys
,getFMVals
,infixFunctionMap
,isFMKeys
) where


--import Functions.Calculus
import Functions.InfixOperations
       ((.+), (.-), (.*), (./), (.**))



getFMKeys :: [(a,b)] -> [a]
getFMKeys fm = [fst pair | pair <- fm]

getFMVals :: [(a,b)] -> [b]
getFMVals fm = [snd pair | pair <- fm]

isFMKeys :: String -> [(a,b)] -> Bool
isFMKeys str fm = elem str $ getFMKeys fm




--Infix Function Map (Functions taking two arguments)
--e.g. (snd $ infixFunctionMap !! 1) 1 2 --> -1.0
infixFunctionMap :: [(String, (String -> String -> String))]
infixFunctionMap = 
    [ ("+", (.+)),
      ("-", (.-)),
      ("*", (.*)),
      ("/", (./)),
      ("^", (.**)),

      
      
      --Extend the map with functions taking two parameters such as combination and permutation
    ]






--Infix Diffrentiation Function Map (Functions taking two arguments)
infixDFunctionMap :: [(String, (String -> String -> String))]
infixDFunctionMap = 
    [ ("+", (+|)),
      ("-", (-|)),
      ("*", (*|)),
      ("/", (/|)),
      ("^", (**|)),
          --Infix operators nested in diffrentiations
    ]






--Prefix Function Map (Functions taking 1 argument)
prefixFunctionMap :: [(String, (String -> String))]
prefixFunctionMap = 
    [ --("ln", (rpnln))
    ]





--Arbitrarily Argumented Function Map (Functions taking arbitrary amount of arguments)
arbArgFunctionMap :: [(String, ([Double]->Double))]
arbArgFunctionMap = 
    [ ("sum", (sum))
    --("diffren", (diffren)),
    --("integ", (integ))
    ]





