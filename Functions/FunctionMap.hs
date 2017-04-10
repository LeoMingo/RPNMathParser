module Functions.FunctionMap
(getFMKeys
,getFMVals
,infixFunctionMap
) where


--import Functions.Calculus

getFMKeys :: [(a,b)] -> [a]
getFMKeys fm = [fst pair | pair <- fm]

getFMVals :: [(a,b)] -> [b]
getFMVals fm = [snd pair | pair <- fm]

--Infix Function Map
--e.g. (snd $ infixFunctionMap !! 1) 1 2 --> -1.0
infixFunctionMap :: [(String, (Double -> Double -> Double))]
infixFunctionMap = 
    [ ("+", (+)),
      ("-", (-)),
      ("*", (*)),
      ("/", (/)),
      ("^", (**))
    ]

--Arbitrarily Argumented Function Map
arbArgFunctionMap :: [(String, ([Double]->Double))]
arbArgFunctionMap = 
    [ ("sum", (sum)),
    --("diffren", (diffren)),
    --("integ", (integ))
    ]





