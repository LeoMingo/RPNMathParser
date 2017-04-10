{-
 - Make x be the first phase test variable
 - -}

module Functions.Calculus.Diffrentiation
(
) where

import RPNCalc.Assets       (makeBlock)

(+') a b = unwords $ (diffren a) : (diffren b) : "+" : []
(-') a b = unwords $ (diffren a) : (diffren b) : "-" : []
(*') a b = unwords $ 
            (makeBlock $ unwords $ (diffren a) : (b) : "*" : []) 
            :
            (makeBlock $ unwords $ (diffren b) : (a) : "*" : [])) 
            : "+" : []
(/') a b = unwords $ 
            (makeBlock $ (unwords $
                (makeBlock $ unwords $ 
                    (diffren a) : b : "*" : []) 
                    :
                (makeBlock $ unwords $ 
                    (diffren b) : a : "*" : []) 
                : "-" : []))
            :
            (makeBlock $ unwords $ b : "2" : "^" : []) 
            : "/" : []

(^') u v = unwords $ 
            ( makeBlock $ unwords $ (makeBlock $ unwords $
                v : (makeBlock $ unwords $ u : (makeBlock $ unwords $ v : "1" : "-" : []) : "^" : []) : "*" : []) : (diffren u) : "*" : [] ) --left term
            : 
            makeBlock $ unwords $ ( (makeBlock $ unwords $ ( (makeBlock $ unwords $ u : v : "^" : []) : (makeBlock $ unwords $ u : "ln" : []) : "*" : [])) : (diffren v) : "*" : [] ) 
            : "+" : []



diffren :: String -> String
diffren str = 


