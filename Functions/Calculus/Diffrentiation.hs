{-
 - Make x be the first phase test variable
 - Make diffrentiaton infix operators be with '|' at its right such as +| -| /| *| ^| etc.
 - It will be '|' at the left side for integration
 - -}

module Functions.Calculus.Diffrentiation
(             diffren
) where


import Functions.InfixOperations


(+|) a b = (diffren a) .+ (diffren b)


(-|) a b = (diffren a) .- (diffren b)


(*|) a b = (.+) 
            ((.*) (diffren a) b) 
            ((.*) (diffren b) a)   --Haven't set precedence yet so lisp way may seem clearer



(/|) a b = (./) 
            ((.-) 
                ((.*) (diffren a) b) 
                ((.*) (diffren b) a)) 
            (b .** 2)

--Leibneiz rule for diffrentiation
(**|) u v = (.+)
            ((.*)
                (v .* (u.**(v.-1)))
                (diffren u))
            ((.*)
                (u.**v) .* (ln u)    --ln will be defined in prefixFunctionMap
                (diffren v))

diffren :: String -> String
diffren str = 


