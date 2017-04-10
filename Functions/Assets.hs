module Functions.Assets
(makeBlock
,checkBlock
) where



makeBlock :: String -> String
makeBlock str = "(" ++ str ++ ")"

checkBlock :: String -> Bool
checkBlock str | (head str) == '(' && (last str) == ')' = True
               | otherwise = False





