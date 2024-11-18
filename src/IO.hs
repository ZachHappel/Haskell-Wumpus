-- take a list of displayable then return one that user selection

getFromUser :: Show a => String -> [a] -> a
getFromUser = undefined


getFromUser "Select the move:" [Left, Back]

{-
Select the move:
1 Left
2 Back
Left
invalid choice
Select the move:
1 Left
2 Back
-}
