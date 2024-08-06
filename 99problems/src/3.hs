{-

(*) Find the K'th element of a list.Solutions
 
The first element in the list is number 1. Example:

* (element-at '(a b c d e) 3)
c
Example in Haskell:

λ> elementAt [1,2,3] 2
2
λ> elementAt "haskell" 5
'e'

-}

findKth :: [x] -> Int -> x
findKth [] _ = error "INVALID"
findKth (x:xs) 1 = x
findKth (x:xs) y
    | y < 1 = error "INVALID"
    | otherwise = findKth xs (y-1)