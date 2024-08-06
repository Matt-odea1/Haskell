{- 

λ> myLast [1,2,3,4]
4
λ> myLast ['x','y','z']
'z'

-}

getLast :: [a] -> a 
getLast [] = error "empty"
getLast [x] = x
getLast (x:xs) = getLast xs