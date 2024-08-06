{-

(*) Find the last-but-one (or second-last) element of a list.Solutions
 
(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

λ> myButLast [1,2,3,4]
3
λ> myButLast ['a'..'z']
'y'

-}

secondLast :: [x] -> x
secondLast [] = error "no elements"
secondLast [x] = error "too short"
secondLast [x,_] = x
secondLast (_:xs) = secondLast xs