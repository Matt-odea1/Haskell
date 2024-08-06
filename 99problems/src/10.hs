{-

(*) Run-length encoding of a list.Solutions
 
Use the result of Problem 9 to implement the so-called run-length encoding data compression method. 
Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example:

* (encode '(a a a a b c c a a d e e e e))
((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
Example in Haskell:

λ> encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

-}

pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest
pack [] = []

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)
