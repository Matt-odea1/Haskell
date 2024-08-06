{-

(**) Pack consecutive duplicates of list elements into sublists.Solutions
 
If a list contains repeated elements they should be placed in separate sublists.

Example:

* (pack '(a a a a b c c a a d e e e e))
((A A A A) (B) (C C) (A A) (D) (E E E E))
Example in Haskell:

λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]

-}

pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest
pack [] = []