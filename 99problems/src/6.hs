{-

(*) Find out whether a list is a palindrome.Solutions
 
Hint: A palindrome can be read forward or backward; e.g. (x a m a x).

Example in Haskell:

λ> isPalindrome [1,2,3]
False
λ> isPalindrome "madamimadam"
True
λ> isPalindrome [1,2,4,8,16,8,4,2,1]
True

-}

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome list
    | head list == last list = isPalindrome (init (tail list))
    | otherwise = False