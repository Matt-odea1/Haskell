-- induction

-- try to prove:  map f (map g xs) = map (f . g) xs 

-- map :: (a -> b) -> [a] -> [b] 
-- map _ [] = [] 
-- map f (x:xs) = f x:map f xs 

-- TODO 


-------------------------------------------------------------------------------------

-- naturals

data Natural = Zero | Succ Natural

toInt :: Natural -> Int 
toInt Zero = 0
toInt (Succ n) = 1 + toInt n

instance Eq Natural where 
    Zero == Zero       = True
    Succ n == Succ m   = n == m
    _ == _             = False

instance Show Natural where
    show n = show (toInt n)