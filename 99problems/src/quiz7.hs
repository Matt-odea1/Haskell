import Control.Monad.State
matching :: String -> Bool
matching xs = fst (runState (go xs) 0)
  where
    go [] = get >>= return . (== 0)
    go (x:xs) | x == '('  = modify (+1) >>= \_ -> go xs
              | x == ')'  = get >>= \n ->
                               if n > 0 then put (n - 1) >>= \_ -> go xs
                                        else return False
              | otherwise = go xs