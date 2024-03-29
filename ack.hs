{-# LANGUAGE BangPatterns #-}

ack :: Int -> Int -> Int
ack !m !n
 | m == 0 = n + 1
 | n == 0 = ack (m-1) 1
 | otherwise = ack (m-1) (ack m (n-1))
               
main = print $ ack 3 10