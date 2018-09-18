{-
 - Euclid's algorithm to find gcd
 -}
gcd :: Int -> Int -> Int
gcd a b
    | b == 0 = a
    | otherwise = gcd b (a `mod` b)

{-
 - Fibonacci sequence with helper function
 -}
fib :: Int -> Int
fib x = fibHelper x 0 1
  where fibHelper y a b
        | y == 0 = a -- previous value
        | y == 1 = b -- next value
        | otherwise = fibHelper (y-1) b (a+b)
