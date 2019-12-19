fat :: Int -> Int
fat 0 = 1
fat n = fat(n-1) * n

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = (a == b) && (c == d) && (a == c)

equalCount :: Int -> Int -> Int -> Int
equalCount a b c
 | (a == b) && (b == c) = 3
 | (a == b) || (a == c) || (b == c) = 2
 | otherwise = 0


mediaVendas :: Int -> Int -> Int -> Float
mediaVendas a b c = fromIntegral(a+b+c)/3