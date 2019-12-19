double :: [Int] -> [Int]
double [] = []
double (x:xs) = (2*x): double xs 

member :: [Int] -> Int -> Bool
member [] a = False
member (x:xs) a
 |x == a = True
 |otherwise = member xs a

digits :: String -> String
digits [] = []
digits(x:xs)
 | x >= '0' && x <= '9' = x:digits xs
 | otherwise = digits xs

sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = []
sumPairs((x,y):xs) = (x+y):sumPairs xs 

mytake :: [Int] -> Int -> [Int]
mytake xs 0 = []
mytake [] n = []
mytake (x:xs) n = x:(mytake xs (n-1))

mydrop :: [Int] -> Int -> [Int]
mydrop xs 0 = xs
mydrop [] n = []
mydrop (x:xs) n = mydrop xs (n-1)

mytakewhile f (x:xs)
 | f x = x:(mytakewhile f xs)
 | otherwise = mytakewhile f xs
 
